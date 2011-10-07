{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Handler.Root where

import Yesod
import Foundation
import Text.Hamlet (hamlet, shamlet)
import Control.Applicative
import qualified Text.Blaze.Renderer.String as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as BlazeText
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Crypto.Hash.SHA1 as Sha1
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified Data.Hex as Hex
import System.FilePath ((</>))
import Text.Blaze

import Database.Persist.Join (SelectOneMany (..), selectOneMany)
import Database.Persist.Join.Sql (runJoin)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        setTitle "licensor homepage"
        addWidget $(widgetFile "homepage")


getImportR :: Handler RepHtml
getImportR = do
    let msg = Nothing :: Maybe LBS.ByteString
    defaultLayout $ do
        setTitle $ "import project"
        addWidget $(widgetFile "import")

storeEntry :: ArchiveId -> Tar.Entry -> Handler ()
storeEntry ai e = do
  case Tar.entryContent e of
    Tar.NormalFile txt len -> do
      let tag = Sha1.hashlazy txt
      let (a, tag') = BS.splitAt 1 tag
      let (b, tag'') = BS.splitAt 1 tag'
      let fmt = BS.unpack . Hex.hex
      let (a', b', tag''') = (fmt a, fmt b, fmt tag'')
      let dp = "state" </> a' </> b'
      let fp = dp </> tag'''
      liftIO $ Dir.createDirectoryIfMissing True dp
      liftIO $ LBS.writeFile fp $ GZip.compress txt
      let path = T.pack $ Tar.entryPath e
      runDB $ insert $ Datum ai path tag
      return ()
    _ -> return ()

store :: ArchiveId -> Tar.Entries -> Handler ()
store ai es = do
  case es of
    Tar.Next e es' -> storeEntry ai e >> store ai es'
    Tar.Done -> return ()
    Tar.Fail msg -> fail ("import failed" ++ msg)

postImportR :: Handler RepHtml
postImportR = do
    -- XXX: inspect user data!
    (_, files) <- runRequestBody
    file <- maybe (redirect RedirectSeeOther ImportR)
                  (return . fileContent)
                  (lookup "file" files)
    let atag = Sha1.hashlazy file
    let es = Tar.read $ GZip.decompress file
    ai <- runDB $ insert $ Archive atag
    store ai es
    let msg = Just "Archive imported"
    defaultLayout $ do
        setTitle $ "import archive"
        addWidget $(widgetFile "import")

getArchiveR :: ArchiveId -> Handler RepHtml
getArchiveR aid = do
    mar <- runDB $ get aid
    case mar of
      Nothing -> do
        getArchiveSetR
      Just ar -> do
        let atag = E.decodeUtf8 $ Hex.hex $ archiveHash ar
        dats <- (runDB $ selectList [DatumArchiveId ==. aid] []) :: Handler [(DatumId, Datum)]
        defaultLayout $ do
          setTitle $ text $ T.append "archive " atag
          addWidget $(widgetFile "archive")

getArchiveDep5R :: ArchiveId -> Handler RepPlain
getArchiveDep5R aid = do
    mar <- runDB $ get aid
    case mar of
      Nothing -> do
        return $ RepPlain $ "No dep5. :("
      Just ar -> do
        dats <- (runDB $ selectList [DatumArchiveId ==. aid] []) :: Handler [(DatumId, Datum)]
        as <- runDB $ runJoin
            (selectOneMany (AssertionDatumId <-.) assertionDatumId) {
                somFilterOne = [ DatumArchiveId ==. aid ]
              , somIncludeNoMatch = True
              }
        return $ RepPlain $ toContent $ T.concat (map go as)
  where
    toLL :: (AssertionId, Assertion) -> T.Text
    toLL (_, a) = T.concat ["License: ", maybe T.empty id (assertionLicense a), "\n"]
    toFile :: Datum -> T.Text
    toFile dat = T.concat ["Files: ", datumPath dat, "\n"]
    go :: ((DatumId, Datum), [(AssertionId, Assertion)]) -> T.Text
    go ((did, dat), [])     = T.concat [toFile dat, "Copyright: \n", "License: \n\n"]
    go ((did, dat), as)     = T.concat ([toFile dat, "Copyright: \n"] ++ map toLL as ++ ["\n\n"])

getArchiveSetR :: Handler RepHtml
getArchiveSetR = do
    ars <- (runDB $ selectList [] []) :: Handler [(ArchiveId, Archive)]
    defaultLayout $ do
        setTitle $ text $ "archives"
        addWidget $(widgetFile "archiveset")

getFileR :: DatumId -> Handler RepHtml
getFileR did = do
    mdat <- runDB $ get did
    case mdat of
      Nothing -> do
        defaultLayout $ do
            setTitle $ "get file"
            toWidget [shamlet|<h1>No file. :(|]
      Just dat -> do
        let path = datumPath dat
        let tag = datumHash dat
        let (a, tag') = BS.splitAt 1 tag
        let (b, tag'') = BS.splitAt 1 tag'
        let fmt = BS.unpack . Hex.hex
        let (a', b', tag''') = (fmt a, fmt b, fmt tag'')
        let dp = "state" </> a' </> b'
        let fp = dp </> tag'''
        ctxt <- liftIO $ LBS.readFile fp
        let txt = E.decodeUtf8 $ BS.concat $ LBS.toChunks $ GZip.decompress ctxt
        assertions <- (runDB $ selectList  [AssertionDatumId ==. did] []) :: Handler [(AssertionId, Assertion)]
        mPrevDid   <- (runDB $ selectFirst [DatumId <. did] [Desc DatumId]) :: Handler (Maybe (DatumId, Datum))
        mNextDid   <- (runDB $ selectFirst [DatumId >. did] [Asc DatumId]) :: Handler (Maybe (DatumId, Datum))
        defaultLayout $ do
            setTitle $ "get file"
            addWidget $(widgetFile "file")

postFileR :: DatumId -> Handler RepHtml
postFileR did = do
    mdat <- runDB $ get did
    case mdat of
      Nothing -> do
        defaultLayout $ do
            setTitle $ "post file"
            toWidget [shamlet|<h1>No file. :(|]
      Just dat -> do
        lic <- runInputPost $ ireq textField "lic"
        runDB $ insert $ Assertion did $ Just lic
        redirect RedirectSeeOther $ FileR did

getFileSetR :: Handler RepHtml
getFileSetR = do
    files <- (runDB $ selectList [] []) :: Handler [(DatumId, Datum)]
    defaultLayout $ do
        setTitle $ text $ "fileset"
        addWidget $(widgetFile "fileset")

getExportR :: Project -> Handler RepHtml
getExportR project = do
    defaultLayout $ do
        setTitle $ text $ T.append "export project " project
        addWidget $(widgetFile "export")

