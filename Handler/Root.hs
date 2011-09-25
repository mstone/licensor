{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import Text.Hamlet (hamlet)
import Control.Applicative ((<$>))
import qualified Text.Blaze.Renderer.String as Blaze
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

storeEntry :: Tar.Entry -> Handler ()
storeEntry e = do
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
      runDB $ insert $ Datum path tag
      return ()
    _ -> return ()

store :: Tar.Entries -> Handler ()
store es = do
  case es of
    Tar.Next e es' -> storeEntry e >> store es'
    Tar.Done -> return ()
    Tar.Fail msg -> fail ("import failed" ++ msg)

postImportR :: Handler RepHtml
postImportR = do
    -- XXX: inspect user data!
    (_, files) <- runRequestBody
    file <- maybe (redirect RedirectSeeOther ImportR)
                  (return . fileContent)
                  (lookup "file" files)
    let es = Tar.read $ GZip.decompress file
    store es
    let msg = Just "Project imported"
    defaultLayout $ do
        setTitle $ "import project"
        addWidget $(widgetFile "import")

getProjectR :: Project -> Handler RepHtml
getProjectR project = do
    if project == ""
      then getProjectsR
      else defaultLayout $ do
        setTitle $ text $ T.append "project " project
        addWidget $(widgetFile "project")

getProjectsR :: Handler RepHtml
getProjectsR = do
    defaultLayout $ do
        setTitle $ text $ "project"
        let project :: Project
            project = ""
        addWidget $(widgetFile "project")

getFileR :: DatumId -> Handler RepHtml
getFileR did = do
    mdat <- runDB $ get did
    defaultLayout $ do
        setTitle $ "get file"
        addWidget $(widgetFile "file")

postFileR :: DatumId -> Handler RepHtml
postFileR did = do
    mdat <- runDB $ get did
    defaultLayout $ do
        setTitle $ "post file"
        addWidget $(widgetFile "file")

getFilesR :: Handler RepHtml
getFilesR = do
    files <- (runDB $ selectList [] []) :: Handler [(DatumId, Datum)]
    defaultLayout $ do
        setTitle $ text $ "files"
        addWidget $(widgetFile "files")

getExportR :: Project -> Handler RepHtml
getExportR project = do
    defaultLayout $ do
        setTitle $ text $ T.append "export project " project
        addWidget $(widgetFile "export")

