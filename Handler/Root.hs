{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
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
        h2id <- lift newIdent
        setTitle "licensor homepage"
        addWidget $(widgetFile "homepage")


getImportR :: Handler RepHtml
getImportR = do
    let msg = Nothing :: Maybe LBS.ByteString
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ "import project"
        addWidget $(widgetFile "import")

storeEntry :: Tar.Entry -> IO ()
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
      Dir.createDirectoryIfMissing True dp
      LBS.writeFile fp $ GZip.compress txt
    _ -> return ()

store :: Tar.Entries -> IO ()
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
    liftIO $ store es
    let msg = Just "Project imported"
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ "import project"
        addWidget $(widgetFile "import")

getProjectR :: Project -> Handler RepHtml
getProjectR project = do
    if project == ""
      then getProjectsR
      else defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ text $ T.append "project " project
        addWidget $(widgetFile "project")

getProjectsR :: Handler RepHtml
getProjectsR = do
    defaultLayout $ do
        setTitle $ text $ "project"
        let project :: Project
            project = ""
        addWidget $(widgetFile "project")

getFileR :: File -> Handler RepHtml
getFileR file = do
    if file == ""
      then getFilesR
      else defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ text $ T.append "get file " file
        addWidget $(widgetFile "file")

postFileR :: File -> Handler RepHtml
postFileR file = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ text $ T.append "post file " file
        addWidget $(widgetFile "file")

getFilesR :: Handler RepHtml
getFilesR = do
    defaultLayout $ do
        setTitle $ text $ "files"
        let file :: File
            file = ""
        addWidget $(widgetFile "file")

getExportR :: Project -> Handler RepHtml
getExportR project = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ text $ T.append "export project " project
        addWidget $(widgetFile "export")

