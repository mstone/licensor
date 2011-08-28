{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import qualified Data.Text as T
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
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "licensor homepage"
        addWidget $(widgetFile "homepage")


getImportR :: Handler RepHtml
getImportR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle $ "import project"
        addWidget $(widgetFile "import")

postImportR :: Handler RepHtml
postImportR = do
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

