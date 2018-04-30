{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified GitHub.Auth   as Github
import qualified GitHub.Endpoints.Issues as Github
import qualified GitHub.Endpoints.Issues.Comments as Github
import qualified GitHub.Data.Name as Github
import qualified GitHub.Data.Id as Github
import qualified Data.Vector             as Vector (fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.IO as T
import Data.Either
import Control.Monad
import qualified Data.Aeson as Aeson
import Control.Concurrent

import Types

org = "HaskellCNOrg"
repo = "haskellcn"
--org = "haskellcn"
--repo="TEST"

main :: IO ()
main = do
  SiteData{..} <- parseData
  let auth = Github.BasicAuth "github username" "github password or token"
  mapM_ (migTopic auth users replies) topics

migTopic :: Github.Auth -> [User] -> [Reply] -> Topic -> IO ()
migTopic auth users replies Topic{..} = do
  print $ "Migrate topic: " `T.append` topicId
  let user = filter (\u -> userId u == topicUserId) users
  let userBy = case user of 
                 [] -> ""
                 [usera] -> " by " `T.append` userName usera
  let newiss = (Github.newIssue (topicTitle `T.append` userBy))
              { Github.newIssueBody = Just topicContent
              , Github.newIssueLabels = Just $ Vector.fromList ["migration"]
              }
  possibleIssue <- Github.createIssue auth org repo newiss
  threadDelay 2000000
  case possibleIssue of 
    Left e -> (print ("Failed for topic" `T.append` topicId) >> print e)
    Right issue -> do 
      let rs = filter (\r -> replyTopicId r == topicId) replies
      let migReply reply = print ("Migrate comment for topic " `T.append` replyTopicId reply)
                          >> threadDelay 2000000
                          >> Github.createComment auth org repo (Github.Id $ Github.issueNumber issue) (replyContent reply)
      re <- mapM migReply rs
      print $ filter isLeft re
                  
toNameLabel :: T.Text -> Github.Name Github.IssueLabel
toNameLabel = Github.N

parseData :: IO SiteData
parseData = do
  userFile <- T.readFile "./2018-01-16/nc-users.json"
  --tagFile <- T.readFile "./2018-01-16/nc-tags.json"
  topicFile <- T.readFile "./2018-01-16/nc-topics.json"
  replyFile <- T.readFile "./2018-01-16/nc-replies.json"
  let users = parseJson userFile :: [Either String User]
  --let tags = parseJson tagFile :: [Either String Tag]
  let topics = parseJson topicFile :: [Either String Topic]
  let replies = parseJson replyFile :: [Either String Reply]
  when (hasError users) (error "cannot parse user file")
  --when (hasError tags) (error "cannot parse tag file")
  when (hasError topics) (print topics >> error "cannot parse topic file")
  when (hasError replies) (error "cannot parse reply file")
  return $ SiteData (rights users) (rights topics) (rights replies)
  
hasError = not . null . filter isLeft

parseJson :: Aeson.FromJSON a => T.Text -> [Either String a]
parseJson = map (Aeson.eitherDecode . TL.encodeUtf8 . TL.fromStrict) . T.lines