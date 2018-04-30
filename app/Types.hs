{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text
import Data.Aeson

data SiteData = SiteData { users :: [User]
                        , topics :: [Topic]
                        , replies :: [Reply]
                        } deriving (Show)
data User = User
  { userId :: Text
  , userName :: Text
  } deriving (Show)

data Tag = Tag
  { tagId :: Text
  , tagName :: Text
  } deriving (Show)

data Topic = Topic
  { topicId :: Text
  , topicUserId :: Text
  , topicContent :: Text
  , topicTitle :: Text
  } deriving (Show)

data Reply = Reply
  { replyUserId :: Text
  , replyTopicId :: Text
  , replyContent :: Text
  } deriving (Show)


oidParser v = v .: "_id" >>= (.: "$oid")
authoidParser v = v .: "author_id" >>= (.: "$oid")
topicIdParser v = v .: "topic_id" >>= (.: "$oid")

instance FromJSON User where
  parseJSON (Object v) = User
                         <$> oidParser v
                         <*> v .: "display_name"

instance FromJSON Tag where
  parseJSON (Object v) = Tag
                        <$> oidParser v
                        <*> v .: "name"

instance FromJSON Topic where
  parseJSON (Object v) = Topic 
                      <$> oidParser v
                      <*> authoidParser v                          
                      <*> v .: "content"                          
                      <*> v .: "title"                          

instance FromJSON Reply where
  parseJSON (Object v) = Reply 
                        <$> authoidParser v      
                        <*> topicIdParser v
                        <*> v .: "content"  