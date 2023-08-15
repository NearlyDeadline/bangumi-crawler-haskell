--
 -- @Date: 2023-07-17 15:17:44
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-07-26 14:55:17
 -- @FilePath: /bangumi-crawler-haskell/src/Type/Main.hs
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
module Type.Main where

import Data.Aeson
import ClassyPrelude
import GHC.Generics (Generic)
import Type.Common

data Bangumi = Bangumi
    { bangumiId :: Int
    , bangumiUrl :: Text
    , bangumiType :: Int
    , bangumiName :: Text
    , bangumiName_cn :: Text
    , bangumiEps :: Int
    , bangumiRating :: Rating
    , bangumiRank :: Int
    , bangumiCollection :: Collection
    , bangumiCrt :: [Character]
    , bangumiStaff :: [Staff]
    } deriving (Show, Generic)

instance FromJSON Bangumi where
    parseJSON = withObject "Bangumi" $ \v -> do 
        bangumiId <- v .: "id"
        bangumiUrl <- v .: "url"
        bangumiType <- v .: "type"
        bangumiName <- v .: "name"
        bangumiName_cn <- v .: "name_cn"
        bangumiEps <- v .: "eps"
        bangumiRating <- v .: "rating"
        bangumiRank <- v .: "rank"
        bangumiCollection <- v .: "collection"
        bangumiCrt <- fromMaybe [] <$> v .: "crt"
        bangumiStaff <- fromMaybe [] <$> v .: "staff"
        return $ Bangumi 
            { bangumiId = bangumiId
            , bangumiUrl = bangumiUrl
            , bangumiType = bangumiType
            , bangumiName = bangumiName
            , bangumiName_cn = bangumiName_cn
            , bangumiEps = bangumiEps
            , bangumiRating = bangumiRating
            , bangumiRank = bangumiRank 
            , bangumiCollection = bangumiCollection
            , bangumiCrt = bangumiCrt
            , bangumiStaff = bangumiStaff
            }
instance ToJSON Bangumi where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "bangumi" }

data Rating = Rating
    { ratingTotal :: Int
    , ratingCount :: RatingCount
    , ratingScore :: Double
    } deriving (Show, Generic)

instance FromJSON Rating where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "rating" }
instance ToJSON Rating where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "rating" }

data RatingCount = RatingCount
    { ratingcount10 :: Int
    , ratingcount9  :: Int
    , ratingcount8  :: Int
    , ratingcount7  :: Int
    , ratingcount6  :: Int
    , ratingcount5  :: Int
    , ratingcount4  :: Int
    , ratingcount3  :: Int
    , ratingcount2  :: Int
    , ratingcount1  :: Int
    } deriving (Show, Generic)

instance FromJSON RatingCount where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "ratingcount" }
instance ToJSON RatingCount where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "ratingcount" }

data Collection = Collection
    { collectionWish :: Int
    , collectionCollect :: Int
    , collectionDoing :: Int
    , collectionOn_hold :: Int
    , collectionDropped :: Int
    } deriving (Show, Generic)

instance FromJSON Collection where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "collection" }
instance ToJSON Collection where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "collection" }

data Character = Character
    { characterId :: Int
    , characterUrl :: Text
    , characterName :: Text
    , characterName_cn :: Text
    , characterRole_name :: Text
    , characterActors :: [Actor]
    } deriving (Show, Generic)

instance FromJSON Character where
    parseJSON = withObject "Character" $ \v -> do
        characterId <- v .: "id"
        characterUrl <- v .: "url"
        characterName <- v .: "name"
        characterName_cn <- v .: "name_cn"
        characterRole_name <- v .: "role_name"
        characterActors <- fromMaybe [] <$> v .: "actors"
        return $ Character 
            { characterId = characterId
            , characterUrl = characterUrl
            , characterName = characterName
            , characterName_cn = characterName_cn
            , characterRole_name = characterRole_name
            , characterActors = characterActors
            }

instance ToJSON Character where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "character" }

data Actor = Actor
    { actorId :: Int
    , actorUrl :: Text
    , actorName :: Text
    } deriving (Show, Generic)

instance FromJSON Actor where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "actor" }
instance ToJSON Actor where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "actor" }

data Staff = Staff
    { staffId :: Int
    , staffUrl :: Text
    , staffName :: Text
    , staffName_cn :: Text
    , staffJobs :: [Text]
    } deriving (Show, Generic)

instance FromJSON Staff where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "staff" }
instance ToJSON Staff where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = fieldLabelModifierGeneric "staff" }

