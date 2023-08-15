--
 -- @Date: 2023-07-17 13:05:38
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-08-03 13:07:41
 -- @FilePath: /bangumi-crawler-haskell/src/PostgreSQL/Common.hs
--
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}


module PostgreSQL.Common where

import           ClassyPrelude
import           Control.Monad.Logger
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Type.Main as T
import           HTTP.Common (findBangumiLabelById)



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Bangumi 
    url Text
    name Text
    name_cn Text 
    type Int 
    eps Int 
    rank Int 
    ratingTotal Int 
    ratingScore Double
    wish Int 
    collect Int 
    doing Int 
    on_hold Int 
    dropped Int
    rating1 Int 
    rating2 Int 
    rating3 Int 
    rating4 Int 
    rating5 Int 
    rating6 Int 
    rating7 Int 
    rating8 Int 
    rating9 Int 
    rating10 Int 
    UniqueBangumiUrl url
    deriving Show
Character 
    url Text
    name Text 
    name_cn Text 
    UniqueCharacterUrl url
    deriving Show
Actor 
    url Text
    name Text 
    UniqueActorUrl url
    deriving Show
Staff 
    url Text
    name Text 
    name_cn Text
    UniqueStaffUrl url 
    deriving Show 
Label
    name Text
    UniqueLabelName name
    deriving Show 
BangumiLabel 
    bangumiId BangumiId 
    labelId LabelId 
    labelCount Int 
    UniqueBangumiLabel bangumiId labelId
    deriving Show
BangumiCharacterActor
    bangumiId BangumiId 
    characterId CharacterId
    actorId ActorId
    role_name Text
    UniqueBangumiCharacterActor bangumiId characterId actorId
    deriving Show
BangumiStaff 
    bangumiId BangumiId 
    staffId StaffId 
    staffJob Text
    UniqueBangumiStaff bangumiId staffId
    deriving Show
|]
connStr :: ConnectionString
connStr = "host=localhost dbname=bangumi user=mike password=mike port=5432"

migrate :: IO ()
migrate = runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll


insertBangumi :: T.Bangumi -> IO ()
insertBangumi b = runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do

        bId <- fmap getIdFromInsertBy $ insertBy $ Bangumi (T.bangumiUrl b) (T.bangumiName b) (T.bangumiName_cn b) (T.bangumiType b)
                    (T.bangumiEps b) (T.bangumiRank b) (T.ratingTotal . T.bangumiRating $ b) (T.ratingScore . T.bangumiRating $ b)
                    (T.collectionWish . T.bangumiCollection $ b) (T.collectionCollect . T.bangumiCollection $ b)
                    (T.collectionDoing . T.bangumiCollection $ b) (T.collectionOn_hold . T.bangumiCollection $ b) (T.collectionDropped . T.bangumiCollection $ b)
                    (T.ratingcount1 . T.ratingCount . T.bangumiRating $ b) (T.ratingcount2 . T.ratingCount . T.bangumiRating $ b)
                    (T.ratingcount3 . T.ratingCount . T.bangumiRating $ b) (T.ratingcount4 . T.ratingCount . T.bangumiRating $ b)
                    (T.ratingcount5 . T.ratingCount . T.bangumiRating $ b) (T.ratingcount6 . T.ratingCount . T.bangumiRating $ b)
                    (T.ratingcount7 . T.ratingCount . T.bangumiRating $ b) (T.ratingcount8 . T.ratingCount . T.bangumiRating $ b)
                    (T.ratingcount9 . T.ratingCount . T.bangumiRating $ b) (T.ratingcount10 . T.ratingCount . T.bangumiRating $ b)
        forM_ (T.bangumiStaff b) (\s -> do
            sId <- fmap getIdFromInsertBy $ insertBy $ Staff (T.staffUrl s) (T.staffName s) (T.staffName_cn s)
            forM_ (T.staffJobs s) (\job -> do
                void $ insertBy $ BangumiStaff bId sId job
                )
            )

        forM_ (T.bangumiCrt b) (\c -> do
            cId <- fmap getIdFromInsertBy $ insertBy $ Character (T.characterUrl c) (T.characterName c) (T.characterName_cn c)
            forM_ (T.characterActors c) (\a -> do
                aId <- fmap getIdFromInsertBy $ insertBy $ Actor (T.actorUrl a) (T.actorName a)
                void $ insertBy $ BangumiCharacterActor bId cId aId (T.characterRole_name c)
                )
            )

        labels <- liftIO $ findBangumiLabelById $ T.bangumiId b
        forM_ labels (\l -> do
            lId <- fmap getIdFromInsertBy $ insertBy $ Label (fst l)
            void $ insertBy $ BangumiLabel bId lId (snd l)
            )
        
        where getIdFromInsertBy :: Either (Entity a) (Key a) -> Key a
              getIdFromInsertBy result = case result of 
                    Left (Entity key _) -> key
                    Right key           -> key

