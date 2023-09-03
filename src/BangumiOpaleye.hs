--
 -- @Date: 2022-05-11 18:32:11
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-09-03 14:23:41
 -- @FilePath: /bangumi-crawler-haskell/src/BangumiOpaleye.hs
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module BangumiOpaleye

where
import ClassyPrelude hiding (sum, groupBy, max)
import Opaleye
import Data.Profunctor.Product
import Data.Profunctor.Product.Default (Default)
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor as P
import qualified Opaleye.Internal.Join
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Scientific as Scientific

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . pack . fromMaybe "Empty select" . showSql


labelTable :: Table (Field SqlInt8, Field SqlText) (Field SqlInt8, Field SqlText)
labelTable = table "label" $ p2 (tableField "id", tableField "name")


labelSelect :: Select (Field SqlInt8, Field SqlText) -- Select by Type
labelSelect = selectTable labelTable

labelText :: Select (Field SqlText)  -- Projection
labelText = do
  (_, name) <- labelSelect
  pure name

bangumiLabelProduct :: Select (Field SqlText, Field SqlText, Field SqlInt8) -- Product/ Inner Join
-- SELECT bangumi.name_cn, label."name", bangumi_label.label_count
-- FROM bangumi 
-- INNER JOIN bangumi_label 
-- ON bangumi."id" = bangumi_label.bangumi_id
-- INNER JOIN label
-- ON bangumi_label.label_id = label."id";
bangumiLabelProduct = do
  bgmRow <- bangumiSelect
  let bangumiTable_id = bIdField bgmRow
      bgmNamecn = bNamecnField bgmRow
  (labelId, labelName) <- labelSelect
  (_, bgmLabelTable_bgmId, bgmLabelTable_labelId, bgmLabelTable_labelCount) <- selectTable bangumiLabelTable

  where_ $ (bangumiTable_id .== bgmLabelTable_bgmId) .&& (labelId .== bgmLabelTable_labelId)
  pure (bgmNamecn, labelName, bgmLabelTable_labelCount)

bangumiLabelTable :: Table (Field SqlInt8, Field SqlInt8, Field SqlInt8, Field SqlInt8) (Field SqlInt8, Field SqlInt8, Field SqlInt8, Field SqlInt8)
bangumiLabelTable = table "bangumi_label" $ p4 (tableField "id", tableField "bangumi_id", tableField "label_id", tableField "label_count")

-------------------------------------------------------------

data Bangumi = Bangumi { bgmId :: Int64
                       , bgmNamecn :: Text
                       , bgmEps :: Int64
                       , bgmRank :: Int64
                       , bgmRatingTotal :: Int64
                       , bgmRatingScore :: Double} deriving (Show)


data BangumiField = BangumiField { bIdField :: Field SqlInt8
                                 , bNamecnField :: Field SqlText
                                 , bEpsField :: Field SqlInt8
                                 , bRankField :: Field SqlInt8
                                 , bRatingTotalField :: Field SqlInt8
                                 , bRatingScoreField :: Field SqlFloat8}

bangumiTable :: Table BangumiField BangumiField
bangumiTable = table "bangumi" (BangumiField
                  <$> P.lmap bIdField (tableField "id")
                  <*> P.lmap bNamecnField (tableField "name_cn")
                  <*> P.lmap bEpsField (tableField "eps")
                  <*> P.lmap bRankField (tableField "rank")
                  <*> P.lmap bRatingTotalField (tableField "rating_total")
                  <*> P.lmap bRatingScoreField (tableField "rating_score"))

bangumiSelect :: Select BangumiField  -- Select by new type 
bangumiSelect = selectTable bangumiTable


instance Default Unpackspec BangumiField BangumiField where
  def = BangumiField <$> P.lmap bIdField D.def
                     <*> P.lmap bNamecnField D.def
                     <*> P.lmap bEpsField D.def
                     <*> P.lmap bRankField D.def
                     <*> P.lmap bRatingTotalField D.def
                     <*> P.lmap bRatingScoreField D.def


aggregateBangumi :: Select (Field SqlFloat8, Field SqlInt8, Field SqlInt8, Field SqlInt8, Field SqlFloat8)
-- SELECT rating_score, eps, count("id"), max("rank"), avg(rating_total) 
-- FROM bangumi 
-- WHERE rating_total >= 500 
-- GROUP BY (rating_score, eps);
aggregateBangumi = aggregate ((,,,,) <$> P.lmap bRatingScoreField groupBy
                                     <*> P.lmap bEpsField groupBy
                                     <*> P.lmap bIdField count
                                     <*> P.lmap bRankField max
                                     <*> P.lmap (unsafeCoerceField . bRatingTotalField) avg)

                              bangumiTable'

      where bangumiTable' :: Select BangumiField  -- WHERE rating_toatal >= 500
            bangumiTable' = do
              row <- selectTable bangumiTable
              where_ (bRatingTotalField row .>= 500)
              pure row


aggregateBangumiHavingCount :: Select (Field SqlFloat8, Field SqlInt8, Field SqlInt8, Field SqlInt8, Field SqlFloat8)
-- SELECT rating_score, eps, count("id"), max("rank"), avg(rating_total) 
-- FROM bangumi 
-- GROUP BY (rating_score, eps)
-- HAVING count("id") > 1;
aggregateBangumiHavingCount = do
  res@(_, _, idCount, _, _) <- aggregate ((,,,,) <$> P.lmap bRatingScoreField groupBy
                                            <*> P.lmap bEpsField groupBy
                                            <*> P.lmap bIdField count
                                            <*> P.lmap bRankField max
                                            <*> P.lmap (unsafeCoerceField . bRatingTotalField) avg)

                              $ selectTable bangumiTable
  where_ (idCount .> 1) -- HAVING count("id") >= 500
  pure res

------------------------------------------------



instance Default FromFields BangumiField Bangumi where
  def = Bangumi <$> P.lmap bIdField D.def
                <*> P.lmap bNamecnField D.def
                <*> P.lmap bEpsField D.def
                <*> P.lmap bRankField D.def
                <*> P.lmap bRatingTotalField D.def
                <*> P.lmap bRatingScoreField D.def


connStr :: ByteString
connStr = "host=localhost dbname=bangumi user=mike password=mike port=5432"


runBangumiSelect :: IO [Bangumi]
runBangumiSelect =
  PGS.connectPostgreSQL connStr >>= (`runSelect` bangumiSelect)

-- 遇到types incompatible异常，去https://hackage.haskell.org/package/opaleye-0.10.0.0/docs/Opaleye-Internal-PGTypesExternal.html
-- 查看DefaultFromField a b的类型，a为PG类型，b为Haskell类型
-- 这里注意avg返回PG中的numeric，对应Scientific类型
runAggregateBangumiSelect :: IO [(Double, Int64, Int64, Int64, Scientific.Scientific)]
runAggregateBangumiSelect =
  PGS.connectPostgreSQL connStr >>= (`runSelect` aggregateBangumi)

runAggregateBangumiSelectHavingCount :: IO [(Double, Int64, Int64, Int64, Scientific.Scientific)]
runAggregateBangumiSelectHavingCount =
  PGS.connectPostgreSQL connStr >>= (`runSelect` aggregateBangumiHavingCount)


runGenericSelect :: Default FromFields fields haskells => Select fields -> IO [haskells]
runGenericSelect c = PGS.connectPostgreSQL connStr >>= (`runSelect` c)


instance DefaultFromField SqlFloat8 Scientific.Scientific where
  defaultFromField = fromPGSFromField











