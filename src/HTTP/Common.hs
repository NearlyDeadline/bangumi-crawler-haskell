--
 -- @Date: 2023-07-17 12:57:47
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-07-19 14:22:33
 -- @FilePath: /bangumi-crawler-haskell/src/HTTP/Common.hs
--
module HTTP.Common 
( findBangumiIdByPageNumber
, findBangumiJsonById
, findBangumiLabelById
) 
where

import ClassyPrelude
import Text.HTML.Scalpel
import Data.Text (replace)
import Network.HTTP.Simple
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Text.Read (readMaybe)



type PageNumber = Int
type BangumiId = Int

headers =
  [
      ("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Safari/537.36 Edg/101.0.1210.39")
  ]

findBangumiIdByPageNumber :: PageNumber -> IO [BangumiId]
findBangumiIdByPageNumber i = do
    let url = "https://bgm.tv/anime/browser?sort=rank&page=" <> show i
    request <- setRequestHeaders headers <$> parseRequest url
    response <- fmap decodeUtf8 <$> httpBS request
    let text = getResponseBody response
    let result = scrapeStringLike text $
                  chroots ("a" @: [hasClass "l"]) $ do
                    let __prefix = "/subject/" 
                    subjectUrl <- attr "href" "a"
                    guard $ __prefix `isPrefixOf` subjectUrl
                    return $ fromMaybe 0 (readMaybe (unpack $ drop (length __prefix) subjectUrl))
    case result of
      Just [] -> return []
      Nothing -> return []
      Just x  -> return x



findBangumiJsonById :: BangumiId -> IO Text
findBangumiJsonById id = do
  let url = "https://api.bgm.tv/subject/" `mappend` show id `mappend` "?responseGroup=medium"
  request <- setRequestHeaders headers <$> parseRequest url
  response <- fmap (TE.decodeUtf8With TEE.lenientDecode) <$> httpBS request
  let text = getResponseBody response
  return $ cleanText text

  where cleanText :: Text -> Text
        cleanText = replace "\\/" "/" . replace "\\r\\n" "" . replace "\\\\" "//"

type LabelText = Text
type LabelCount = Int

findBangumiLabelById :: BangumiId -> IO [(LabelText, LabelCount)]
findBangumiLabelById id = do
  let url = "https://bgm.tv/subject/" `mappend` show id
  request <- setRequestHeaders headers <$> parseRequest url
  response <- fmap decodeUtf8 <$> httpBS request
  let text = getResponseBody response
  case scrapeStringLike text (chroots ("div" @: [hasClass "subject_tag_section"]) scrapeTagSections) of
    Nothing -> return []
    Just x  -> return $ join x
  where
    scrapeTagSections :: Scraper Text [(LabelText, LabelCount)]
    scrapeTagSections = chroots ("a" @: [hasClass "l"]) $ do
        tagName <- text "span"
        tagCountStr <- text "small"
        let tagCount = fromMaybe 0 (readMaybe (unpack tagCountStr))
        return (tagName, tagCount)
