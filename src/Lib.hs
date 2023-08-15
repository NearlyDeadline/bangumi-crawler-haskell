--
 -- @Date: 2022-05-11 18:32:11
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-08-03 13:42:29
 -- @FilePath: /bangumi-crawler-haskell/src/Lib.hs
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( main
    , someFunc
    ) where
import ClassyPrelude
import Type.Main
import Data.Aeson
import HTTP.Common
import PostgreSQL.Common (insertBangumi, migrate)
import Control.Concurrent (forkIO, threadDelay)
import Katip
import Data.Time

someFunc :: Int -> IO (Maybe Bangumi)
someFunc i = do
    bgmjson <- findBangumiJsonById i
    return (decodeStrict . encodeUtf8 $ bgmjson :: Maybe Bangumi)

main :: IO ()
main = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "BangumiCrawler" "production"
  -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket makeLogEnv closeScribes $ \le -> do
    let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
    let initialNamespace = "main"
    runKatipContextT le initialContext initialNamespace $ do
      run


run :: KatipContextT IO ()
run = do
    chan <- newTChanIO

    startTime <- liftIO getCurrentTime
    $(logTM) InfoS $ "Start Time: " <> showLS startTime
    liftIO migrate

    let crawlThreads = 4
    liftIO $ forConcurrently_ [1..crawlThreads] $ \i -> do
        let crawlPages = 30
        let pages = [(i - 1) * crawlPages + 1 .. i * crawlPages]

        for_ pages $ \p -> do
          bgmUrlIds <- findBangumiIdByPageNumber p
          threadDelayOneSecond
          for_ bgmUrlIds $ \bid -> do
            bgmjson <- findBangumiJsonById bid
            threadDelayOneSecond
            case decodeStrict . encodeUtf8 $ bgmjson :: Maybe Bangumi of
              Just bgm -> do
                -- putStrLn $ "Successfully parsed bangumi: " <> tshow bid
                atomically $ writeTChan chan bgm
              Nothing ->
                putStrLn $ "Unsuccessfully parsed bangumi: " <> tshow bid
    liftIO $ processChan chan

    endTime <- liftIO getCurrentTime
    $(logTM) InfoS $ "End Time: " <> showLS endTime

    where
      threadDelayOneSecond = threadDelay 1000000

      processChan :: TChan Bangumi -> IO ()
      processChan chan = do
        bgm <- atomically $ tryReadTChan chan
        case bgm of
          Just b -> insertBangumi b >> processChan chan
          Nothing -> do
            isChanEmpty <- atomically $ isEmptyTChan chan
            unless isChanEmpty $ threadDelayOneSecond >> processChan chan







