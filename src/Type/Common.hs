--
 -- @Date: 2023-07-17 16:15:21
 -- @LastEditors: Mike
 -- @LastEditTime: 2023-07-18 15:40:44
 -- @FilePath: /bangumi-crawler-haskell/src/Type/Common.hs
--
module Type.Common where

import ClassyPrelude


fieldLabelModifierGeneric :: String -> String -> String
fieldLabelModifierGeneric prefix = drop (length prefix) . toLower

