{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.Part.Hello where

import Control.Monad.Trans      (liftIO)
import Control.Applicative (Applicative, Alternative, (<$>))
import Data.Maybe               (fromMaybe)
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (unpack, pack)
import Data.Monoid              ((<>))
import Data.String.Utils        (strip)
import Network.IRC.Bot.Log      (LogLevel(Debug))
import Network.IRC.Bot.BotMonad (BotMonad(..), BotEnv, botName, maybeZero)
--import Network.IRC.Bot.BotMonad 
import Network.IRC.Bot.Commands (PrivMsg(..),askSenderNickName, replyTo, privMsg, sendCommand)
import Network.IRC.Bot.Parsec   (botPrefix, parsecPart)
import System.Random            (randomRIO)
import Text.Parsec              (ParsecT, (<|>), sepBy, string, try, noneOf, oneOf, many)
import Data.List
import Data.List.Split          (splitOn)

helloPart :: (BotMonad m) => m ()
helloPart = hiPart

hiPart :: (BotMonad m) => m ()
hiPart = 
    do
       priv <- privMsg
       let msgString = unpack (msg priv)
       --logM Debug $ pack ("Got a PM: " ++ msgString)
       target <- maybeZero =<< replyTo
       mNick <- askSenderNickName
       if isHi msgString
           then do
               let greetings = ["Hello", "Howdy", "Greetings", "What up"]
               n <- liftIO $ randomRIO (0, length greetings - 1)
               let byeMsg = greetings!!n <> ", " <> (fromMaybe "stranger" mNick)
               sendCommand (PrivMsg Nothing [target] byeMsg)
            else
                return ()
       where
            isHi x = "morning" `isInfixOf` x ||
                      "hello" `isInfixOf` x 

byePart :: (BotMonad m) => m ()
byePart = 
    do
       priv <- privMsg
       let msgString = unpack (msg priv)
       target <- maybeZero =<< replyTo
       mNick <- askSenderNickName
       if isBye msgString
           then do
               let greetings = ["Make like a bot and netsplit", "See ya", "Later Gator", "Word to your mother"]
               n <- liftIO $ randomRIO (0, length greetings - 1)
               let byeMsg = greetings!!n <> ", " <> (fromMaybe "stranger" mNick)
               sendCommand (PrivMsg Nothing [target] byeMsg)
            else
                return ()
       where
            isBye x = "bye" `isInfixOf` x ||
                      "night" `isInfixOf` x ||
                      "later" `isInfixOf` x ||
                      "see ya" `isInfixOf` x 


chuckPart :: (BotMonad m) => m ()
chuckPart = 
    do
       priv <- privMsg
       let msgString = unpack (msg priv)
       target <- maybeZero =<< replyTo
       mNick <- askSenderNickName
       if forChuck msgString
           then do
               let byeMsg = pack $ "Chuck Norris could " ++ (map strip $ splitOn "can't" msgString)!!1
               sendCommand (PrivMsg Nothing [target] byeMsg)
            else
                return ()
       where
            forChuck x = "can't" `isInfixOf` x




questionPart :: (BotMonad m) => m ()
questionPart = 
    do
       priv <- privMsg
       let msgString = unpack (msg priv)
       target <- maybeZero =<< replyTo
       botNameVal <- botName <$> askBotEnv
       logM Debug $ pack ("Got a PM: " ++ (unpack botNameVal))
       mNick <- askSenderNickName
       if (isQ msgString && (unpack botNameVal) `isInfixOf` msgString)
           then do
               let greetings = ["How in the world would I know that", "If you say so", "Would you be my BFF", "Most certainly", "It is to be expected"]
               n <- liftIO $ randomRIO (0, length greetings - 1)
               let byeMsg = greetings!!n <> ", " <> (fromMaybe "stranger" mNick)
               sendCommand (PrivMsg Nothing [target] byeMsg)
            else
                return ()
       where
            isQ x = "?" `isInfixOf` x 
