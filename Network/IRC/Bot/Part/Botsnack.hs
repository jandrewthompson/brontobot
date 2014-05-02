{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.Part.Botsnack where

import Control.Monad            (replicateM)
import Control.Monad.Trans      (liftIO)
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (pack)
import Data.Monoid              ((<>))
import Network.IRC.Bot.Log      (LogLevel(Debug))
import Network.IRC.Bot.BotMonad (BotMonad(..), maybeZero)
import Network.IRC.Bot.Commands (PrivMsg(..), sendCommand, replyTo)
import Network.IRC.Bot.Parsec   (botPrefix, nat, parsecPart)
import System.Random            (randomRIO)
import Text.Parsec              (ParsecT, (<|>), (<?>), char, skipMany1, space, string, try)

snackPart :: (BotMonad m) => m ()
snackPart = parsecPart snackCommand

snackCommand :: (BotMonad m) => ParsecT ByteString () m ()
snackCommand =
    do try $ botPrefix >> string "botsnack"
       logM Debug "snackPart"
       target <- maybeZero =<< replyTo
       sendCommand (PrivMsg Nothing [target] "Mmmmmm... Barrrrr-B-Q!")
    <|> return ()
