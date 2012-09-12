import Network
import System.IO
import System.Random
import System.Exit
import Text.Printf
import Text.Regex.Posix
import Data.List
import Data.List.Split
import Data.Char
import Network.HTTP
import Network.Browser
import Control.Exception

server = "irc.freenode.org"
ircport = 6667
-- chan = "#bitswebteam"
chan = "#hsbot-testing"
nick = "brontobot"

main = do
    h <- connectTo server (PortNumber (fromIntegral ircport))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :hsbot")
    write h "JOIN" chan
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s 
        then pong s 
        else
            if isMsg s && toMe (clean s) 
                then speak s
                else eval h (clean s) 
    putStrLn s
  where
    forever a = a >> forever a

    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)
    isMsg x   = "PRIVMSG" `isInfixOf` x
    toMe x    = "brontobot" `isInfixOf` last (splitOn ":" x) || "@whatis" `isInfixOf` x
    speak x | isInfixOf "hello"    x  = privmsg h "I'm a brontobot." 
            | isInfixOf "hi"       x  = privmsg h "I'm a brontobot." 
            | isInfixOf "whats up" x  = privmsg h "Not much.  I'm a brontobot." 
            | isInfixOf "what's up"x  = privmsg h "Not a whole lot.  I'm a brontobot." 
			| isInfixOf "@whatis " x  = 
								do
									txt <- wiki $ last $ splitOn "@whatis " x
									privmsg h txt 
            | otherwise               = privmsg h $ pick x

eval :: Handle -> String -> IO ()
eval h   "!quit"                      = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x      = privmsg h (drop 4 x)
eval _  _                             = return ()

clean :: String -> String
clean c = drop 1 ( dropWhile (/=':') c)

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

-- Given an input string, (start,end), return a random Int between (start,end)
getR :: [Char] -> (Int, Int) -> Int
getR (_:xs) z = getR' xs 42 z

-- Given an input string, seed, (start,end), return a random Int between (start,end)
getR' :: [Char] -> Int -> (Int, Int) -> Int
getR' [x] y z = fst $ randomR z (mkStdGen (y * 9234))
getR' (_:xs) y z = getR' xs (y + 1) z

-- given a given input string, return a random saying 
-- This uses some a rather lame 'ramdom' number generator
pick :: [Char] -> [Char] 
pick (xs) = args !! (getR xs (0, (length args) - 1) )
    where
        args = ["It is certain","Without a doubt","Concentrate and ask again","My reply is no","Outlook not so good","So... it has come to this","So... it has come to this","So... it has come to this"]


-- getit :: [Char] -> String
getit x = 
    do
        rsp <- simpleHTTP (getRequest x)
        (getResponseBody rsp)
   
wiki :: [Char] -> IO [Char]
wiki x = 
    do
        dd <- getit $ "http://en.wikipedia.org/w/api.php?action=query&prop=revisions&titles=" ++ urlEncode x ++ "&rvprop=content&redirects&rvsection=0"  
        do
            return ( parse $ getAllTextMatches(dd =~ "''' (.*)" :: AllTextMatches [] [Char] ) )

parse [] = "Hmm... brontobot is unsure."
parse (xs) = last $ splitOn "'''" $ last xs 



