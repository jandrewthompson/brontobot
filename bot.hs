import Network
import System.IO
import Text.Printf
import Text.Regex.Posix
import Data.List
import Data.List.Split
import System.Exit

server = "irc.freenode.org"
port = 6667
chan = "#bitswebteam"
nick = "brontobot"

main = do
	h <- connectTo server (PortNumber (fromIntegral port))
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
	if isMsg s && toMe (clean s) then speak s else eval h (clean s)
	putStrLn s
  where
  	forever a = a >> forever a
	
	isMsg x = "PRIVMSG" `isInfixOf` x
	toMe x  = "brontobot" `isInfixOf` last (splitOn ":" x)
	speak x = privmsg h "I'm a brontobot." 

eval :: Handle -> String -> IO ()
eval h   "!quit"					= write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x    = privmsg h (drop 4 x)
eval _  _ 							= return ()

clean :: String -> String
clean c = drop 1 ( dropWhile (/=':') c)

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
