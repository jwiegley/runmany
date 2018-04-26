{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List (elemIndex)
import           Data.Monoid
import           Options.Applicative as Opt
import           System.Exit
import           System.IO
import           System.Process

data Options = Options
    { verbose  :: Bool
    , commands :: [(String,String)]
    }
    deriving Show

splitOn :: Char -> String -> (String, String)
splitOn c xs =
    maybe ("unnamed", xs) (\(x, _:y) -> (x, y))
        $ flip splitAt xs <$> elemIndex c xs

options :: Opt.Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display verbose output")
    <*> some (option (splitOn '=' <$> str)
                     (short 'c' <>
                      long "command" <>
                      help "Display verbose output"))

data MsgType = MsgError | MsgOutput
    deriving (Eq, Show)

data Message = Message
    { messageType    :: MsgType
    , messageOrigin  :: String
    , messageContent :: ByteString
    }
    deriving Show

loggingActor :: Bool -> TChan Message -> IO ()
loggingActor verb msgs = forever $ do
    when verb $ putStrLn "Waiting for message..."
    msg@Message {..} <- atomically $ readTChan msgs
    when verb $ putStrLn $ "Read message: " ++ show msg
    putStr $ messageOrigin ++ ": "
          ++ case messageType of
              MsgError -> "ERROR: "
              MsgOutput -> ""
    BS.putStr messageContent
    putStrLn ""

readOutput :: Bool -> TChan Message -> MsgType -> String -> Handle -> IO ()
readOutput verb msgs msgType label h = go
  where
      go = do
          eof <- hIsEOF h
          unless eof $ do
              line <- BS.hGetLine h
              when verb $ putStrLn $ "read: " ++ label ++ ": " ++ show line
              atomically $ writeTChan msgs (Message msgType label line)
              go

runCommands :: Options -> IO ()
runCommands opts = do
    msgs <- newTChanIO
    withAsync (loggingActor verb msgs) $ \_ -> do
        hs <- forM (commands opts) $ \(label, cmd) -> do
            let p = CreateProcess
                    { cmdspec            = ShellCommand cmd
                    , cwd                = Nothing
                    , env                = Nothing
                    , std_in             = Inherit
                    , std_out            = CreatePipe
                    , std_err            = CreatePipe
                    , close_fds          = False
                    , create_group       = False
                    , delegate_ctlc      = False
                    , detach_console     = False
                    , create_new_console = False
                    , new_session        = False
                    -- , use_process_jobs   = False
                    , child_group        = Nothing
                    , child_user         = Nothing
                    }
            when verb $ putStrLn $ "Executing: " ++ cmd
            (_, Just hout, Just herr, h) <- createProcess p

            t1 <- async $ readOutput verb msgs MsgOutput label hout
            link t1
            t2 <- async $ readOutput verb msgs MsgError label herr
            link t2

            return (h, cmd)

        when verb $ putStrLn "Waiting for processes to finish..."
        forM_ hs $ \(h, cmd) -> do
            exitCode <- waitForProcess h
            case exitCode of
                ExitFailure n ->
                    errorWithoutStackTrace
                        $ "runmany: command failed with exit code "
                            ++ show n ++ ": " ++ cmd
                _ -> return ()

        when verb $ putStrLn "Waiting for logging queue to flush..."
        atomically $ do
            e <- isEmptyTChan msgs
            unless e retry

        when verb $ putStrLn "Exiting..."
  where
    verb = verbose opts

main :: IO ()
main = execParser opts >>= runCommands
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Run multiple commands, interleaving output and errors"
                 <> header "runmany - run many commands at once")
