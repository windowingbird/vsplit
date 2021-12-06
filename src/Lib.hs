{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( someFunc
    ) where

import Numeric.Natural
import Options.Applicative
import System.Process.Typed (readProcess, proc)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Concurrent.Async (Async, async, wait)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import System.Exit

import Vsplit.Command.Version (version)

data Command
  = Split !Natural !Natural !String !String
  | Version
  deriving (Eq, Show)

data Vsplit = Vsplit
  { cmd :: !Command
  } deriving (Eq, Show)

cmdParser :: Parser Vsplit
cmdParser = Vsplit <$> hsubparser (splitCommand <> versionCommand)

cmdParserInfo :: ParserInfo Vsplit
cmdParserInfo = info (cmdParser <**> helper) idm

splitCommand :: Mod CommandFields Command
splitCommand = command "split" (info (Split <$> option auto (long "duration" <> short 'd' <> value 4800 <> metavar "DURATION" <> help "specify split duration, default value is 4800") <*> option auto (long "number" <> short 'n' <> value 0 <> metavar "NUMBER" <> help "specify output file base number, default value is 0") <*> strOption (long "dst" <> short 'D' <> value "out" <> metavar "DIST" <> help "specify dist directory, default value is ./out") <*> strArgument (metavar "SOURCE" <> help "specify source file")) (progDesc "split a video file"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Print version"))

commandDispatch :: Command -> IO ()
commandDispatch (Split duration base dst file) = split duration base dst file
commandDispatch Version = version

someFunc :: IO ()
someFunc = do
  Vsplit { cmd } <- execParser cmdParserInfo
  commandDispatch cmd
  pure ()

split :: Natural -> Natural -> String -> FilePath -> IO ()
split duration base dst file = do
  let args = ["-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", file]
  (exitCode, out, _err) <- readProcess . proc "ffprobe" $ args
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure e -> print e
  let videoDuration = read (BLC.unpack out) :: Float
  let batchSize = ceiling (videoDuration / fromIntegral duration)
  createDirectoryIfMissing True dst
  splitProcessIter videoDuration duration 0 base dst file []
  pure ()

splitProcessIter :: Float -> Natural -> Natural -> Natural -> String -> FilePath -> [ Async () ]-> IO ()
splitProcessIter videoDuration duration cur base dst file fibers = do
  let next = cur + duration
  if fromIntegral cur < videoDuration
    then do
    fiber <- async $ splitProcess videoDuration duration cur base dst file
    splitProcessIter videoDuration duration next base dst file (fiber : fibers)
    else
    mapM_ wait fibers

splitProcess :: Float -> Natural -> Natural -> Natural -> String -> FilePath -> IO ()
splitProcess videoDuration duration cur base dst file = do
  let next = cur + duration
  let no = floor (fromIntegral cur / fromIntegral duration) + 1 :: Int
  let ss = if cur < duration
        then []
        else ["-ss", show cur]
  let to = if fromIntegral cur > (videoDuration - fromIntegral duration)
        then []
        else ["-to", show next]
  let args = ss <> to <> ["-accurate_seek", "-i", file, "-codec:v", "copy", "-codec:a", "copy", "-avoid_negative_ts", "make_zero", "-y", dst </> "p" <> show (fromIntegral base + no) <> takeExtension file]
  putStrLn $ "ffmpeg" <> " " <> unwords args
  (exitCode, _out, _err) <- readProcess . proc "ffmpeg" $ args
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure e -> print e
  pure ()
