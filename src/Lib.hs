{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( someFunc
    ) where

import Numeric.Natural
import Options.Applicative
import System.Process.Typed (readProcess, proc, setWorkingDir)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Concurrent.Async (Async, async, wait)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), takeExtension, replaceExtension)
import System.Exit
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.IO.PlafCompat (nullFileName)
import System.IO (hFlush)
import Data.Text (Text, unpack, pack)
import Data.Text.IO (hPutStrLn)
import Data.Maybe (fromMaybe)

import Vsplit.Command.Version (version)

data Command
  = Split !Natural !Natural !String !String
  | Cut !(Maybe String) !(Maybe String) !String !String !String !String
  | Listen !(Maybe String) !(Maybe String) !String !String !(Maybe String) !String !String
  | Combine ![ Text ] !(Maybe String)
  | Version
  deriving (Eq, Show)

data Vsplit = Vsplit
  { cmd :: !Command
  } deriving (Eq, Show)

cmdParser :: Parser Vsplit
cmdParser = Vsplit <$> hsubparser (splitCommand <> cutCommand <> listenCommand <> combineCommand <> versionCommand)

cmdParserInfo :: ParserInfo Vsplit
cmdParserInfo = info (cmdParser <**> helper) idm

splitCommand :: Mod CommandFields Command
splitCommand = command "split"
  ( info
    ( Split
      <$> option auto
      (   long "duration"
          <> short 'd'
          <> value 4800
          <> metavar "DURATION"
          <> help "specify split duration, default value is 4800"
      )
      <*> option auto
      (   long "number"
          <> short 'n'
          <> value 0
          <> metavar "NUMBER"
          <> help "specify output file base number, default value is 0"
      )
      <*> strOption
      (    long "dst"
           <> short 'D'
           <> value "out"
           <> metavar "DIST"
           <> help "specify dist directory, default value is ./out"
      )
      <*> strArgument
      (    metavar "SOURCE"
           <> help "specify source file"
      )
    )
    ( progDesc "split a video file"
    )
  )

cutCommand :: Mod CommandFields Command
cutCommand = command "cut"
  ( info
    ( Cut
      <$> optional
      ( strOption
        ( long "ss"
          <> metavar "START"
          <> help "specify start time, format HH:MM:SS[.SSS], optional" )
      )
      <*> optional
      ( strOption
        ( long "to"
          <> metavar "END"
          <> help "specify end time, format HH:MM:SS[.SSS], optional" )
      )
      <*> ( strOption
        ( long "vb"
          <> value "4M"
          <> metavar "VIDEO BITRATE"
          <> help "specify video bitrate, default 4M, optional" )
      )
      <*> ( strOption
        ( long "ab"
          <> value "128K"
          <> metavar "AUDIO BITRATE"
          <> help "specify audio bitrate, default 128K, optional" )
      )
      <*> strOption
      (    long "out"
           <> short 'o'
           <> value "output.mp4"
           <> metavar "OUT"
           <> help "specify output file, default value is output.mp4"
      )
      <*> strArgument
      (    metavar "SOURCE"
           <> help "specify source file"
      )
    )
    ( progDesc "cut a video file"
    )
  )

listenCommand :: Mod CommandFields Command
listenCommand = command "listen" ( info
    ( Listen
      <$> optional
      ( strOption
        ( long "ss"
          <> metavar "START"
          <> help "specify start time, format HH:MM:SS[.SSS], optional" )
      )
      <*> optional
      ( strOption
        ( long "to"
          <> metavar "END"
          <> help "specify end time, format HH:MM:SS[.SSS], optional" )
      )
      <*> ( strOption
        ( long "format"
          <> short 'f'
          <> value "aac"
          <> metavar "AUDIO FORMAT"
          <> help "specify audio format, default aac, optional" )
      )
      <*> ( strOption
        ( long "ab"
          <> value "128K"
          <> metavar "AUDIO BITRATE"
          <> help "specify audio bitrate, default 128K, optional" )
      )
      <*> optional
      ( strOption
        ( long "volume"
          <> short 'v'
          <> metavar "VOLUME"
          <> help "specify volume adjustment, format [-]XdB, optional" )
      )
      <*> strOption
      (    long "out"
           <> short 'o'
           <> value "output.m4a"
           <> metavar "OUT"
           <> help "specify output file, default value is output.m4a, file extension for mp3 and aac format will be automaticlly alternatived"
      )
      <*> strArgument
      (    metavar "SOURCE"
           <> help "specify source file"
      )
    )
    ( progDesc "listen a video file"
    )
  )

combineCommand :: Mod CommandFields Command
combineCommand = command "combine" ( info
    ( Combine  <$> many
      ( strArgument
        (metavar "INPUTS"
          <> help "specify input files" )
      )
      <*> optional
      ( strOption
        ( long "out"
          <> short 'o'
          <> metavar "OUT"
          <> help "specify output file, default value is p1.[extension]" )
      )
    )
    ( progDesc "Combine video files"
    )
  )

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Print version"))

commandDispatch :: Command -> IO ()
commandDispatch (Split duration base dst file) = split duration base dst file
commandDispatch (Cut start end vb ab out file) = cut start end vb ab out file
commandDispatch (Listen start end format ab vo out file) = listen start end format ab vo out file
commandDispatch (Combine inputs output) = combine inputs output
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
  --let batchSize = ceiling (videoDuration / fromIntegral duration)
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
  let no = floor (fromIntegral cur / fromIntegral duration :: Double) + 1 :: Int
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

cut :: Maybe String -> Maybe String -> String -> String -> String -> String -> IO ()
cut ssM toM vb ab output input = do
  cur <- getCurrentDirectory
  let ss = case ssM of
        Just ssStr -> ["-ss", ssStr]
        Nothing -> mempty
  let to = case toM of
        Just toStr -> ["-to", toStr]
        Nothing -> mempty
  let argsa = ss <> to <> ["-accurate_seek", "-i", cur </> input, "-codec:v", "libx265"]
  let argsb = ["-f", "mp4", "-codec:a", "aac", "-avoid_negative_ts", "make_zero", "-y"]
  withSystemTempDirectory "vsplit" $ \tempDir -> do
    let pass1args = argsa <> ["-x265-params", "pass=1"] <> argsb <> [nullFileName]
    let pass2args = argsa <> ["-x265-params", "pass=1", "-b:v", vb, "-b:a", ab] <> argsb <> [cur </> output]
    let proc1 = proc "ffmpeg" pass1args
    putStrLn $ "ffmpeg" <> " " <> unwords pass1args
    (exitCode, _out, err) <- readProcess . setWorkingDir tempDir $ proc1
    case exitCode of
      ExitFailure e -> do
        print e
        print err
      ExitSuccess -> do
        let proc2 = proc "ffmpeg" pass2args
        putStrLn $ "ffmpeg" <> " " <> unwords pass2args
        (exitCode2, _out, _err) <- readProcess . setWorkingDir tempDir $ proc2
        case exitCode2 of
          ExitFailure e -> do
            print e
            print err
          ExitSuccess -> pure ()
    pure ()
  pure ()

listen :: Maybe String -> Maybe String -> String -> String -> Maybe String -> String -> String -> IO ()
listen ssM toM format ab voM output input = do
  cur <- getCurrentDirectory
  let ss = case ssM of
        Just ssStr -> ["-ss", ssStr]
        Nothing -> mempty
  let to = case toM of
        Just toStr -> ["-to", toStr]
        Nothing -> mempty
  let vo = case voM of
        Just voStr -> ["-af","volume=" <> voStr]
        Nothing -> mempty
  let outfile = case format of
        "mp3" -> replaceExtension output "mp3"
        "aac" -> replaceExtension output "m4a"
        _ -> output
  let args = ss <> to <> ["-accurate_seek", "-i", cur </> input, "-vn", "-codec:a", format, "-b:a", ab] <> vo <> ["-avoid_negative_ts", "make_zero", "-y", outfile]
  putStrLn $ "ffmpeg" <> " " <> unwords args
  (exitCode, _out, _err) <- readProcess . proc "ffmpeg" $ args
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure e -> print e
  pure ()

combine :: [ Text ] -> Maybe String -> IO ()
combine inputs outputM = do
  cur <- getCurrentDirectory
  let ext = takeExtension . unpack . head $ inputs
  let output = fromMaybe (replaceExtension "p1.flv" ext) outputM
  withSystemTempFile "vsplit-combine" $ \tempFile tempFileHandle -> do
    mapM_ (hPutStrLn tempFileHandle . (pack "file '" <>) . (<> pack "'") . pack . (cur </>) . unpack) inputs
    hFlush tempFileHandle
    let args = ["-f", "concat", "-safe", "0", "-i", tempFile, "-c", "copy", output]
    putStrLn $ "ffmpeg" <> " " <> unwords args
    (exitCode, _out, err) <- readProcess . proc "ffmpeg" $ args
    case exitCode of
      ExitSuccess -> pure ()
      ExitFailure e -> do
        print e
        print err
