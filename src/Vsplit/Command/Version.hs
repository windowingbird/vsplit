{-# LANGUAGE TemplateHaskell #-}

module Vsplit.Command.Version
  ( version
  ) where

import Data.Version
import qualified Paths_vsplit as Vsplit
import Development.GitRev

version :: IO ()
version = putStrLn $ "Version "<> showVersion Vsplit.version <> ", Git " <> $(gitHash) <> ", " <> $(gitCommitCount) <> " commits, Date " <> $(gitCommitDate)
