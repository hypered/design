{-# LANGUAGE NoImplicitPrelude #-}
module Hypered.Design.Run
  ( run
  ) where

import qualified Hypered.Design.Command        as Command
import           Protolude


--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run Command.Dummy = do
  putStrLn "Dummy"
  exitSuccess

run Command.Dummy_ = do
  putStrLn "Dummy_"
  exitSuccess
