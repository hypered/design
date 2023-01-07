{-# LANGUAGE NoImplicitPrelude #-}
module Main
  ( main
  ) where

import qualified Hypered.Design.Command        as Command
import qualified Hypered.Design.Run            as Run
import qualified Options.Applicative           as A
import           Protolude


--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser Command.parserInfo >>= Run.run
