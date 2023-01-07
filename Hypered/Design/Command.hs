{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Hypered.Design.Command
  ( Command(..)
  , parserInfo
  ) where

import qualified Options.Applicative           as A
import           Protolude


--------------------------------------------------------------------------------
data Command =
    Dummy
  | Dummy_
  deriving (Eq, Show)


--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "hypered-design - The source of truth for the Hypered design system"
    <> A.progDesc
         "The Hypered design system."


--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
      (  A.command
          "dummy"
          ( A.info (parserDummy <**> A.helper)
          $ A.progDesc "A dummy command"
          )

      <> A.command
          "dummy_"
          ( A.info (parserDummy_ <**> A.helper)
          $ A.progDesc "A dummy command"
          )
      )


--------------------------------------------------------------------------------
parserDummy :: A.Parser Command
parserDummy = pure Dummy

parserDummy_ :: A.Parser Command
parserDummy_ = pure Dummy_
