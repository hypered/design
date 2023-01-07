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
    GenerateTemplates Bool
    -- ^ If True, use a prefix suitable for GitHub Pages (i.e. the /design
    -- prefix, since the static site is at hypered.github.io/design).
  | GenerateGuide
  | Wrapper
    -- ^ The document wrapper. This should match `pages/_app.js`.

    -- Individual components

  | GenerateNav
  | GenerateFooter

    -- Stories from Storybook

  | GenerateFormLogin
  | GenerateNavigation
  | GenerateNavigationSpaceBetween
  | GenerateLayoutDefault
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
          "generate-templates"
          ( A.info (parserGenerateTemplates <**> A.helper)
          $ A.progDesc "Generate HTML templates that can be used with Pandoc"
          )

      <> A.command
          "generate-guide"
          ( A.info (parserGenerateGuide <**> A.helper)
          $ A.progDesc "A dummy command"
          )

      <> A.command
          "wrapper"
          ( A.info (parserWrapper <**> A.helper)
          $ A.progDesc "Generate the document wrapper. This should match `pages/_app.js`."
          )

      <> A.command
          "nav"
          ( A.info (parserNav <**> A.helper)
          $ A.progDesc "Generate a nav component."
          )

      <> A.command
          "footer"
          ( A.info (parserFooter <**> A.helper)
          $ A.progDesc "Generate a footer component."
          )

      <> A.command
          "form--login"
          ( A.info (parserFormLogin <**> A.helper)
          $ A.progDesc "Generate a login form example."
          )

      <> A.command
          "navigation--navigation"
          ( A.info (parserNavigation <**> A.helper)
          $ A.progDesc "Generate a navigation bar example."
          )

      <> A.command
          "navigation--navigation-space-between"
          ( A.info (parserNavigationSpaceBetween <**> A.helper)
          $ A.progDesc "Generate a navigation bar example."
          )

      <> A.command
          "layout--default"
          ( A.info (parserLayoutDefault <**> A.helper)
          $ A.progDesc "Generate the default layout."
          )
      )


--------------------------------------------------------------------------------
parserGenerateTemplates :: A.Parser Command
parserGenerateTemplates = GenerateTemplates <$> A.switch
  (A.long "docs" <> A.help "Use a prefix suitable for GitHub Pages.")

parserGenerateGuide :: A.Parser Command
parserGenerateGuide = pure GenerateGuide

parserWrapper :: A.Parser Command
parserWrapper = pure Wrapper

parserNav :: A.Parser Command
parserNav = pure GenerateNav

parserFooter :: A.Parser Command
parserFooter = pure GenerateFooter

parserFormLogin :: A.Parser Command
parserFormLogin = pure GenerateFormLogin

parserNavigation :: A.Parser Command
parserNavigation = pure GenerateNavigation

parserNavigationSpaceBetween :: A.Parser Command
parserNavigationSpaceBetween = pure GenerateNavigationSpaceBetween

parserLayoutDefault :: A.Parser Command
parserLayoutDefault = pure GenerateLayoutDefault
