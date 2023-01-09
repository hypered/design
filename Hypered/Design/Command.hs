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

  | GenerateAnchorBlue
  | GenerateAnchorBlack
  | GenerateNav
  | GenerateBannerGreen
  | GenerateBannerRed
  | GenerateBannerYellow
  | GenerateButtonPrimary
  | GenerateButtonPrimaryDisabled
  | GenerateButtonSecondary
  | GenerateButtonSecondaryDisabled
  | GenerateButtonFullWidth
  | GenerateFooter

    -- Stories from Storybook

  | GenerateFormLogin
  | GenerateNavigation
  | GenerateNavigationSpaceBetween
  | GenerateLayoutDefault

    -- Helpers to explore Storybook stories

  | ListCategories
  | ListStories
  | JsImportStories
  | JsStories
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
          "a--blue"
          ( A.info (parserAnchorBlue <**> A.helper)
          $ A.progDesc "Generate a blue link component."
          )

      <> A.command
          "a--black"
          ( A.info (parserAnchorBlack <**> A.helper)
          $ A.progDesc "Generate a black link component."
          )

      <> A.command
          "nav"
          ( A.info (parserNav <**> A.helper)
          $ A.progDesc "Generate a nav component."
          )

      <> A.command
          "banner--green"
          ( A.info (parserBannerGreen <**> A.helper)
          $ A.progDesc "Generate a green banner component."
          )

      <> A.command
          "banner--red"
          ( A.info (parserBannerRed <**> A.helper)
          $ A.progDesc "Generate a red banner component."
          )

      <> A.command
          "banner--yellow"
          ( A.info (parserBannerYellow <**> A.helper)
          $ A.progDesc "Generate a yellow banner component."
          )

      <> A.command
          "button--primary"
          ( A.info (parserButtonPrimary <**> A.helper)
          $ A.progDesc "Generate a primary button component."
          )

      <> A.command
          "button--primary-disabled"
          ( A.info (parserButtonPrimaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled primary button component."
          )

      <> A.command
          "button--secondary"
          ( A.info (parserButtonSecondary <**> A.helper)
          $ A.progDesc "Generate a secondary button component."
          )

      <> A.command
          "button--secondary-disabled"
          ( A.info (parserButtonSecondaryDisabled <**> A.helper)
          $ A.progDesc "Generate a disabled secondary button component."
          )

      <> A.command
          "button--full-with"
          ( A.info (parserButtonFullWidth <**> A.helper)
          $ A.progDesc "Generate a full-width button component."
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

      <> A.command
          "list-categories"
          ( A.info (parserListCategories <**> A.helper)
          $ A.progDesc "List Storybook categories."
          )

      <> A.command
          "list-stories"
          ( A.info (parserListStories <**> A.helper)
          $ A.progDesc "List Storybook stories."
          )

      <> A.command
          "js-import-stories"
          ( A.info (parserJsImportStories <**> A.helper)
          $ A.progDesc "Generate JS code to import the Storybook stories."
          )

      <> A.command
          "js-stories"
          ( A.info (parserJsStories <**> A.helper)
          $ A.progDesc "Generate JS code to render Storybook stories."
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

parserAnchorBlue :: A.Parser Command
parserAnchorBlue = pure GenerateAnchorBlue

parserAnchorBlack :: A.Parser Command
parserAnchorBlack = pure GenerateAnchorBlack

parserNav :: A.Parser Command
parserNav = pure GenerateNav

parserBannerGreen :: A.Parser Command
parserBannerGreen = pure GenerateBannerGreen

parserBannerRed :: A.Parser Command
parserBannerRed = pure GenerateBannerRed

parserBannerYellow :: A.Parser Command
parserBannerYellow = pure GenerateBannerYellow

parserButtonPrimary :: A.Parser Command
parserButtonPrimary = pure GenerateButtonPrimary

parserButtonPrimaryDisabled :: A.Parser Command
parserButtonPrimaryDisabled = pure GenerateButtonPrimaryDisabled

parserButtonSecondary :: A.Parser Command
parserButtonSecondary = pure GenerateButtonSecondary

parserButtonSecondaryDisabled :: A.Parser Command
parserButtonSecondaryDisabled = pure GenerateButtonSecondaryDisabled

parserButtonFullWidth :: A.Parser Command
parserButtonFullWidth = pure GenerateButtonFullWidth

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

parserListCategories :: A.Parser Command
parserListCategories = pure ListCategories

parserListStories :: A.Parser Command
parserListStories = pure ListStories

parserJsImportStories :: A.Parser Command
parserJsImportStories = pure JsImportStories

parserJsStories :: A.Parser Command
parserJsStories = pure JsStories
