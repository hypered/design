-- | This generates simple (pre-computed, using just @clamp@) interpolated
-- spacing values depending on the viewport width. The result is an expression
-- that can be used in CSS rules.
-- This is based on utopia.fyi. The main difference is that we allow 1rem to be
-- 10px (instead of the default 16px).
-- (Instead of using pre-computed values, we can do everything directly in CSS
-- using larger expressions, with the advantage that this would expose CSS
-- custom properties.)
--
-- I think I'll use the following parameters:
-- minimum and maximum viewport width: 320-1480
-- minimum and maximum font size: 16-20
-- minimum and maximum type scale: 1.2-1.3333
-- Those numbers make the fluid grid using gutter width from M to L, and
-- maximum column width 2XL (with 12 columns) exactly 1480.
-- Other maximum width for the font size will create different spacing, which
-- will create different grids ending in a different maximum width.
-- This is probably quite abitrary (to want those match or not) but let's try
-- that.
-- utopia.fyi adds gutters to the left and right (so a total of 13 gutters for
-- 12 columns). With the above numbers, their width starts at 24px and end at
-- 40px.
module Hypered.Design.Fluid
  ( everything
  , properties
  , stepsC
  , space_3xs
  , space_2xs
  , space_xs
  , space_s
  , space_m
  , space_l
  , space_xl
  , space_2xl
  , space_3xl
  , space_m_l
  , grid
  , addStep
  , subStep
  , mulStep
  , minorThird
  , perfectFour
  , space_2xs_xl
  , space_eccentric_large
  , space_eccentric_medium
  , space_eccentric_small
  , generateVWBasedValue
  , generateVWBasedValues
  ) where

import Data.Text (pack)
import Protolude
import Text.Printf (printf)

--------------------------------------------------------------------------------
everything :: Text
everything =
  ":root {\n"
    <> generateSteps "step-a" 10.0 stepsA
    <> "}\n\n"
    <> ":root {\n"
    <> generateSteps "step-b" 10.0 stepsB
    <> "}\n\n"
    <> ":root {\n"
    <> generateSteps "step-c" 10.0 stepsC
    <> "}\n\n"
    <> ":root {\n"
    <> generateSteps "step-d" 10.0 stepsD
    <> "}\n\n"
    <> ":root {\n"
    <> properties 10.0
    <> "}\n\n"
    <> ":root {\n"
    <> generateVWBasedValues 10.0
        [ ("space-m-l", space_m_l)
        ]
    <> "}\n\n"
    <> grid

properties :: Double -> Text
properties remInPx = generateVWBasedValues remInPx
  [ ("space-3xs", space_3xs)
  , ("space-2xs", space_2xs)
  , ("space-xs", space_xs)
  , ("space-s", space_s)
  , ("space-m", space_m)
  , ("space-l", space_l)
  , ("space-xl", space_xl)
  , ("space-2xl", space_2xl)
  , ("space-3xl", space_3xl)
  ]

properties' :: Double -> Text
properties' remInPx = generateVWBasedValues remInPx
  [ ("space-2xs-xl", space_2xs_xl')
  , ("space-eccentric-large", space_eccentric_large)
  , ("space-eccentric-medium", space_eccentric_medium)
  , ("space-eccentric-small", space_eccentric_small)
  ]

--------------------------------------------------------------------------------
-- Type scale

-- Smaller headings for the application context, similar to the existing design.
stepsA :: Steps
stepsA = makeSteps' majorSecond majorSecond $ Parameters 320 1480 16 16

-- Normal font for the content context, similar to the existing design.
stepsB :: Steps
stepsB = makeSteps $ Parameters 320 1480 16 16

-- Quite large font.
stepsC :: Steps
stepsC = makeSteps $ Parameters 320 1480 16 20

-- Proportions remain the same (i.e. 10 * (1480 / 320) = 46.25).
-- This is quite big on large viewports, and quite tiny on small viewports.
stepsD :: Steps
stepsD = makeSteps $ Parameters 320 1480 10 46.25

makeSteps :: Parameters -> Steps
makeSteps = makeSteps' minorThird perfectFour

makeSteps' :: Double -> Double -> Parameters -> Steps
makeSteps' scaleMin scaleMax params = Steps {..}
 where
  step5 = addStep scaleMin scaleMax step4
  step4 = addStep scaleMin scaleMax step3
  step3 = addStep scaleMin scaleMax step2
  step2 = addStep scaleMin scaleMax step1
  step1 = addStep scaleMin scaleMax step0
  step0 = params
  stepMinus1 = subStep scaleMin scaleMax step0
  stepMinus2 = subStep scaleMin scaleMax stepMinus1

addStep :: Double -> Double -> Parameters -> Parameters
addStep atMin atMax params@Parameters {..} =
  params
    { minValue = minValue * atMin
    , maxValue = maxValue * atMax
    }

subStep :: Double -> Double -> Parameters -> Parameters
subStep atMin atMax params@Parameters {..} =
  params
    { minValue = minValue / atMin
    , maxValue = maxValue / atMax
    }

majorSecond :: Double
majorSecond = 1.125

minorThird :: Double
minorThird = 1.2

perfectFour :: Double
perfectFour = 1.0 + 1.0 / 3.0

--------------------------------------------------------------------------------
-- Space scale

space_3xs, space_2xs, space_xs, space_s, space_m, space_l, space_xl, space_2xl, space_3xl :: Parameters
space_3xs = mulStep 0.25 space_s
space_2xs = mulStep 0.5 space_s
space_xs = mulStep 0.75 space_s
space_s = step0 stepsC
space_m = mulStep 1.5 space_s
space_l = mulStep 2.0 space_s
space_xl = mulStep 3.0 space_s
space_2xl = mulStep 4.0 space_s
space_3xl = mulStep 6.0 space_s

-- One-up pairs

space_m_l :: Parameters
space_m_l = pair space_m space_l

-- Other pairs

space_2xs_xl :: Parameters
space_2xs_xl = pair space_2xs space_xl

mulStep :: Double -> Parameters -> Parameters
mulStep x params@Parameters {..} =
  params
    { minValue = minValue * x
    , maxValue = maxValue * x
    }

pair :: Parameters -> Parameters -> Parameters
pair p1 p2 = p1 { maxValue = maxValue p2 }

--------------------------------------------------------------------------------
-- Grid

grid :: Text
grid =
  ":root {\n\
  \  --grid-max-width: 148rem;\n\
  \  --grid-gutter: var(--space-m-l);\n\
  \  --grid-columns: 12;\n\
  \}\n\
  \\n\
  \.u-container {\n\
  \  max-width: var(--grid-max-width);\n\
  \  padding-inline: var(--grid-gutter);\n\
  \  margin-inline: auto;\n\
  \}\n\
  \\n\
  \.u-grid {\n\
  \  display: grid;\n\
  \  gap: var(--grid-gutter);\n\
  \}\n"

--------------------------------------------------------------------------------
space_2xs_xl' :: Parameters
space_2xs_xl' = Parameters 320 1240 18 60
-- TODO I probably want to use 1280 instead of 1240. Similarly I have to find
-- the "right" values for large, medium, and small below. Everything should probably
-- be ligned up on a grid columns.

-- This is a space that grows from a tiny margin (used with a .o-container)
-- to the margin necessary for .o-container--eccentric.
-- maximum left margin for the eccentric medium layout:
-- (1280 - 960 (large)) / 2 = 320 / 2 = 160
-- The minWidth depends on the container width, so that the minimum margin is
-- reached faster for larger container (so that its right side is not outside
-- the viewport).
-- TODO The rate of change (wrt. to the schrinking width) is not really great.
-- Maybe it should be interpolated (i.e. double interpolation) with space_2xs_xl'.
space_eccentric_large, space_eccentric_medium, space_eccentric_small :: Parameters
space_eccentric_large = Parameters (960 {- large -} + 18 + 18) 1240 18 160
space_eccentric_medium = space_eccentric_large { minWidth = 680 {- medium -} }
space_eccentric_small = space_eccentric_large { minWidth = 560 {- medium -} }

--------------------------------------------------------------------------------
-- | Different font sizes corresponding to h5 to two levels below p.
data Steps = Steps
  { step5 :: Parameters
  , step4 :: Parameters
  , step3 :: Parameters
  , step2 :: Parameters
  , step1 :: Parameters
  , step0 :: Parameters
  , stepMinus1 :: Parameters
  , stepMinus2 :: Parameters
  }

generateSteps :: Text -> Double -> Steps -> Text
generateSteps name remInPx Steps {..} = generateVWBasedValues remInPx
  [ (name <> "-5", step5)
  , (name <> "-4", step4)
  , (name <> "-3", step3)
  , (name <> "-2", step2)
  , (name <> "-1", step1)
  , (name <> "-0", step0)
  , (name <> "--1", stepMinus1)
  , (name <> "--2", stepMinus2)
  ]

--------------------------------------------------------------------------------
-- | The values we want to interpolate, and at which viewport widths the minimum
-- and maximum should be reached. Units are in px.
data Parameters = Parameters
  { minWidth :: Double
    -- ^ Minimum viewport width, at which point the 'minValue' should be reached.
  , maxWidth :: Double
    -- ^ Maximum viewport width, at which point the 'maxValue' should be reached.
  , minValue :: Double
    -- ^ The lower value to interpolate.
  , maxValue :: Double
    -- ^ The higher value to interpolate.
  }
  deriving Show

-- | See https://utopia.fyi/space/calculator and https://utopia.fyi/blog/clamp/
-- TODO Should I add calc() around the addition ?
generateVWBasedValue :: Double -> Parameters -> Text
generateVWBasedValue remInPx Parameters {..} =
  pack $ printf "clamp(%.4frem, %.4frem + %.4fvw, %.4frem);"
    (toRem minValue)
    (toRem yIntersection)
    (slope * 100)
    (toRem maxValue)
 where
  slope = (maxValue - minValue) / (maxWidth - minWidth)
  yIntersection = (-minWidth) * slope + minValue
  toRem = (/ remInPx)

generateVWBasedValues :: Double -> [(Text, Parameters)] -> Text
generateVWBasedValues remInPx paramss =
  unlines $ map f paramss
 where
  f (name, params) = "  --" <> name <> ": " <> generateVWBasedValue remInPx params
