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
  ( properties
  , step_5
  , step_4
  , step_3
  , step_2
  , step_1
  , step_0
  , step_minus1
  , step_minus2
  , addStep
  , subStep
  , minorThird
  , perfectFour
  , space_2xs_xl
  , space_eccentric_large
  , space_eccentric_medium
  , space_eccentric_small
  , generateVWBasedValue
  , generateVWBasedValues
  ) where

import Protolude

--------------------------------------------------------------------------------
properties :: Double -> Text
properties remInPx = generateVWBasedValues remInPx
  [ ("space-2xs-xl", space_2xs_xl)
  , ("space-eccentric-large", space_eccentric_large)
  , ("space-eccentric-medium", space_eccentric_medium)
  , ("space-eccentric-small", space_eccentric_small)
  ]

--------------------------------------------------------------------------------
-- Type scale

step_0, step_1, step_2, step_3, step_4, step_5 :: Parameters

step_5 = addStep minorThird perfectFour step_4
step_4 = addStep minorThird perfectFour step_3
step_3 = addStep minorThird perfectFour step_2
step_2 = addStep minorThird perfectFour step_1
step_1 = addStep minorThird perfectFour step_0
step_0 = Parameters 320 1480 16 20
step_minus1 = subStep minorThird perfectFour step_0
step_minus2 = subStep minorThird perfectFour step_minus1

addStep atMin atMax params@Parameters {..} =
  params
    { minValue = minValue * atMin
    , maxValue = maxValue * atMax
    }

subStep atMin atMax params@Parameters {..} =
  params
    { minValue = minValue / atMin
    , maxValue = maxValue / atMax
    }

minorThird :: Double
minorThird = 1.2

perfectFour :: Double
perfectFour = 1.0 + 1.0 / 3.0

--------------------------------------------------------------------------------
space_2xs_xl :: Parameters
space_2xs_xl = Parameters 320 1240 18 60
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
-- Maybe it should be interpolated (i.e. double interpolation) with space_2xs_xl.
space_eccentric_large, space_eccentric_medium, space_eccentric_small :: Parameters
space_eccentric_large = Parameters (960 {- large -} + 18 + 18) 1240 18 160
space_eccentric_medium = space_eccentric_large { minWidth = 680 {- medium -} }
space_eccentric_small = space_eccentric_large { minWidth = 560 {- medium -} }

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
  "clamp(" <> show (toRem minValue) <> "rem , "
    <> show (toRem yIntersection) <> "rem + "
    <> show (slope * 100) <> "vw, "
    <> show (toRem maxValue) <> "rem);"
 where
  slope = (maxValue - minValue) / (maxWidth - minWidth)
  yIntersection = (-minWidth) * slope + minValue
  toRem = (/ remInPx)

generateVWBasedValues :: Double -> [(Text, Parameters)] -> Text
generateVWBasedValues remInPx paramss =
  unlines $ map f paramss
 where
  f (name, params) = "--" <> name <> ": " <> generateVWBasedValue remInPx params
