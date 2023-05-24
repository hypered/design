{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module is loaded only in the GHCi session.
-- It define example data and import modules (available qualified in GHCi too).
--
-- In particular, everything is loaded, then Main doesn't add new modules.
-- Main gives us the `:main` function.
-- We should be able to use `:main`, example data from this module, and
-- exported symbols from the (qualified) modules, e.g. `Fluid.step_0`.
module Hypered.Design.GHCi (
  ) where

import Protolude
import Hypered.Design.Fluid qualified as Fluid

-- This import is present to set -interactive-print in ghci.conf.
import Text.Pretty.Simple qualified
