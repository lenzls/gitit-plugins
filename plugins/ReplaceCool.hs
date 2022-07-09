module ReplaceCool (plugin) where

-- This plugin replaces "cool" with "yey"

import Network.Gitit.Interface
import Data.Char (toLower)
import Data.Text (pack, unpack)

plugin :: Plugin
plugin = mkPageTransform replaceCool

replaceCool :: Inline -> Inline
replaceCool (Str x) | x == pack "cool"  = Str (pack "yey")
replaceCool x = x

