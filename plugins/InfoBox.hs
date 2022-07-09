{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module InfoBox (plugin) where

-- This plugin adds a wikipedia style InfoBox

import Network.Gitit.Interface
import Data.Char (toLower)
import Data.Text (pack, unpack)

plugin :: Plugin
plugin = mkPageTransform transformBlock

transformBlock :: Block -> Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "infobox" `elem` classes = Para [(Str "tests")]
transformBlock x = x
