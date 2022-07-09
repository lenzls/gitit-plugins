{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module InfoBox (plugin) where

-- This plugin adds a wikipedia style InfoBox

import Network.Gitit.Interface
import Data.Char (toLower)
import Data.Text (pack, unpack)

plugin :: Plugin
plugin = mkPageTransform transformBlock

transformBlock :: Block -> Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "infobox" `elem` classes =
    Table
        ("ttable", [], [])
        (Caption (Just [(Str "some caption")]) [])
        [(AlignLeft,ColWidth 20), (AlignLeft,ColWidthDefault)]
        (TableHead ("thead", [], []) [])
        [TableBody ("tbody1", [], []) (RowHeadColumns 2) [Row ("trow1", [], []) [
            Cell ("tcell1", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1")]]
        ]] []]
        (TableFoot ("tfoot", [], []) [])
transformBlock x = x
