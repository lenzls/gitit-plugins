{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module InfoBox (plugin) where

-- This plugin adds a wikipedia style InfoBox.
-- Use like this:
-- ~~~ {.infobox}
-- title=Terra Nova
-- imageURL= ./bla.png
-- imageCaption=Image of Terra nova
-- heading=Characteristics
-- field=Radius=40k
-- field=Inhabitants=40 million
-- heading=Atmosphere
-- field=Surface pressure=2000Pa
-- ~~~

import Network.Gitit.Interface
import Data.Char (toLower)
import Data.Text (pack, unpack)

plugin :: Plugin
plugin = mkPageTransform transformBlock

transformBlock :: Block -> Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "infobox" `elem` classes =
    Div ("test", [], []) (map (Para . (:[]) . Str . pack) (lines (unpack contents)))    

    -- return $ Table
    --     ("ttable", [], [])
    --     (Caption (Just [(Str "some caption")]) [])
    --     [(AlignLeft,ColWidth 20), (AlignLeft,ColWidthDefault)]
    --     (TableHead ("thead", [], []) [
    --         Row ("theadrow1", [], []) [
    --             Cell ("theadcell1", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "Header col 1")]]
    --         ]
    --     ])
    --     [TableBody ("tbody1", [], []) (RowHeadColumns 0) [
    --         Row ("trow1.1", [], []) [
    --             Cell ("tcell1.1a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.1a"), (Str "2nd content cell1.1")]]
    --         ],
    --         Row ("trow1.2", [], []) [
    --             Cell ("tcell1.2a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.2a")]],
    --             Cell ("tcell1.2b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.2b")]]
    --         ],
    --         Row ("trow1.3", [], []) [
    --             Cell ("tcell1.3a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.3a")]],
    --             Cell ("tcell1.3b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.3b")]]
    --         ]
    --     ] [
    --         Row ("trow2.1", [], []) [
    --             Cell ("tcell2.1a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.1a")]],
    --             Cell ("tcell2.1b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.1b")]]
    --         ],
    --         Row ("trow2.2", [], []) [
    --             Cell ("tcell2.2a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.2a")]],
    --             Cell ("tcell2.2b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.2b")]]
    --         ]
    --     ]]
    --     (TableFoot ("tfoot", [], []) [])
transformBlock x = x
