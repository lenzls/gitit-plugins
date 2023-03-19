This repository houses several gitit plugins.

## Plugins
- InfoBox — Adds a wikipedia style infobox
- ReplaceCool — Replaces every mention of `cool` with `yey`

## Usage
1. put plugins in ./plugins/ subfolder of your gitit working directory (from where you start the wiki)
1. add `infobox.css` to your `static/css` folder and import it in your `custom.css` with `@import url("infobox.css");`
1. add the plugins to your gitit config like this: `plugins: plugins/InfoBox.hs`

## Plugin Ideas
- Orphaned pages — Lists pages which are not linked from other pages (to include pages with categories could be a toggle)
- Network Graph — Shows a graph with pages as nodes and links as edges (to include category links could be a toggle)
- Mermaid support — Potentially client side support for [mermaid charts](https://mermaid.js.org)
