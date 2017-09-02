# Bestiary

Bestiary is a mobile-friendly 5e compendium of creatures that organizes them in various classifications and strengths.

See the latest compiled build here: [http://johnsonra.github.io/bestiary/](http://chisaipete.github.io/bestiary/)

## Changelog
* 09/01/17: Forked from chisaipete, updated a few links in README.md (master) and index.html (ghpages), and added the R branch for an R package. See that branch for specific details on the R package.
* 03/21/16: Merged typo fixes and table markdown fix
* 03/09/16: Added hiding of tag headers during search
* 03/02/16: Added monsters from online supplements
* 02/27/16: Added monsters from the SRD

## To Do
* Filter by language in rencounter().
* Add and filter by terain type in rencounter().
* Add random monster generator based on instructions for creating your own monsters in the Dungeon Master's guide.

## Structure
Creatures can be found inside `_posts/`. Each creature gets its own post, written and stored as a [Markdown](http://daringfireball.net/projects/markdown/basics) file. The date is arbitrary and never displayed, but still necessary for [Jekyll](http://jekyllrb.com) to process the posts properly.

If you'd like to help fill out the rest of the creatures from the MM, for each new spell you make:

1. Make a new post inside `_posts/` for each new creature, and copy the formatting from another creature.
2. Submit a pull request for the creatures(s) when you're finished, and that's it! Thank you so much. :)

## Build Instructions
I've edited _config.yml for my own build purposes, but if you've got [Jekyll](http://jekyllrb.com) set up locally, the following should create the build from your friendly command line terminal:
`jekyll serve -Vw --no-watch --baseurl ""`

## Thanks

Shoutout to Saph of http://ephe.github.io/grimoire from whom I've shamelessly borrowed the majority of the codebase
