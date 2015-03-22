---
title: About this site
tags: info
---

The code for this site is [on GitHub](https://github.com/nathantypanski/nathantypanski.com). It's written in [Hakyll](http://jaspervdj.be/hakyll/), a static site generator for Haskell.

## Compilation

I compile the site generator in a [cabal sandbox](http://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes) to isolate its dependencies from the rest of my system, by running `cabal build` at the shell. I run `./site watch` while I'm editing so I get a local preview, and then when I'm done I deploy via [rsync](https://en.wikipedia.org/wiki/Rsync).
