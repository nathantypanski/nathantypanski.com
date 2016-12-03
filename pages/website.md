---
title: About this site
tags: info
---

The code for this site is [on GitHub](https://github.com/nathantypanski/nathantypanski.com). It's written in [Hakyll](http://jaspervdj.be/hakyll/), a static site generator for Haskell.

## Compilation

I compile the site generator in a [cabal sandbox](http://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes) to isolate its dependencies from the rest of my system, by running `cabal build` at the shell. I run `./site watch` while I'm editing so I get a local preview, and then when I'm done I deploy via [rsync](https://en.wikipedia.org/wiki/Rsync). I use a script like the following to deploy:

```{.bash}
#!/bin/bash

git checkout master &&\
git push&&\
./site clean &&\
./site build &&\
rsync --checksum -ave 'ssh' \_site/* [redacted]:/srv/http/nathantypanski.com
```

This makes the website really easy to maintain. It is just a bunch of markdown files that generate a bunch of HTML files that can be served as a static site. No backing database, no content management system, just a bunch of files on a computer.
