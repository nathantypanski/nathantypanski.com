---
title: Returning to i3
---

After nearly a month using [xmonad](http://xmonad.org/), I'm back to using the [i3 window manager](http://i3wm.org/). Still, this is the longest stretch of tiling wm usage I have ever managed. It might be that I have finally migrated from [openbox](http://openbox.org/), which is by far my all-time favorite floating window manager.

What I liked about xmonad
----

- Fully customizable in ways you wouldn't even think a window manager could manage.
- Huge, excellent [community extensions library](http://xmonad.org/xmonad-docs/xmonad-contrib/).
- Reminiscent of [dwm](http://dwm.suckless.org/), the code is your configuration.

What I didn't like about xmonad
----

- I still don't know Haskell.

I've been reading books. Everything from [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) to the oft-recommended [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/). And while I managed to [develop my xmonad.hs to something I was happy with](https://github.com/nathantypanski/dotfiles/blob/master/.xmonad/xmonad.hs), it still didn't come close to meeting the built-in features of i3.

Besides, I had already invested my time in [writing a dzen script for a statusbar](https://github.com/nathantypanski/dotfiles/blob/master/bin/dzen/dz.sh) and [tricking out my i3 config](https://github.com/nathantypanski/dotfiles/blob/master/.i3/config). While I had thought that i3 lacked features and xmonad would offer greater customization, the reality is that xmonad offers greater power for *people who already know Haskell*. If, like me, you don't know Haskell, then i3 is arguably superior. It is written in C, which most Linux people can be expected to be familiar with. Its configs are [easy and human-readable from the get-go](https://www.youtube.com/watch?v=QnYN2CTb1hM#t=15m30s).

Using xmonad made me better appreciate i3.

When I switched back today, I noticed that the Arch Wiki page had an interesting link to [i3-wm-scripts](https://github.com/yiuin/i3-wm-scripts), a set of Python scripts for manipulating i3. They add vim-esque mark and goto functions to i3, as well as window searching by name (with dmenu) and similar awesome features. Installation was simple: I just cloned the scripts into my *~/bin* directory and added keybindings from the *README*.

It's a bittersweet reunion. I really, *really* wanted to like xmonad. But the learning curve for programming it just proved too high.
