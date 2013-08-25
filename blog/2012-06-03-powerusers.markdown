---
title: Powerusers
tags: linux
---
<a href="http://i.imgur.com/RSwNK.png"><img src="http://i.imgur.com/RSwNKl.png" title="Hosted by imgur.com" alt="" /></a>

See that? That's one of my ten [Openbox 3](http://openbox.org/) desktops running [vim](http://www.vim.org/) fullscreen in [rxvt unicode](http://software.schmorp.de/pkg/rxvt-unicode.html) with the [Tmux terminal multiplexer](http://tmux.sourceforge.net/). It's styled using the [Solarized color scheme](http://ethanschoonover.com/solarized) by Ethan Schoover and uses the beautiful [Terminus](http://en.wikipedia.org/wiki/Terminus_%28typeface%29) typeface.

These are all things I care about. They are all things I've hand-picked for my [Arch Linux](http://www.archlinux.org/) system, which boots into an ugly low reolution terminal by default. I type `xinit` after the login prompt to start the GUI because this eliminates the overhead of a regular login manager.

Because of the stability this system offers, I don't have to reboot and type that very often. Maybe twice a month. When I do, my desktop looks something like this:

<a href="http://i.imgur.com/RaaV1.png"><img src="http://i.imgur.com/RaaV1l.png" title="Hosted by imgur.com" alt="" /></a>

Except without 1.53 GiB of RAM usage, a CPU core clocked up to 2.7 GHz, and WLAN connection info in my logfile.

I have ten virtual desktops, and they all serve different functions. I switch between them by hitting "Super" (the Windows key) and then a number (0-9).

- terminals
- coding
- web
- graphics
- 5
- 6
- 7
- files
- chat
- music

The function of most of these is obvious. 5, 6, and 7 are throwaway desktops for unconventional uses, like playing games or reading a pdf file.

The reason I'm organized this way is that on a modern system, with 8 GiB of RAM and a quad-core CPU, there's no reason to close and open programs every time I need to use them. I should be able to hit two keys and instantly be doing the thing that I want, whether it's graphics editing, web browsing, text editing, coding, irc or instant messenger chat, or anything else I do on a day to day basis.

When I'm not actively using one of these programs, my CPU clocks down from 2.7 GHz to 800 MHz to conserve battery usage and heat output. When I am using one, it clocks up the CPU cores, but only by as much and on as many cores as are necessary to do this.

When I'm busy, like during finals week last semester, my desktop looks like this:

<a href="http://i.imgur.com/nrCHR.png"><img src="http://i.imgur.com/nrCHRl.png" title="Hosted by imgur.com" alt="" /></a>

Yeah, that's my calendar. Those events all send custom notifications to my cell phone and desktop, with frequency proportional to their importance.

I'm a poweruser, and it's a cycle that gains and never decreases in complexity. Once you start tweaking a particular part of your system you never go back and think "oh, well, I suppose the defaults were `just good enough`." If they were good enough, you never would have changed them in the first place.

Some of these things are really boring, like configuration files that tell my computer exactly which programs to place on which desktops and how to scale them and when to run them.

Or [my ~/.conkyrc](http://pastebin.com/urgcZhem), for example.

One of these I'm really proud of. It's the [vincemod](http://www.nathantypanski.com/vincemod.html) theme for Openbox 3 I designed. It's a modification of [Dyne](http://box-look.org/content/show.php/Dyne?content=62000), a theme I loved, which makes some improvements upon the original to be more usable (while still maintaining the utter simplicity).

<a href="http://i.imgur.com/JtmdO.png"><img src="http://i.imgur.com/JtmdOl.png" title="Hosted by imgur.com" alt="" /></a>

Namely:

- 1px borders around menus
- text labels on inactive windows
- 4px borders around windows and 10px handles at the bottom of windows, for easy resizing (Dyne uses very thin borders, which is good for simplicity but makes drag-resizing unnecessarily difficult).
- Complete adoption of the [Solarized Dark](http://ethanschoonover.com/solarized) color scheme, to be easy on the eyes after long sessions of computer usage and integrate well into terminal windows (and the rest of my desktop).

There are endless more tweaks like this I've made to my desktop, and I will post about them in the future, but I felt an introduction to my poweruser habits necessary.
