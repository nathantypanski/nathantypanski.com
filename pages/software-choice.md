---
title: Software Choices
---

## Philosophy

I have a certain philosophy to the software that I use. I strive to do more than just describe one program as "better" or "worse" than another, but to actually analyze the details behind those gut feelings:

- What makes me prefer one program over another?
- Why would I describe something as "better"? What is the underlying philosophy or design goal that makes me like the software?

As such, I try to codify my values into a coherent view of the software world. I do this not only so that I *know what I want*, but also how to build things that I would like to use.

### Freedom

I prefer to use software that gives me [freedom](https://en.wikipedia.org/wiki/Free_software), because I care about having ownership of the tools I use every day. Occasionally I [fix things](worklog.html) and send patches, and I think that process is important in fostering a community among software developers.

Besides, I wish to own my software in the same sense that a writer owns her typewriter. If it breaks, she will fix it. She understands the mechanics of the machine, so that she can diagnose problems and fix them. There are no laws governing whether or not she may disassemble the typewriter. These are fundamental properties of ownership of everyday objects: they afford that a sufficiently adept individual may master her tools.

### Simplicity

In alignment with the Arch Linux philosophy of [simplicty](https://wiki.archlinux.org/index.php/The_Arch_Way#Simplicity), I prefer software that I can understand. The more open and obvious your software is, the more I appreciate it for what it does. Simple things connect well with other pieces of software - that's the basic philosophy of the GNU core toolkit, and it applies to most other software you may find.

As such, I try to avoid a desktop environment like Gnome or KDE. I find they're not really necessary for me to use my computer - I spend most of my time hopping between terminals, a text editor, and a web browser anyway. I need software that lets me do that as efficiently as possible.

A corollary to the fact that simple things connect well with other software is that simple software is more easily scriptable. People accustomed to writing software understand that automating tedious tasks is one of the reasons it is a useful skill. Think of [Git](http://git-scm.com/) and all the wonderful tools people have built on top of it. The reason it is so easy to build things on top of Git is the fundamental simplicity of its data model and the programs that talk to it. The language might be difficult at first, yes, but from a technical standpoint Git is conceptually simple, both in design and execution.

### Efficiency

I prefer software that is fast. Of course, *ceteris paribus*, everyone will agree with me on that. But there are many tradeoffs made in software development, and among the most important decisions is how you manage performance.

This doesn't just mean software should be fast to run. Maybe it should be fast for the user, or more efficient to use, at the cost of being somewhat less performant.

Prof. Charles Leiserson said in a [lecture on algorithms](https://www.youtube.com/watch?v=JPyuH4qXLZ0) at MIT:[^leiserson]

> Performance is like money. It's like currency. So you say, "what good does a stack of hundred dollar bills do for you? Wouldn't you rather have food, or water, or shelter, or whatever?" And you're willing to pay those hundred-dollar bills for that commodity, even though water is far more important for your living. Similarly, performance is what you pay for user-friendliness. It's what you pay for security. You hear people say "I want greater functionality" - so people will program in Java, even though it's much slower than C. And they'll say "it costs me maybe a factor of 3 or something to program in Java, but Java's worth it because it's got all these object-oriented features and so forth, exception mechanisms, and so on." And people are willing to pay a factor of 3 in performance. So that's why you want performance: because you can use it to pay for these other things that you want.

I prefer software that makes smart decisions about this time tradeoff. Oftentimes, the right way to go for me as a user is to just write software that is fast and simple, and then I will make it do what I need.

## Tools

### Operating system

I use Linux exclusively. It caters better to developers and tinkerers than the alternatives.

- **[Arch Linux]((https://www.archlinux.org/))**: Currently Arch Linux is my main OS.
- [Debian](https://www.debian.org/): I used Debian, with a blend of Sid/testing, from January-June 2014.

### Text editors

I use both Emacs and Vim, which is an odd combination to hear someone mention.

- **[Emacs](http://www.gnu.org/software/emacs/)**: I have a post about my Emacs configuration [here](blog/2014-07-02-switching-to-emacs.html).
- [Vim](http://www.vim.org/): I started using Vim in early 2012, long before Emacs. I still use it to edit configuration files and make one-off edits, though my primary development has switched to Emacs for its greater featureset.
- [vi](https://en.wikipedia.org/wiki/Vi): used when I edit files as `root`, or when I'm installing Arch. Nowhere else.

### File manager

- [Ranger](http://ranger.nongnu.org/) almost exclusively, but plain shell more often than that. I have a [post about it](blog/2013-08-12-ranger.html).
- On very rare occasions, I will open [Thunar](https://en.wikipedia.org/wiki/Thunar) to satisfy Firefox's "Open containing folder" button in the download list.

### Window manager

- Current: **[Xmonad](http://xmonad.org/)**. Great tiling window manager. Written in [Haskell](http://www.haskell.org/haskellwiki/Haskell).
- [bspwm](https://github.com/baskerville/bspwm). Nicer tiling features than Xmonad. Harder to customize: I find Haskell easier to work with than C.

### Email

- **[mutt](http://www.mutt.org/)** with [isync](http://isync.sourceforge.net/) (a.k.a. `mbsync`) for offline IMAP.

### Terminal

- **[rxvt-unicode](https://en.wikipedia.org/wiki/Rxvt-unicode)**.
- [Termite](https://github.com/thestinger/termite) if I care less about resource usage and more about user friendliness.

#### Multiplexer

- **[tmux](http://tmux.sourceforge.net/)**.

### irc

- **[Weechat](http://weechat.org/)**. I'm in the minority among IRC users: I think [irssi](http://www.irssi.org/) is too buggy and unreasonably convoluted.

### shell

- **[zsh](http://www.zsh.org/)**. Preferable to Bash if you want a modern user interface in your terminal. I still stick to Bash for scripting, of course.

### Document generation

- **[Pandoc](http://johnmacfarlane.net/pandoc/)** generally, with documents written in markdown.
- **[LaTeX](http://www.latex-project.org/)** for printed works, or things heavy in math or references.

### Document viewers

- **[Zathura](http://pwmt.org/projects/zathura/)** is the best PDF viewer I've found. It has Vim keybindings by default, is extremely lightweight, and supports multiple backends.

### Productivity

- **[Anki](http://ankisrs.net/)** ([notes](/notetaking.html#memory))

### Web

#### Browser

- **[Firefox](https://www.mozilla.org)**.
- [dwb](http://portix.bitbucket.org/dwb/)
- [uzbl](http://www.uzbl.org/)

#### Publishing

- **[Hakyll](http://jaspervdj.be/hakyll/)**: see [About this site](/about.html)
- **[SCSS](http://sass-lang.com/)**: CSS is painful to write. I automate it a bit with something modern.

[^leiserson]: I transcribed more of it than just the quote here. The introduction to that part was:

    > Ok, so then you can say "why bother" and "why study algorithms and performance?" Okay, if it's at the bottom of the heap. Almost always people would rather have these other things than performance. You go off and you say to someone "would you rather have performance or more user-friendliness" and "more user-friendliness" is almost always more important than performance.
    >
    > Why do we care? Sometimes performance is correlated with user-friendliness. Nothing is more frustrating than sitting there waiting.
    >
    > > [the class offers suggestions]
    >
    > So there's several reasons that I think are important. One is that, often, performance measures the line between the feasible and the infeasible. So we heard some of these things: when there are real-time requirements, if it's not fast enough, it's simply not functional. Or if it uses too much memory, it's simply not going to work for you. So what you find is that often times algorithms are on the cutting edge of entrepreneurship. If you're talking about just reimplmenting things that other people did ten years ago, performance isn't that important at some level. But if you're talking about something nobody has done before, often the reason they haven't done it is it's too time-consuming, things don't scale, and so forth.
