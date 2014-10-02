---
title: Worklog
---

These are my efforts to make the world better.

The point of this is to keep some sort of record of the things that I've helped out with online, and organizations I participate in, so that I can reference them later and don't forget about my efforts.

## Rust

[Rust](http://www.rust-lang.org/) is a systems programming language from Mozilla with a heavy community focus.

### Rust class

I took David Evans' [operating systems class](http://rust-class.org/) in Spring 2014 as an "open student", part of his project letting anyone worldwide register. Informally, I think I was the only open student to actually finish all of the projects. I collaborated on the last three remotely on teams with UVa students - who were actually taking the course for credit, and graded on the projects. I was on Google Hangouts [in one of the presentations](http://rust-class.org/presentations.html) for the final projects at the end, but didn't get the chance to talk (I'm not bothered).

Overall it was a wild experience. From using a completely new language to me, to doing systems and kernel programming for the first time, it's really one of the most positive experiences I've had programming.

#### Projects

Team projects are labeled explicitly. Others were completed solely by me.

* [ps0](https://github.com/nathantypanski/cs4414-ps0): getting familiar with Rust.
* [ps1](https://github.com/nathantypanski/cs4414-ps1): really simple webserver.
* [ps2](https://github.com/nathantypanski/cs4414-ps2): writing a shell.
* ps3 (currently private): parallelized webserver (*team project*).
* [ps4](https://github.com/lenary/cs4414-ps4): hacking [ironkernel](https://github.com/wbthomason/ironkernel) (*team project*).
* [final project](https://github.com/lenary/cs4414-project): [Raft](http://raftconsensus.github.io/) in Rust (*team project*). Incomplete.

It's worth noting that all of these were written to a very old version of rust (0.7 or so, slightly later for the more recent ones). Rust has changed very significantly since then; none of them even still compile.

### rust-lang/rust

The [Rust reference implementation](https://github.com/rust-lang/rust).

* [Dead code removal](https://github.com/rust-lang/rust/pull/14956)
* [Documentation code update](https://github.com/rust-lang/rust/pull/14992)
* [Add regression test for issue #10766](https://github.com/rust-lang/rust/pull/17061)
* [Add ICE regression test for issue #16218.](https://github.com/rust-lang/rust/pull/17062)
* [Add ICE regression test with unboxed closures](https://github.com/rust-lang/rust/pull/17093)

### Guidelines

The [Rust guidelines](https://github.com/rust-lang/rust-guidelines/) are Mozilla's official "[c]onventions, principles, patterns, and best practices for Rust code".

* [features/modules: clarification & updates ](https://github.com/rust-lang/rust-guidelines/commit/4c208773d48d7512302a0cf01380401edceea354)
* [features/modules: Expand modules section](https://github.com/rust-lang/rust-guidelines/commit/bdbf206894f86670511c50bebcc7cf8e340f8e41)
* [style/comments: fix typo](https://github.com/rust-lang/rust-guidelines/commit/efec52d0053b0241a6417424c5300b94c486fb68)
* [expand comments doc ](https://github.com/rust-lang/rust-guidelines/commit/e976db8bc739c7851e8e03970cc327c1a78b4617)
* [style/imports: add example for import ordering ](https://github.com/rust-lang/rust-guidelines/commit/11267f3b90372ae3b3d8ec7daa1030039a920048)
* [fix bulleted list in unit testing section](https://github.com/rust-lang/rust-guidelines/pull/32)

### quickcheck

BurntSushi's [quickcheck](https://github.com/BurntSushi/quickcheck) is a [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) implementation for Rust.

* [fix Travis CI and Cargo builds](https://github.com/BurntSushi/quickcheck/pull/17)

### bors

[bors](https://github.com/graydon/bors) is Mozilla's buildbot for Rust.

* [allow hyphens in usernames](https://github.com/graydon/bors/pull/39)

### rust-nightly-archlinux

A [rust-nightly-archlinux](https://github.com/michaelsproul/rust-nightly-archlinux/) is a [PKGBUILD](https://wiki.archlinux.org/index.php/PKGBUILD) generator for [Arch Linux](https://wiki.archlinux.org/index.php/Arch_Linux).

* [use argparse to parse cli arguments](https://github.com/michaelsproul/rust-nightly-archlinux/pull/2). Somewhat daft, but I improved the CLI interface for this script, since I use it sometimes and seeing manual argument parsing in Python bothers me.

### rust-url

* [README: fix typo](https://github.com/servo/rust-url/pull/23#event-159534491)

## Python

### Ranger

[Ranger](http://ranger.nongnu.org/) is a text-based file manager.

* [Fixed a crash](https://github.com/hut/ranger/pull/139) in previews of non-previewable files.

### python-mode

[python-mode](https://launchpad.net/python-mode/) is a [major mode for Emacs](https://answers.launchpad.net/python-mode/+question/250108).

* [Bug report regarding virtualenv](https://answers.launchpad.net/python-mode/+question/250108), with some code for a potential workaround. My code got improved by one of the contributors, and a real workaround is supplied further down.

### Pandas

[Pandas](https://github.com/pydata/pandas) is a Python data analysis library.

- [CI: fix typos in readme](https://github.com/pydata/pandas/pull/7831)

### tabview

Tabview is a CSV viewer written in Python with Vim-like keybindings.

- [Add `0` key for beginning-of-line](https://github.com/firecat53/tabview/pull/15)

### python-iwlib

iwlib is an interface to [wireless tools for Linux](http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/Tools.html). [python-iwlib](https://pypi.python.org/pypi/iwlib) is Python bindings to that interface, so you don't have to use C for everything.

- [Add Python 3 support](https://bitbucket.org/nathantypanski/python-iwlib/commits/branch/py3): awaiting feedback on [pull request](https://bitbucket.org/getoffmalawn/python-iwlib/pull-request/1/add-python3-support/diff) as of 2014-10-01.

## Haskell

### sandbox-move

- [bump max base version requirement to 4.8](https://github.com/philopon/sandbox-move/pull/1)

## Arch Linux

I make a [lot of wiki contributions](https://wiki.archlinux.org/index.php/Special:Contributions/Ndt) for Arch. Still, if I mention a page here, it's not to say "I singlehandedly wrote this" - wiki pages are community efforts, and no one person gets credit for them.

There were some cases where I made significant contributions to pages, and watched them grow into awesome resources where others could take over. One of the dynamics you see on wikis is that people don't help out with the stub pages, since they don't visit them without useful information. Having good information on a page attracts visitors, however, which in turn garners contributions from people who otherwise would never help out.

### DisplayLink

* When I got a USB DisplayLink monitor back in 2013, there weren't really any resources on how to set it up on a modern Linux machine. There were kernel drivers for it (`udlfb` and the - at the time - staging driver `udl`) but nothing on how to make them work with X. Supposedly Fedora could do it, but that turned out to be a dead end: nobody ever wrote something about it.
* I started [this forum thread](https://bbs.archlinux.org/viewtopic.php?id=164385) about DisplayLink, in hopes of some solutions. Once I figured out to do [`--setprovideroutputsource`](https://bbs.archlinux.org/viewtopic.php?pid=1298516#p1298516) we started to make progress.
* Over the course of this I filled out the [DisplayLink wiki page](https://wiki.archlinux.org/index.php/DisplayLink) with working information. It's now pretty well-maintained by others, and is currently (22/06/2014) the second result on Google for "linux displaylink".
* Still, in March 2014 Pluggable[^pluggable] [wrote](http://plugable.com/2014/03/06/displaylink-usb-2-0-graphics-adapters-on-linux-2014-edition):

    > Multi-monitor on Linux, especially with multiple graphics cards and USB graphics adapters, remains problematic. You can find many distros and configurations where it just won’t work. We’d recommend staying away unless you’re an advanced Linux user who is willing to play with different distros, install optional components and do hand configuration. Unfortunately, it’s just not plug and play yet today, as it is on Windows.

[^pluggable]: Authors of the first Displaylink linux drivers, I think.

### Security

I did a big overhaul of the [Arch Wiki page on security](https://wiki.archlinux.org/index.php/Security) in June--August 2013.

* The whole thing used to just be links, or lists of links to other pages. I rewrote nearly every section, did tons of research on Linux security (RHEL best practices, research papers on mandatory access control, etc.) and shaped it into an actual pleasant overview of security.

### iptables

I rewrote the then-dwindling ArchWiki page for [iptables](https://wiki.archlinux.org/index.php/Iptables).

* Wrote an actual tutorial. This was the one thing the page desperately needed - a no-frills walkthrough on how to use it. It's still my #1 reference when I need to hack at some rules, and I only read things like this [crazy in-depth guide](https://www.frozentux.net/iptables-tutorial/iptables-tutorial.html) if I *really* have to.
* I spent a lot of time poring over the verbage in the "basic concepts" section. For these things to be useful, the language had to be *exact*, providing a quick overview of what exactly these words like "chains", "tables", and "rules" meant.
* Now the 8th link on Google for `iptables`, just below Debian's [much more spartan competition](https://wiki.debian.org/iptables).

### Packages

Occasionally, I find a package isn't available in either the Arch standard repositories or the AUR. When this is the case, I package it rather than just installing it directly (perhaps from the Makefile). This helps my system stay clean, and it saves time for anyone who repeats the work after me.

#### global-pygments plugin

* [Generate test/ctags_stub.py with automake](https://github.com/yoshizow/global-pygments-plugin/pull/3#event-159658469)
* Maintainer for [AUR package](https://aur.archlinux.org/packages/global-pygments-plugin-git/) (Arch Linux)

#### stmd-git

* [AUR Package](https://aur.archlinux.org/packages/stmd-git/) [GitHub](https://github.com/nathantypanski/archlinux-stmd-git)

## My projects

### This website

See [About this site](website.html).

### CNULUG

*Summer 2013 - present*. [Website](http://cnulug.org/).

I founded the Linux user group at [CNU](http://www.cnu.edu/pcs/academics/ce.asp). It's still getting on its feet, but I run the mailing lists and do a static site for it. We had weekly meetings all through Spring 2014.

### Dotfiles

*Fall 2012*. [GitHub](https://github.com/nathantypanski/dotfiles).

One of the cool things Linux users do is they post their user-level configuration files on the internet for other people to grab. We often have useful scripts, vim configurations, and creative ways of setting our environment variables tucked away in our dotfiles for nobody to see. Now, with sites like GitHub, you can search the vast repositories of user configuration for just the settings you need.

I post mine, too. There's a short writeup with some extra photos about my most recent configuration [over at Imgur](http://imgur.com/a/vPhbb).

### Math notebook

*Spring 2011*. [Webpage](/tex.html). [GitHub](https://github.com/nathantypanski/texnotes).

Once college-level integral calculus hit me I started searching for a better solution to managing my notes. One of the nice things about computerized documents is that you can search them, but math always seemed out of that domain. [LaTeX](http://en.wikipedia.org/wiki/LaTeX) turned out to be the solution to that problem - I started learning it early into my second semester of college, and ended up writing over 200 pages of notes. Some on discrete math, some on calculus, and a little bit of random tidbits thrown in there. It covers more than just that one class, but the bulk of describes tricky integration, sequences, and series - standard Calc II material.

It's something I'm really proud of, and I still reference them whenever I need a bit of that math knowledge.

The experience as a whole gave me the chance to explore mathematics a little deeper, and to gain some grasp on technical writing. I haven't feared any technical writing projects since then, and I gained an awesome new skill (technical typesetting) for any time I've had to write papers.

### SmartsGarden Visualizer

*Spring 2011*. [GitHub](https://github.com/nathantypanski/SGVisualizer).

I joined a capstone project to build generative music and artwork from network data. It was a team project with two others (although we each built specialized components) and my first serious effort using Git.

I made the music visualizer. Even though it was generated from music, and not visualizing live network data, the results had to *look* like they were coming from a network. I experimented with creating nodes onscreen and drawing them like network graphs, then got them working in resposne to thresholds from a [FFT](http://en.wikipedia.org/wiki/Fast_Fourier_transform) of the audio.

We won [CNU](http://cnu.edu/pcs)'s information systems capstone competition that year.
The result was visually impressive, and the others had produced good music from their traffic analysis. My code is written in [Processing](https://www.processing.org/), a sort of Java-lite for the computer graphics and artwork community. As far as I know, only the visualizer component is open-source.

### ZombieRL

*2011*. [GitHub](https://github.com/nathantypanski/zombierl).

As a senior in high school I got the chance to take my fourth and final computer science class as effectively an independent study. It was taught in C++, but I blew through the course material and started exploring other languages and projects to work on. I did the first couple chapters of [SICP](http://mitpress.mit.edu/sicp/), which I have yet to get back to, and picked up Python in my free time.

The culmination of all this effort was a zombie-themed roguelike in Python. It uses [The Doryen Library](http://doryen.eptalys.net/) to draw graphics to the screen. After working on it for a while I ported it to Python 3, but it hasn't seen much activity since then.

This was my first real intro to ideas like blitting graphics to a screen or having a main loop that had to respond to user input. It was also probably my first project where version control would have helped me; unfortunately I didn't discover Git until college.

### MGMT

*2010*. [GitHub](https://github.com/nathantypanski/MGMT).

I took [IB Computer Science](http://www.ibo.org/diploma/curriculum/group5/ComputerScience.cfm) as a Junior in high school. This was my "dossier," a massive final project that makes up half of the curriculum.

It managed customer contacts, but only had a CLI interface. The real beauty of the project was that - unlike most pedagogical software projects - it forced you to go through a sort of software engineering life cycle that lasted more than a week or two. I had to make diagrams, describe the class interfaces and the way the objects interacted, and all that fun stuff before I could even get started *coding*.

## Other

### NASA

I've worked at NASA since September 2013. My big project has been standardizing this huge volume of data files, where a team and I have built an expert system that uses fuzzy matching against our knowledgebase to standardize (meta)data.

From my [Reddit post](http://www.reddit.com/r/cscareerquestions/comments/27umgk/anyone_here_work_in_machine_learning_big_data/ci4q5ws):

> Data scientists call this process [data wrangling](http://en.wikipedia.org/wiki/Data_wrangling), and my job is making it automatic.
>
> This means writing parsers for things that no sane person would write parsers for (a human-writable data "standard") and automatically sanitizing/cleaning that input data so normal people can actually write parsers for data that's in the standard format. Then I apply machine learning techniques to (fuzzy) match the metadata for those files to a database of known metadata, thereby automatically standardizing things like variable naming, descriptions, and notation used to write their units.
>
> I have yet to receive my undergrad degree in computer engineering. The main skills involved in my work are
>
> - a deep understanding of Python (my main language),
> - knowledge of parsers (compilers can teach you about this),
> - fuzzy matching algorithms.

* Hopefully will get GPL'd someday. I've been pushing for this, but it's not really in my control.
* We have a short conference paper in [ICAI](http://www.worldacademyofscience.org/worldcomp14/ws/conferences/icai14) discussing the work.

#### Git talk

I gave an hour-long "brown bag" talk about Git for one of NASA's departments (though some outsiders stumbled in as well). It was accompanied by a live demo with some branching/rebasing bits. You can find the slides [over here](http://nathantypanski.github.io/git-talk/).

We're building momentum and there's now plans for some teams to start migrating projects away from Subversion.

#### nasa/NTL-Lunar-Mapping-and-Modeling-Portal

* [README: cleanup botched merge text](https://github.com/nasa/NTL-Lunar-Mapping-and-Modeling-Portal/pull/1#event-165488297)

### LaTeX WikiBook

* Added a section on [color in mathematics](http://en.wikibooks.org/wiki/LaTeX/Mathematics#Color).

### CNU UAV Lab

I volunteer hosting the [mailing lists](http://lists.cnulug.org/mailman/listinfo/uav-lab_lists.cnulug.org) for CNU's unmanned aerial vehicle lab.

### CNU Math HPC project

CNU's math department is getting their feet wet with high-performance computing. I volunteer as the sysadmin for their server, a ProLiant SL250s Gen8. Mostly this means I installed RHEL on it, found drivers for the Tesla GPU, and configured a dev environment for CUDA and whatnot.

I'll hopefully be doing more wrt coding on it soon; my scratchpad [is on GitHub](https://github.com/nathantypanski/PDEs).
