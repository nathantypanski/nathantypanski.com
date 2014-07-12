---
title: Worklog
---

These are my minor efforts to make the world better. Sometimes they're staging for things in my [projects list](/projects.html), other times they're just one-offs for projects I've helped out with.

The point of this is to keep some sort of record of the things that I've helped out with online, so that I can reference them later and don't forget about my efforts.

## CNULUG

I founded the [Linux user group](http://cnulug.org/) at [CNU](http://www.cnu.edu/pcs/academics/ce.asp). It's still getting on its feet, but I run the mailing lists and do a static site for it. We had weekly meetings all through Spring 2014.

## CNU UAV Lab

I volunteer hosting the [mailing lists](http://lists.cnulug.org/mailman/listinfo/uav-lab_lists.cnulug.org) for CNU's unmanned aerial vehicle lab.

## CNU Math HPC project

CNU's math department is getting their feet wet with high-performance computing. I volunteer as the sysadmin for their server, a ProLiant SL250s Gen8. Mostly this means I installed RHEL on it, found drivers for the Tesla GPU, and configured a dev environment for CUDA and whatnot.

I'll hopefully be doing more wrt coding on it soon; my scratchpad [is on GitHub](https://github.com/nathantypanski/PDEs).

## Rust class

I took David Evans' [operating systems class](http://rust-class.org/) in Spring 2014 as an "open student", part of his project letting anyone worldwide register. Informally, I think I was the only open student to actually finish all of the projects. I collaborated on the last three remotely on teams with UVa students - who were actually taking the course for credit, and graded on the projects. I was on Google Hangouts [in one of the presentations](http://rust-class.org/presentations.html) for the final projects at the end, but didn't get the chance to talk (I'm not bothered).

Overall it was a wild experience. From using a completely new language to me, to doing systems and kernel programming for the first time, it's really one of the most positive experiences I've had programming.

### Projects

* [ps0](https://github.com/nathantypanski/cs4414-ps0): getting familiar with Rust
* [ps1](https://github.com/nathantypanski/cs4414-ps1): really simple webserver
* [ps2](https://github.com/nathantypanski/cs4414-ps2): writing a shell
* ps3 (currently private): parallelized webserver
* [ps4](https://github.com/lenary/cs4414-ps4): hacking [ironkernel](https://github.com/wbthomason/ironkernel)
* [final project](https://github.com/lenary/cs4414-project): [Raft](http://raftconsensus.github.io/) in Rust

## Rust

I've been getting involved with [Rust](http://www.rust-lang.org/) development to help facilitate the move to 1.0. Mostly in the vein of helping with documentation, but I'm studying the compiler internals and familiarizing myself with the issues (~1500 at time of writing) as I look to get more involved.

## NASA

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

## Arch Linux

I made a lot of wiki contributions for Arch in 2013. I'm quite proud of them. If I mention a page here, then don't take it "I singlehandedly wrote this" - wiki pages are community efforts, and no one person gets credit for them.

There were some cases where I made significant contributions to pages, though, and watched them grow into awesome resources where others could take over. One of the dynamics you see on wikis is that people don't help out with the stub pages, since they don't visit them without useful information. Having good information on a page attracts visitors, however, which in turn garners contributions from people who otherwise would never help out.

The pages I mention here are listed because I saw them go from [bitrot](http://en.wikipedia.org/wiki/Software_rot) to useful as I worked on them, along with the efforts of everyone else who contributed.

### DisplayLink for Linux

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

## Ranger

[Ranger](http://ranger.nongnu.org/) is a text-based file manager.

* [Fixed a crash](https://github.com/hut/ranger/pull/139) in previews of non-previewable files.


## python-mode

`python-mode` is a [major mode for Emacs](https://answers.launchpad.net/python-mode/+question/250108).

* [Bug report regarding virtualenv](https://answers.launchpad.net/python-mode/+question/250108), with some code for a potential workaround. Definitely shouldn't make it into the codebase, however.

## LaTeX WikiBook

* Added a section on [color in mathematics](http://en.wikibooks.org/wiki/LaTeX/Mathematics#Color).
