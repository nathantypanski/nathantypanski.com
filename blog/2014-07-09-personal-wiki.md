----
title: Gitit as my personal wiki
tags: wiki, workflow
----

I've wanted a personal wiki for a while now. The problem is there wasn't any good software out there for it, or I couldn't find it.

My favorite production wiki software is MediaWiki. You've seen it, and probably even heard about it: it runs Wikipedia, the fantastic Arch Linux Wiki, and a billion other great wiki websites out there. But it's enterprise-grade wiki software, if you know what I mean. I've deployed it before with LDAP authentication. It runs on PHP. There's a SQL backend. I don't want that on my laptop.

[MoinMoin](http://moinmo.in/) is the runner-up in wiki software. [Kernel Newbies](http://kernelnewbies.org/) uses this. So do [Ubuntu](https://wiki.ubuntu.com/) and [Freedesktop.org](http://www.freedesktop.org/wiki/). It's written in Python, so I can hack at it without feeling like I'm playing with fire. But it uses Apache's webserver and the ugly common wiki markup that anyone who's worked with MediaWiki or similar are used to.

## What I want

1. Git backend
2. Markdown files
3. Web interface
4. Interlinked documents

Give me these four things and I can sync my wiki across multiple computers, edit it in plaintext or online, and roll out pages into blog posts with minimal effort. *That's* what I want. Even more ideally, I'd like to have [Pandoc](http://johnmacfarlane.net/pandoc/) in the backend so I can write pages in whatever format I'd like.

## Gitit

Oh wait. I've seen this before.

[Gitit] does all that. It's written in Haskell[^haskell] and pages only get updated when I commit them. I can write pages in LaTeX, markdown, or anything else supported by Pandoc. I get an API that I can mess with. The server backend is [Happstack](http://happstack.com/page/view-page-slug/1/happstack). Why haven't I been using this?[^features]

### Installing

I could've installed it with `cabal` like a good Haskeller, but it was in the Debian repos and I don't need it on multiple servers yet. So I did:

```{.bash}
$ sudo aptitude install gitit
$ mkdir ~/wiki && cd ~/wiki
$ gitit
```

and I had a wiki.

### Customizing

The first thing I didn't like was that Gitit doesn't run in the background, and it didn't run at launch. I stopped having a [systemd user config](https://wiki.archlinux.org/index.php/Systemd/User) (which I loved) when I moved to Debian, since even on sid the latest I can get is [systemd 204](https://packages.debian.org/sid/systemd). I'm not completely exactly sure what is going on under the hood with my systemd init in Debian, so I'm not about to compile 215 or something.

But I still want to daemonize this process. A quick Google search [rescued me](http://stackoverflow.com/questions/525247/how-do-i-daemonize-an-arbitrary-script-in-unix):

> You can daemonize any executable in Unix by using nohup and the & operator:
>
>    nohup yourScript.sh script args&

There's a script provided that uses pidfiles to ensure it only ran once. I feel like this is something I'll eventually hack into a set of personal scripts, so I whipped out the following:

```{.bash}
# Grep for a process name, excluding grep from the output.
function psgrep() {
    ps aux | grep $1 | grep -v grep
}

# Spawn a daemonized script ($1) only if it is not already started.
# If $2 is provided, this will run the daemon in that folder.
function daemonize_script () {
    if [[ -z $(psgrep $1) ]]; then
        if [[ -n $2 ]]; then
            olddir=$(pwd)
            cd $2
        fi
        nohup $1 > /dev/null 2>&1 &
        if [[ -n $2 ]]; then
            cd $olddir
        fi
    fi
}

daemonize_script gitit ~/wiki
```

This starts `gitit` once, when I start my computer, and then not again.

Just using `grep` like this doesn't work if you have other processes running that have that name in them. So this is pretty weak and not as useful as pidfiles, but it got the job done as a workaround.

Then I realized I didn't want `~/wiki` to have a bunch of gitit stuff in it - I just wanted the pages and Git repo. The repository is in there by default - but how to separate the Gitit data wasn't obvious.

My first shot was just to move Gitit to `~/.gitit` and symlink `~/.gitit/wikidata` to `~/wiki`, which worked when I ran it manually, but it broke my simple zsh script. To complicate things more, I also needed to start Gitit with a custom config file,[^configfile] which has to be passed as an argument like `gitit -f my.conf`. I *could* make this work with multiple arguments, but ...

Adding a `start_gitit` script to my `~/bin` folder solved this:

```{.bash}
#!/usr/bin/env bash
gitit -f $HOME/.gitit/my.conf
```

So now I'm calling

```{.bash}
daemonize_script ~/bin/start_gitit ~/.gitit
```

and that gets the job done. I'm looking forward to spending more time with my daemonizing scripts, since I know they're worthless as-is, but eventually might prove a useful thing to have around.

## Having a wiki

In my first couple of days with Gitit, I've started collecting useful bits of notes and categorizing them. It's nice because Pandoc's markdown (as opposed to, say, regular markdown) is powerful enough to give me everything I want for writing, and is overall simpler than [org-mode](http://orgmode.org/).

I made a lot of edits to the Arch Wiki last year, so I know that they're a format that I appreciate. Interlinking documents is an awesome feature.

As much as I love Org-mode, I'm still learning to cope with its complexity and it's nowhere near as portable as having a markdown wiki in my home folder.

## Moving forward

This is a new personal project for me, so there's still a lot of goals. If it becomes an ongoing effort of mine (I hope so!), then I should do the following:

- Write every day, whether it's blog posts or notes in my wiki.
    - Keeping a database of personal notes should open me up to writing just for myself. I don't do that enough as is, and it's pretty often I wish I had a record of my old unfinished thoughts so I could build on them and review them.
- <strike>Keep improving my daemonizing script.</strike> No longer necessary; I [switched to systemd user sessions](2014-07-25-all-your-daemons.html).
    - <strike>Stop grepping `ps aux`; use pidfiles or something normal.</strike>
- Find a nice way to link pages from my wiki to my website, so they're ultimately just the same thing.
    - This'll be hard, since they're both separate Git repos. Maybe using submodules can get the job done, or just writing a script to do the rollover when contents change?
- Start hacking at Gitit and learning its internals.
    - <strike>Move to cabal version instead of Debian package.</strike>

[^haskell]: Like Pandoc and [Hakyll](http://jaspervdj.be/hakyll/), which I use for this website.
[^features]: There was one feature it was missing when I tried it out last time. I can't remember it anymore. It must not have mattered much.
[^configfile]: There isn't a way to let anyone edit the wiki without a config file. I'm just hosting this locally for myself, so I have no need for a user account system and might eventually figure out a way to remove it.
