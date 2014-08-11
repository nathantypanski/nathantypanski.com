----
title: All your daemons are belong to us
tags: linux
----

This January I [switched from Arch to Debian](2014-01-19-debian.html), and I was very happy with it. Now I'm back on Arch.

## Why go back?

After using it for six months, I *love* Debian. I finally understand what it's like to have a user-friendly operating system that is still powerful for the experts. But the problems I had with it were hit just as I started transitioning toward expert usage: on Debian, doing the advanced things is *harder* than doing them on Arch.

So I've gone back. And I love Arch just as much as ever, and appreciate it a bit more now that I've tried something else.

### systemd

Even though it was something I complained about in Arch, the one big thing I ended up missing was systemd. Arch makes systemd a first-class citizen. Debian has been in the middle of an official switch for months, but it still feels painful and not fully thought out. In retrospect, this makes sense: Arch has been on systemd for *years*, and Debian only announced systemd as the official init system [back in February](https://lists.debian.org/debian-ctte/2014/02/msg00405.html).

Of course, Debian has systemd. Jessie had the 204 release for most of the time I was using it. I'm back on Arch now, running systemd 215. As someone who uses my computer as a laptop and not, you know, a server, systemd user sessions are among the most useful features it has to offer.

Not long ago, I posted a [horrible script for daemonizing](2014-07-09-personal-wiki.html) my personal wiki. It barely worked. Sometimes it would cause `ps` to bug and report problems, which I was told to submit to the developers. Unfortunately, the line reported for causing the bug was the same line that was supposed to display information about the bug. I also have no idea where I'm supposed to report the bug - [here at SourceForge](http://sourceforge.net/p/procps-ng/tickets/?source=navbar)? Who knows.

In lieu of writing a better script, something that uses `mkdir` for PID locks,[^pidlock] I started to miss the sheer beauty of letting systemd manage my user-level init. To me, it's a simple fact that using shell scripts to spawn daemons for, say X startup is archaic, awful, and prone to errors.

On Arch, I can do this:

``` {.sourceCode}
$ userctl start gitit
```

and it spawns my wiki process, keeps logs, and monitors its status like any other daemon. I've aliased `userctl` to `systemctl --user`, since it's way easier to type and makes more sense in my head.

The script, located at `$HOME/.config/systemd/user/gitit.service`, looks like so:

``` {.sourceCode}
[Unit]
Description=my personal wiki

[Service]
Type=simple
WorkingDirectory=%h/.gitit
ExecStart=/home/nathan/.cabal/bin/gitit -f %h/.gitit/my.conf
Restart=no

[Install]
WantedBy=console.target
```

Let's break that down:

- `[Unit]`: generic information about the unit. Often contains dependency information - but not in this simple case. `systemd.unit (5)` has the details.
    - `Description`: name displayed in UI. For example, this is the name displayed when I type `systemctl status gitit`.[^systemctl-status]

- `[Service]`: Info about the process. See `systemd.service (5)` for the nitty-gritty.
    - `Type`: How the process gets started. `simple` means the process just runs, and it would block your terminal if you ran it at the shell and didn't background it. There's also `forking` for processes that call `fork()` during startup, `oneshot`, which starts and then dies quickly, and a few others that I won't describe here.
    - `WorkingDirectory`: The directory that the service starts in. I had configured Gitit to start in `$HOME/.gitit`, and in systemd `%h` is the variable for a user's home folder.
    - `ExecStart`: The command that launches the daemon (or program, in the case of `oneshot`). One quirk here: this has to be a fully qualified path, not just an executable or something you'd expect to find in `$PATH`.
    - `Restart`: Whether to restart the service on startup.
- `[Install]`: How to set up the dependencies and startup for a service.
    - `WantedBy`: The target that "wants" this service. Services in systemd are owned by *targets*, and when you enable or disable a target it will start the appropriate services with it. In this case, `console.target` is a service that starts every time I start a user session.

Once you have your daemons properly configured, systemd tracks *everything* they spawn and makes sure the process hierarchy matches that setup. There was a time, months ago, when I had my entire user session spawned by systemd - no bash scripts the whole way through. The `htop` tree output was *beautiful*: systemd tracks the parents of each process perfectly, and nothing escapes its watch and reparents to `1` like in traditional Linux systems.

And if I ever need more fine-grained control, [cgroups](http://en.wikipedia.org/wiki/Cgroups) have me covered.

### Understanding

The other core part about Arch is that its very nature means you understand *everything* that goes on in your system. Certain parts about Debian just make it completely non-obvious how they work. What is `update-alternatives` doing under the hood? How is `dpkg` managing its [configuration subsystem](https://www.debian.org/doc/debian-policy/ap-pkg-conffiles.html)? The details are complicated just to *use* these systems, and finding information on the implementation nuts-and-bolts is over my head. The Arch philosophy of simplicity is immediately apparent: *I can actually grok this system completely*.

Even on a Debian minimal install, it started a daemon when I installed *nethack*. Frigging *nethack*! Can't I make these decisions for myself?

There are situations where it's not worth my time to figure out how to get things working, and I just want the operating system to do everything for me. But there's an important balancing act whenever you're building an operating system, where you can either make things easy at first and in the default case, or make them easy for the general case (but harder by default). Arch leans toward "this system should be easy for a *competent user*".

### Arch Build System

When I finally started compiling my own software on Debian, I realized how much harder it is to make packages compared to Arch. If I need a package for something that isn't in the default repositories, I can usually spin one together in a few minutes (provided the software is easy to build). Debian packages are *complicated*, by contrast - capable of much more, perhaps, but raising the cost of creation.

Lowering the barrier to entry to the packaging system means I can have my entire system be first-class citizens to the package manager. That is *awesome*. I can't remember the last time I had an arch system where I just ran `sudo make install` on some software and dropped it all over `/usr/local`. It's easier to make a real package than to do that garbage.

### Arch Wiki

The [Arch Wiki](https://wiki.archlinux.org/) is fantastic. I tell people it's the best wiki in the Linux ecosystem. I'm pretty sure that I'm right about that one. Lots of people, including myself,[^delay] really work hard to keep it up to date on the latest software in the Linux world. Today I was able to get [nftables](https://wiki.archlinux.org/index.php/Nftables) up and running in a few minutes thanks to their documentation.

## Concerns

I'm no longer totally comfortable using Arch as my daily OS. I had far more trust in Debian and the fact that I wouldn't see upstream releases that caused serious breakages until they were actually stable for most people. Arch doesn't try to protect its users by withholding packages until they work perfectly with everything and don't break: if upstream pushes bad code, you get the bad code.

There's a degree of responsibility the user has to assume, then, to not let upstream break their system. But considering every distro can end up with broken software *anyhow*, it's not as bad as you'd think.

The Arch dev community also seems a bit more secluded than Debian's. With Debian, you can follow nearly all of the dev activity, and it's easy to watch and find. The Arch developers leave you with `arch-dev-public` and the occasional DeveloperWiki updates to keep the users up on their activity. I think this is mostly a result of the smaller size of the Arch community: the sheer size of Debian leads to a more open development process almost of necessity.

## Conclusions

I think I'll be happy with Arch. Everything I've seen so far has shown me that it's thoroughly alive and better than ever.

But I'm sure in a few months I'll have more to say. Maybe one day I'll put some real effort into Gentoo or one of the BSDs.

[^pidlock]: Using regular files as locks is a bad idea. The problem is that there is no way to build a [test and set](http://en.wikipedia.org/wiki/Test-and-set) with regular files - on Unix-like systems, you can't both *check if a file exists* and *create it if it doesn't exist* in an atomic manner. The `mkdir` command, on the other hand, has all the properties we need to build this concurrency primitive:

    - Failing and returning an exit status if the directory exists,
    - Creating the directory if it doesn't exist.

    The problem you can run into with regular files is that if the file *does* exist, when you try to spawn a new process and create a lockfile in that place, another process could have spawned in the time between when you deleted the lockfile and when you're writing the new one. This means your daemonizing system will eventually fail, and your system will be broken.

[^systemctl-status]: Example output:

    ``` {.sourceCode}
    $ userctl status gitit
    ● gitit.service - my personal wiki
       Loaded: loaded (/home/nathan/.config/systemd/user/gitit.service; enabled)
       Active: active (running) since Fri 2014-07-25 21:21:16 EDT; 13min ago
     Main PID: 4905 (gitit)
       CGroup: /user.slice/user-1000.slice/user@1000.service/gitit.service
               └─4905 /home/nathan/.cabal/bin/gitit -f /home/nathan/.gitit/my.conf

    Jul 25 21:21:16 dionysus systemd[388]: Started my personal wiki.
    ```
[^delay]: I made [16 contributions](https://wiki.archlinux.org/index.php/Special:Contributions/Ndt) even during the 6 months I used Debian primarily. I still had Arch on a VPS of mine, but that system doesn't see modification very often.
