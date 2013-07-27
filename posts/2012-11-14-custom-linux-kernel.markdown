----
title: Custom Linux Kernel
tags: linux
----

I compiled a custom version of the Linux kernel for my machine yesterday. It's based on kernel version 3.6.6-1 (Arch distro), and, like any custom kernel, includes a variety of tweaks customized for my hardware.

The [Arch Build System](https://wiki.archlinux.org/index.php/Arch_Build_System) makes it really simple to compile packages from source. It includes a tree of [PKGBUILDS](https://wiki.archlinux.org/index.php/PKGBUILD) that you can copy to your home directory (building as root is strongly unadvised) to compile software for your system.

Why would you want to do this? Mostly, it's for distribution and architecture-specific optimizations that are only available in packages compiled from source. In the case of the Linux kernel, there are literally hundreds of modules that go unused on my machine. For a pre-compiled kernel, it would scan through all of my hardware on every boot and check it against these modules to be sure that it shouldn't load them.

In addition, there are a number of cool tweaks in the config for people to play with.

I also [migrated to systemd](https://wiki.archlinux.org/index.php/Systemd) a few weeks ago. I remember a post or e-mail somewhere in which systemd caught a lot of flak for being untested, running against the tried-and-true sysvinit, but can't seem to find it. [Arch Linux just made systemd official](https://www.archlinux.org/news/end-of-initscripts-support/), and honestly, I trust the devs on their decision.

My boot time has nearly tripled, about which you'll hear no complaints from me.

Finally, I [switched from bash to zsh](http://en.wikipedia.org/wiki/Zsh). I haven't come close to taking full advantage of it yet, but the ability to tab-complete filenames has to be the best thing in the world.
