----
title: Switching to Wayland
tags: wayland, xorg, linux
----

I converted my personal machine (a mid-2015 MacBook Pro running Linux) to Wayland over the past couple days.
Previously I used i3 as my window manager, and moving to Wayland requires a lot of little things to change in order for it to work reliably.

The main reasons to use Wayland are
[security](http://www.mupuf.org/blog/2014/02/19/wayland-compositors-why-and-how-to-handle/#2-improving-the-security-of-x)
and
[architectural simplicity](https://wayland.freedesktop.org/architecture.html).
It makes a few fundamental design choices that are superior to Xorg
