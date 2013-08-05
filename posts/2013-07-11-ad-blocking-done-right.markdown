---
title: Ad blocking done right
tags: privacy, privoxy, proxy
---

If you're like me, you've probably been running [adblock plus](https://adblockplus.org/en/firefox) in Firefox for years. It's not because you don't want to support the sites you visit: maybe you specifically unblock domains that you know use unintrusive ads, like [reddit](www.reddit.com). Rather, it is usually the implementation of most web ads that is problematic. Maybe they:

- Are flash and play sound
- Take up screen real-estate
- Slow down your internet connection
- Set hundreds of cookies and track your browsing
- Slow your web browser, eating CPU cycles

Regardless of the reason, Adblock extensions to the web browser are a reasonable first step toward speeding up and maintaining control of your browsing experience. [Ghostery](http://www.ghostery.com/) and [NoScript](http://noscript.net/) are two other fine extensions that take further steps toward this goal.

Why Adblock Plus isn't good enough
------

Adblock Plus isn't perfect. I don't just mean its blocking abilities; the filter lists are very good. The problem is we are filtering content at our web browser. This impacts Firefox's overall responsiveness, it is detectable (Adblock doesn't replace ads, only removes them), and it is not very cross-browser. My Adblock Plus on Firefox and Adblock Plus on Chromium are not synchronized. If I block some content on one, I must either devise a system for them to automatically share filter lists (a waste of time) or manually synchronize them (a bigger waste of time).

It also is antithetical to what a web browser *should* be doing: browsing the web. Not filtering content. Filtering is simply not the job of the browser.

Moreover, I cannot implement Adblock Plus in any sort of gateway format. Maybe this doesn't matter to you, but it impacts one of my future projects: my plan is to eventually use a powerful single-board computer in conjunction with a pair of Ethernet switches, [openvpn](http://openvpn.net/), [DNSMasq](http://www.thekelleys.org.uk/dnsmasq/doc.html), and [iptables](http://netfilter.org/) to create a powerful, centralized filtering service for my network traffic. This lets me offload filtering services from my main computer, which needs its CPU power to compile things and [crunch numbers with mathematica](http://www.wolfram.com/mathematica/), not filter, secure, and log network traffic.

This is why Adblock Plus on Android is implemented as a local proxy and not any other design choice. It is simply more powerful that way, and it means Adblock does not care what the web browsers are doing, and the web browser does not care about the logic of the content filtering service.

A better adblock
------

I use a localized [privoxy](http://www.privoxy.org/) for my filtering. It is customizable and uses powerful regex-based filtering like Adblock does, but it is wholly separated from the guts of your web browsers. This means you can share the filter lists between browsers, choose which browsers use it and which don't on the fly, use the same popular filter lists from Adblock Plus, and much, much more.

All content that enters your web browser is now at the mercy of your filtering software. This includes cookies, HTTP headers, and even DNS requests if you're willing to go the [SOCKS](http://en.wikipedia.org/wiki/SOCKS) route.

Privoxy is completely customizable, in all of these areas, from both simple and well-documented configuration files or a web interface that is disabled by default. It offers multiple levels of default configurations, from basic, very safe settings to filtering-rich configuration to all-out complete control over HTTP behavior.

To use it, you just start the service, then change your browser's proxy settings to point HTTP and HTTPS traffic to

````{.bash}
localhost:8118
````

Then you will want to edit the configuration files. Then, to configure blocking, you can use [privoxy-blocklist.sh](http://andrwe.org/scripting/bash/privoxy-blocklist) and set it as a cronjob if you want regular updates. Otherwise, Privoxy provides great defaults at the medium level of filtering, and user customizations can be added in the file

````{.bash}
/etc/privoxy/user.action
````

in the form of regex + actions as documented in the default action file. Ideas can be found in the various adblocking filterlists out there, but the final decisions are yours.

This is the method I use, since it forces me to regularly spend time in the privoxy configurations, tweaking and improving the more advanced features that are provided. I suggest playing around with [Panopticlick](https://panopticlick.eff.org/) and [wireshark](http://www.wireshark.org/) to get an all-around idea of how your browser is behaving, then adjusting the privoxy settings to taste.

 If you are not willing to tinker with this software, then the improvement gained over Adblock Plus is worthwhile but minimal. But for the power user or system administrator, the difference is huge.
