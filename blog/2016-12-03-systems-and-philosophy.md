----
title: Systems and philosophy
tags: Linux, zsh, Bash, free software
----

When I started computing, I was heavy into customization. I wrote [Powerusers](/blog/2012-06-03-powerusers.html), which was about the intense customizations I had done to personalize my Arch Linux distribution. It felt sort of juvenile to spend that much time [ricing](https://fun.irq.dk/funroll-loops.org/) my Linux distro. But it was fun. Linux and open-source were my hobbies; I did them in my free time and studied computer engineering as my main gig.

At the time, I only really managed two Linux systems: my laptop and this webserver. So it was easy to get away with hacking the hell out of my own system, since I knew it well, and refusing to automate tasks like my webserver configuration because I ran it in more of a fire-and-forget style. Put [nginx](https://www.nginx.com/resources/wiki/) in a [chroot](https://wiki.archlinux.org/index.php/change_root) once and you're done.

Now I'm getting older, I manage Linux systems professionally, and my philosophies on customizaiton, among many other things, are starting to change. Here's an example: my zsh config has [`setopt vi`](https://github.com/nathantypanski/dotfiles/blob/15900ce524a20b117ba950eaf9dbf03d3c263849/zsh/.config/zsh/settings.zsh#L64) in it. This means that on my local system, I have vi key bindings in my shell. But when I SSH into a remote host, it's more likely than not that my shell will be Bash. It's possible that the system maintainer hasn't even installed zsh on it, or maybe won't install it. But I can predictably, always, find Bash. Now I'm considering removing the vi bindings, or scrapping zsh entirely and moving back to Bash, just because that's what I find on every single system that I encounter.[^bourne]

I can't context-switch from vi to Emacs-like Bash keybindings very often. It gets quite confusing.

> - What shell do you use?
> - zsh.
> - What? Just use Bash, dude.
> - Why?
> - Just use Bash.

## Free software backoff

What else has changed? Well, for a long time I was a very public advocate of using only [free software](https://www.gnu.org/philosophy/free-sw.en.html), at least to the fullest extent that I could manage. I wrote all my school papers in [LaTeX](https://www.latex-project.org/), which was the subject of some scorn from my professors when I could not submit `.doc` or `.docx` files. I still use LaTeX for some of my personal correspondence, but now I'm back to using Google Docs or [Confluence](https://www.atlassian.com/software/confluence) for half of my things. It can be handy, sometimes, to have the editor get out of the way, and sometimes the benefit of collaborative work is worth the software philosophical tradeoff.

However, I will add that free software solutions to the problem of collaborative document editing, at least in the case of wikis, exist. You can use [Gitit](https://github.com/jgm/gitit), which gives you a Git-backed wiki with Pandoc for document markup. Sure, it doesn't have an LDAP connector, but it's a wonderful system to write in, and something like Markdown is relatively easier to learn than MediaWiki markup.

## Computer brand

Another switch: I use a Macbook. Not for my personal computing, but for my work stuff, ever since around September 2014. Getting used to OSX after spending something like 5 years running almost exclusively Arch Linux was a real change. But nearly every developer who I know in the industry uses OS X[^OSX], and sometimes I just have to play by other people's rules. I still put Arch on half of it, but to use that system I have to compile a [custom kernel](https://aur.archlinux.org/packages/linux-macbook/) with Apple-specific patches to work around [Bug 103211](https://bugzilla.kernel.org/show_bug.cgi?id=103211). This takes an annoying amount of effort when I have more important things to be doing than customizing my Linux distro.

Besides: when there's an emergency, and I happen to be stranded in a coffee shop and need to access the internet immediately, not having to fumble around with [netctl](https://wiki.archlinux.org/index.php/netctl) to access WiFi comes in handy. So, in those situations, I use OS X.

## Programming languages

The other thing that has happened is my languages of choice have become a bit less dogmatic. I will probably always write Python, because it is ubiquitous, easy to write and read, and I know it quite well. But I've since [picked up Ruby](/blog/2015-05-17-ruby.html), worked on a Rails application, and now I'm a bit less dogmatic about languages in general. I'm no longer out there trying to get everyone to learn Haskell because strong static type systems and functional languages are just *so damn cool*. I understand when people look at [Rust](https://www.rust-lang.org/en-US/) and tell me that high-level abstractions coupled with a serious type system and strong, compile-time guarantees of memory safety are not useful things to put in production or learn about (though I disagree with those people, and they probably enjoy writing Java).

## Conclusion

People who knew me in 2014-2015 might be convinced that I've sold out. I've reneged on my free software idealism, am slowly tending toward anti-customization, bought an iPhone, and am becoming complacent with my gradual absorption into the general tech monoculture. But in some ways, this is OK. I've picked up new skills, tried out new things, and I would bet that people now find me a bit easier to work with.

The other bit is that I've moved more seriously into the private industry. When you're just a student and researcher, it's easy to be dogmatic about Linux and weird programming languages - especially when you're broke and can't afford to pay for computer programs. And when your day-to-day activities are math and electronics classes, it's easy to go home and hack code in Haskell in your free time. But when you're hacking code all day and working hard with complex Linux systems, sometimes nothing beats just being able to go home at the end of it, chill out, and watch Netflix on a MacBook.

This isn't some giant announcement that everything I believed in college was wrong and proprietary software has no ethical issues. It's more of just an acknowledgement that life is fluid, and reality is dynamic and difficult. Sometimes this means making concessions. I am willing to do that, now, just as long as I learn some new things and can get the job done.

[^bourne]: If you're really hardcore, you could argue that [Bourne Shell](https://en.wikipedia.org/wiki/Bourne_shell) is the most ubiquitous, and therefore I should use that.
[^OSX]: Something I'm not too happy about, but it's the case.
