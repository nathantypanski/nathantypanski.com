---
title: Genius programmers
tags: philosophy, personal
---

I haven't blogged in a few months. I've tried to write a number of times, and even sat down and thought I had a good idea and could write a blog post about it. Then I'd hammer out a few paragraphs, decide they're stupid, and end up not pushing them out to my webserver out of fear that people would think I seem dumb.

This process wasn't overt; it happened under the hood, and I kept diguising in my head and telling myself it was just writer's block. Then I read [Josh Davis' post](http://joshldavis.com/2014/06/13/put-yourself-out-there/) about putting yourself out there, and what I've been feeling was clear as day: I've just been isolating myself, and it's a totally separate process from having writer's block or not having ideas.

In his blog post, Josh summarizes a Google I/O talk titled [The Myth of the Genius Programmer](http://joshldavis.com/2014/06/13/put-yourself-out-there/). He boils it down to four core points:

> 1. Make yourself vulnerable.
> 2. Be open.
> 3. Communicate well.
> 4. Embrace criticism.

It occurs to me that this applies to more than just software. Nobody has ever achieved anything meaningful by *not* making themselves vulnerable. If you want to start a company, or write a novel, or just get in a habit of going to the gym every day, you need to make yourself vulnerable.

## Excuses for the wrong problem

I can't be the only one who has made excuses to myself like:

- "It's just writer's block."
- "I need to focus on *X*, so I can't do *Y* that I really wish I could do."
- "There are already experts on the topic, so surely nothing I output will be as worthwhile as what they're doing."

The problem with these excuses is twofold:

1. They each disguise the real problem by making you think you're making an excuse about something else.
2. They trick you into thinking you're spending your time wisely.

In each of these examples, it looks like the problem is actually something obvious: you're not feeling creative, or you're busy, or you're not experienced enough. These are all *temporary* obstructions, things you can overcome now that you recognize them. They're all things that might go away with time, or with a bit of practice, or an ease on you workload. But if the real problem is just that you're not making yourself vulnerable, then you don't naturally make that leap from the reason you think you're not succeeding to why you're actually failing.

But second, and just as insidiously, you're deceived into believing that whatever you're doing instead of getting out there is a good use of your time. This is almost never the case. Programmers don't write good code by crawling into a cave and pecking away - they communicate, they interact with others, and they take criticism and turn it around into useful changes.

## The genius anti-loop

The pair of these two is something I'll call *the genius anti-loop*.[^antiloops] It goes like this:

1. I am not a genius.
2. Therefore I will [`wait`/`work`/`practice`] until I am a genius.

I read a blog post a while back that was somewhat similar to this.[^unsourced] Except it argued that even though there are genius programmers out there, we shouldn't try to be like them. We should be happy with where we are, and share it anyhow because that's the least we can do to better ourselves.

I wasn't satisfied with that, so the argument never worked on me. *I want to change the world, damn it*. It wasn't enough to know that I'm doing the best I can: I had to actually be impressive. Since I wasn't impressive, I tried to do things that would make me *eventually impressive*, which is kind of like systems that are [eventually consistent](http://en.wikipedia.org/wiki/Eventual_consistency) except it never happens.

## Make yourself vulnerable

The real magic here is that the people we idolize as geniuses are never actually geniuses the way we think of them. That makes sense: if we had an accurate mental model of how a genius works, and what her thought process is like, then we could just decide to be geniuses ourselves. But the reality is

> They say that if you follow the guidelines above:
>
>> ...people will think you're a genius.

This is supremely demistifying, because if you're trapped in the genius anti-loop then it actually presents a solution. You don't have to be *eventually a genius*, or even *eventually impressive*. You just have to try, and do that one practically-easy-but-not-mentally-easy thing right now: stop worrying what people might think.

> This was completely revolutionary to me. I understood it the second they explained it in the video. I knew people that hid nothing and were vulnerable in every way; I just hadn't realized that this was why I revered them as such.[^joshblogpost]


## What learning feels like

I've tried to write about all sorts of things. The only big one that I publish is my [math notes](/tex.html). Math is one of those topics where you *always feel stupid*. I'm of the belief that it's hard even for mathematicians: when you try to read papers about it, you're confronted with entire languages of symbols you don't understand.

> The reason for this phenomenon is that mathematics is so rich and infinite that it is impossible to learn it systematically, and if you wait to master one topic before moving on to the next, you'll never get anywhere. Instead, you'll have tendrils of knowledge extending far from your comfort zone.[^mathishard]

In other words, in math, *there are no geniuses*. It feels exactly the same for me to do math as it did to Leonhard Euler: hard. He might have been way smarter than me, and had he been born today he might be capable of more precision and abstraction than I can dream of, but *it would still feel hard*.

I'm pretty convinced that programming is the same way. There will always be way more knowledge than one person can wrap their head around, but biggest thing that separates the masters from the cruft is that this doesn't phase them. They just keep on sharing their work and taking criticism, because just by doing that they are automatically better than everyone who won't.

## Beneficial feedback loops

One thing I have noticed about learning programming is that it is exponentially improved by quick feedback cycles.

- Automated testing means you don't constantly introduce old regressions.
- Static type checking means you catch "simple" errors at compile-time.
- Agile software development[^agile] produces code faster than the waterfall method.

You could *never* become a decent programmer in 2014 without harnessing quick feedback cycles. All other things equal, if we're both working on the same project, but I have all this awesome test automation and compile-time checks and ship code weekly instead of yearly, my code will be better than yours.

### Social feedback loops

The key to all of this "get yourself out there" garbage is that it creates another feedback cycle. The difference is that this feedback cycle targets *you* instead of targeting something superficial like your code. If your code gets better along the way - great - but the real plus is that you won't write code the same dumb way twice.

Maybe I try to isolate myself because working on things feels like *testing*, and contributing, writing, and publishing feels like *production*. Maybe I'm less confident in my ability to reach out and share ideas with other people than I am to work on them in my own headspace. But it's time to break that loop: if I don't get myself out there, I'm doing the equivalent of writing reams and reams of code and never even running it.

Today, I'm shipping this blog post.

[^mathishard]: [For potential Ph.D. students](http://math.stanford.edu/~vakil/potentialstudents.html).
[^unsourced]: I can't find the source, but I'll update this if I ever do.
[^antiloops]: The name comes from Steve Yegge's [interview anti-loop](http://steve-yegge.blogspot.com/2008/03/get-that-job-at-google.html),  which is related in that it involves the same kind of mental deceit.
[^joshblogpost]: From the [blog post cited earlier](http://joshldavis.com/2014/06/13/put-yourself-out-there/).
[^agile]: Neglecting any potential drawbacks, and focusing on the objective fact that *you're shipping code more often*.
