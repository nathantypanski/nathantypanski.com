----
title: Head down for the finish line
tags: life, Red Hat, algorithms, Palantir, Red Hat, microcontrollers, hardware, Arch Linux, Linux, jobs
----

I haven't blogged since January, and I really wish I had a better reason for it but the truth is I simply have not applied my characteristic productive laziness to writing.

I've been reading a lot of jwz lately.
I think I read [the netscape dorm](http://www.jwz.org/gruntle/nscpdorm.html) three times in the last few weeks.
That was a bad sign, and I should have known better, but I didn't quite pick up on it.

So let's do a breakdown of the past few months; maybe somehow I can justify the silence.

## Red Hat

[Tom Callaway](https://fedoraproject.org/wiki/User:Spot) from Red Hat [came to speak on March 24](http://blog.pcs.cnu.edu/?p=254) for CNU's PCSE[^pcse] department and the [LUG I founded](http://cnulug.org/).
That was awesome.
I don't think anything could validate a newly-founded Linux user group more than having industry leaders come joke with you about tiling window managers. Quoting Tom:

> The venn diagram between people who use tiling window managers and people who know Haskell is just a circle.

In complete good spirit, it's worth mentioning [i3](https://i3wm.org/) was written largely *because* the developers thought XMonad was cool, but didn't want to learn Haskell.

After the presentation I was told by one of the LUG members that Tom's talk was really inspiring for them.
The student said "It's really great seeing industry leaders and finding out they're just like us - only maybe a bit older or more knowledgeable - and feeling like I can be just like that, too, if I just work hard and dedicate myself.

It's that kind of thing that makes me so glad I stuck with CNULUG.
Even through all the lows, the awful turnout for the first year or so,
we've pulled through - and now we matter so much that we're making a real difference
in students' lives.
Those sort of results, to me, almost makes my whole four years at CNU worth it on their own.

[^pcse]: Physics, computer science, and engineering.


## PCSE Coding Challenge

A team I was on (with [Wilson Ho](https://www.linkedin.com/pub/wilson-ho/82/33/b23) and [Ian Miller](https://www.linkedin.com/pub/ian-miller/b1/ba9/513)) won my university's "PCSE Coding Challenge" on April 3.
That was fun.
I totally embarrassed myself during the big-wig ACM programming competition last semester, so it's relieving to see some sort of quantifiable progress.

Something struck me about the [problems](/files/2015Spring-PCSE-coding-challenge-Koehl.pdf), though.
We had three language options: Java, C (`gcc` with default flags), and C++ (`g++` with default flags).
At least two of these would be more well-suited to scripting languages, though.

Problem 4 is simple text substitution.
The input two lines forming a substitution table (preceded by a less-than-helpful count), followed by a list of plaintext/cyphertext blocks:

``` { .sourceCode }
3
a b c
B C D
2
PT: abc
CT: BCDDCB
```

You're supposed to encrypt the plaintext blocks, and decrypt the cyphertext blocks.
Normally I'd solve this in Bash:[^c-solution]

``` {.sourceCode .Bash}
#!/bin/bash

while read -r cypher_count; do
    [[ ${cypher_count} -eq 0 ]] && break
    read -r plaintext
    read -r cyphertext
    read -r count
    for _ in $(seq ${count}); do
        read -r line
        if [[ ${line:0:2} == 'PT' ]]; then
            echo -n "CT: "
            echo ${line:4} | tr "$plaintext" "$cyphertext"
        else
            echo -n "PT: "
            echo ${line:4} | tr "$cyphertext" "$plaintext"
        fi
    done
done
```

Problems 1 and 5 were semi-interesting graph problems, but why bother with those when there are nice and easy text substitution problems to play with?

Anyway, I'm actually pretty embarrassed by the win.
My team solved the above text translation problem and the Sudoku problem (problem 3).
Neither of those are particularly difficult, or require any sort of serious algorithms knowledge to solve. They're more like run-of-the-mill programming challenges.
We were about 90% of the way through problem 2, another simple text processing/sorting problem, when the timer rang.

Main lesson learned: brush up on I/O in whatever language I'm choosing before the next algorithms competition.
We could have performed a *lot* better if we spent less time fumbling around with a `BufferedReader` and more time thinking about algorithms.

## Mike Bland, CNU research, Google, and the US Government

[Mike Bland](http://mike-bland.com/)  keynoted [Paideia](http://cnu.edu/research/about/paideia.asp), CNU's annual "research conference" on April 11.
He gave a primer of his talk, itself a sort of toned-down variation of his slideshow behemoth
[Large Scale Development Culture Change: Google and the US Government](https://18f.gsa.gov/2014/12/11/large-scale-development-culture-change/)
to the PCSE department on Friday, April 10.
Apparently no one told him that the April 10 event would be a "talk", and not a Q&A, so Mike just ran through the same slides he'd be covering at Paideia.

Mike really did a great job relating to the audience at the keynote.
Even though he covered somewhat technical material, I think everyone who was there (mostly non-programmers) managed to relate to his talk and took him seriously.
He made [18F](https://18f.gsa.gov/) seem like *the real deal*,
where Google was just like the training grounds for work that will have real,
lasting impact on the United States.

One unfortunate consequence of Friday's talk was that PCSE folks were notably absent on Saturday.
In some sense that's OK because they already saw most of it on Friday, but in another sense it's not since Mike was quite obviously more well-prepared on Saturday.

Mike is locally famous for organizing student demonstrations to "save", from bureaucratic shutdown, CNU's
[graduate program](http://cnu.edu/academics/graduate/),
which basically makes him the reason I can write "Christopher Newport University" on my resumé instead of "Christopher Newport College".
But he was actually *confused* why he was relevant when
[Dr. Gore](http://www.pcs.cnu.edu/~David.Gore/) asked him to keynote CNU's undergraduate/graduate research conference. Go figure.

One thing I noticed is that he put a lot of effort into engaging the audience during
his presentations.
Sometimes this was successful, and sometimes this wasn't.
The difference I noticed between successively captivating a live audience and *not* doing so is when he asked people in the audience to express themselves.
I can recall this same dynamic from when I gave my [talk advocating Git](http://nathantypanski.github.io/git-talk/) at NASA Langley: people were engaged when it seemed like I valued their opinions on things.
Asking questions like "How many have you have used version control? How many of you *liked* using it?" engaged viewers way more than my 15-minute usage demo.

Similarly, when an audience members got to ask questions and be part of a conversation
instead of just recipients of a speech, they seemed more alive and interested themselves,
and the presentation seemed to have bigger impact.

This kind of thing reminds me not to focus too hard on slides when I do a presentation.
What I should be doing is motivating the audience, and then letting them tell me what they're interested in - not the other way around.
Unless I have something truly original to say, people are probably going to be more interested in how what I'm saying applies to *them* than whatever it is that I'm saying.
Maybe that's a sad reality, and people should care more about the generalized intellectual pursuit than individual relevance, but feeling that way isn't going to make people listen to you.

Making people relate to you will make them listen. Inspiring the hell out of people will make them listen. You don't need to win everywhere to captivate an audience, but you better damn well captivate them or you're just wasting a bunch of people's time.

[Previously](2014-08-22-openssl-makefiles.html).

## Microcontrollers are fun

I'm in a microprocessors class this semester, and it's awesome.
It's definitely among the hardest classes I've taken at CNU, but also one of the most rewarding.
Among most programmers I meet, both hardware programming and C are seen as some crazy sort of "black magic".
I understand we all pine for the good ol' days when C was taught to every budding programmer instead of [Java](http://www.joelonsoftware.com/articles/ThePerilsofJavaSchools.html),
when Linus Torvalds' [comments on linked lists](http://grisha.org/blog/2013/04/02/linus-on-understanding-pointers/)
actually made sense to people with CS degrees,
 but modern CS departments apparently don't throw students into the deep end with nothing but a C compiler and an assembly-language manual anymore.

Yeah, most people in the class weren't ready for that.
They freaked out at first.
But, you know what?
The students learned the material.
Myself included.
[Dr. Wang](http://cnu.edu/pcs/faculty_staff/wang.asp) didn't spoonfeed us; he just gave us hardware manuals and told us "when you get out in the industry, you're not gonna have textbooks" and let everyone sink or swim.

I spent this evening solving example problems from an exercise booklet on a [PIC16F877A microcontroller](https://www.microchip.com/wwwproducts/Devices.aspx?product=PIC16F877A).
It was fun.
I feel like I haven't had fun programming in a while, but when you sit down and program hardware in C it can really reinvigorate love of the craft.
The work wasn't very interesting: make different LEDs light up depending on the voltage reading from a dial.
Set a cutoff threshold whenever a button is pressed and light different LEDs depending on whether you're above or below that threshold.
Simple things, but the experience is rewarding.
It reminds me of being in high school, writing my first Java classes to simulate the Monty Hall problem or whatever.
There's a sense of joy and wonder that programming can produce, and it's when I find it that I do my best work.

# Leaving college, joining Palantir

I graduate in May, and I'm absolutely terrified.
I took an internship with Palantir this summer, on their Mission Operations team, instead of taking the "safe" route taking a full-time offer (like I might have gotten with [SSAI](https://www.ssaihq.com/), who have given me arguably the most rewarding work experience of my lifetime applying bleeding-edge industry practices to the US government).
As apprehensive as this leaves me, I'm really excited about joining Palantir.

Why? I really can't overstate the influence of the mission ops team lead in inspiring me to do this.
Paraphrasing, this guy told me upfront:

> You seem good, but you're kind of green.
> There are specific things about Linux that you didn't know - out of genuine ignorance - and
> our goal is gonna be to teach you those things, so you're capable enough
> to do your day-to-day tasks with us and not need support from anyone else.
> You're not on the level we need *quite* yet, and that's why we're offering
> you an internship, but my goal is going to be to *get* you there and *make*
> you that good so you can succeed with us.

Now, to understand why that kind of talk inspires me, you've gotta understand something about
who I am.
I'm naturally a bit arrogant.
I founded the Linux user group at my school, and I built my first Arch system seven years ago
with nothing but a paper printout of the [beginner's guide](https://wiki.archlinux.org/index.php/Beginners%27_guide) and a single desktop computer.
I've been administering my own webservers for three years, and running other people's
Linux servers for money for two.
At least among my peers at CNU, I'm simply *not used* to people knowing more than me about this operating system.

But the people at Palantir are better than me.
They showed me up in the interview process, but it's not just that
- being smart isn't the only requirement to win my respect - they're willing to teach me.
And that kind of attitude is what makes me fall in love with a company.

I'll quote [one of their cofounders, Stephen Cohen](http://dcurt.is/the-compounding-returns-of-intelligence), to drive this point home:

> We tend to massively underestimate the compounding returns of intelligence. As humans, we need to solve big problems. If you graduate Stanford at 22 and Google recruits you, you’ll work a 9-to-5. It’s probably more like an 11-to-3 in terms of hard work. They’ll pay well. It’s relaxing. But what they are actually doing is paying you to accept a much lower intellectual growth rate. When you recognize that intelligence is compounding, the cost of that missing long-term compounding is enormous. They’re not giving you the best opportunity of your life. Then a scary thing can happen: You might realize one day that you’ve lost your competitive edge. You won’t be the best anymore. You won’t be able to fall in love with new stuff. Things are cushy where you are. You get complacent and stall. So, run your prospective engineering hires through that narrative. Then show them the alternative: working at your startup.


[^c-solution]: To be fair, the C solution isn't much worse:

    ``` {.sourceCode .C}
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>

    #define MAX_LINE 512

    int main() {
            char *line = malloc(sizeof(char) * MAX_LINE);
            int count = 0;
            do {
                    scanf("%d\n", &count);
                    char *pt = malloc(sizeof(char) * count + 1);
                    char *ct = malloc(sizeof(char) * count + 1);

                    int i;
                    for(i = 0; i < count; i++)
                            scanf("%c ", pt + i);
                    for(i = 0; i < count; i++)
                            scanf("%c ", ct + i);

                    scanf("%d ", &count);

                    for(i = 0; i < count; i++) {
                            char *left;
                            char *right;
                            fgets(line, MAX_LINE, stdin);
                            if (line[0] == 'P') {
                                    left = pt;
                                    right = ct;
                                    printf("CT: ");
                            } else {
                                    left = ct;
                                    right = pt;
                                    printf("PT: ");
                            }
                            int j;
                            for(j = 4; line[j] != '\n'; j++) {
                                    int k;
                                    for(k = 0; left[k] != line[j]; k++);
                                    printf("%c", right[k]);
                            }
                            printf("\n");
                    }
                    free(pt);
                    free(ct);
            } while (count != 0);
            free(line);
            return 0;
    }

    ```

    But there's a bug in the official test data, and that code won't work unless
    the following is fixed:

    ``` {.sourceCode}
    -5
    +7
     d e h l o r w
     H J L O Q T V
    ```
