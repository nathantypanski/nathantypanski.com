---
title: Crafting signal from noise
tags: alerting, philosophy,
...

When I worked at NASA a decade ago, after a coffee chat one afternoon the security folk invited me to their War Room---you know, the one with the Big Board*---and the screens mounted on the wall lit up with alarms, which they told me captured all the bad things which were happening in the network.
People were busy investigating those alarms, it seemed, so it sure *looked* like their tools were doing something useful.
Investigating bad activity is of obvious value, while *not investigating bad activity* is obviously wrong.


> Nothing had been hacked, but the board remained red.
> A red board means people are busy.
> Idle teams don't justify job openings.

![]()

Government incentive structures are often mirror market incentives, making good teams do crazy things: wasting money makes you broke in a free market, but in government wasting money *makes you more money*. Spend is use-it-or-lose-it, and unspent funds get reallocated next calendar year.

In my sysadmin days,[^sysadmin] we had just inherited a security alarm tool, which those in the industry call a "SIEM."
Security information and Event Management systems are a mainstay of classical defensive security, consuming logs and detecting malicious activity.
_Detections_ trigger _alerts_ based on detected events.
This company had just suffered a small but expensive security breach, so we came in to figure out how to improve things.
Over the last several years, their SIEM fired *40,000* events *per day*.
Even the "high priority" issues were impossible to realistically triage.

As things worked out, the breach *had* alarmed on their platform, but because there was so much noise, their team disconnected the pagers.

As it turns out, there are *two ways to be blind* in system alerting. One way to be blind is to not have the right detections. Maybe you alerted on Ceph, but the attacker went for S3. If you alert on the wrong events, you won't see the baddies.
The other way to be blind is to detect *too many events*.
If the number of security issues detected by your system is infeasible to triage, you *also* can't see.

One reason teams keep noisy alarms comes from compliance requirements:
Removing alarms causes compliance problems: even a noisy alarm *is an alarm*, and you are required to have alarms.
Bad compliance frameworks overindex on satisfication of requirements, but this is only truly dangerous when it comes at the expense of system security.

Compliance requirements are generic requirements from industry-standard frameworks or rules.
Less charitably, *standard* means *unspecialized*, so those requirements are insufficient for security.

Our aim in alerting, both for platform reliability and security alerting, is to focus on signal: alerts should be actionable.
Alarming too often is *worse than

Alerts should be *actionable*.

[^sysadmin]: For all it's worth, sysadmins could *always* write code; an average sysadmin is probably competent in several languages. Nowadays the term has lost its meaning and now we have articles about how [sysadmins can't code](https://cuddletech.com/2013/05/why-sysadmins-cant-code/index.html), so we call them "reliability engineers" or "devops engineers" or "production engineers". A good sysadmin can probably speak competently about Linux Kernel implementation details and has likely written *at least* 100k+ lines of code over the years. Unlike regular SWEs, however, sysadmins build, test, and code in service of *system management* rather than end-users. To grow the field, the industry lowered its skill bar for sysadmins, becoming something akin to "configuration manager," i.e. "YAML engineer."

You're a sysadmin at a company in a field where security is important and you're trying to detect malicious behavior.
SIEM tools, EDR, XDR and ilk are are _excellent_ at detecting misbehavior: they often detect hundreds or *thousands* of malicious events per day.
The security event detections produces all noise today; you couldn't come close to paging on the event stream.
How do you go about approaching

"Data loss prevention" detection, in particular, notoriously produces *lots of noisy events*.
That's the problem, actually: the tools *detect too many things*.
To concretize our landscape a bit, let's say you happen to work on *Foo*.
Your adversaries could reap lots of value if they exfiltrate *Foo* from your databases, and so one of the many alarms in your detection engine is a *FooDetector*.

This satisfies the compliance team, since one of your requirements is to alert on DLP events pertaining to Foo.

Were the screens *valuable*? Was the information rendered a *valid* rendering of their security posture?

> That team was full of competent staff, they had seen it all, and they were quick to respond and investigate problems. This isn't a problem with *team*: misaligned incentives can turn even good people into bad outcomes, especially in government.
