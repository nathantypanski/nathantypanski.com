----
title: Emacs just segfaulted
----

Well, that was fun.

``` {.sourceCode}
nathan@blackbook power_hungry % emacs problem.md
Fatal error 11: Segmentation fault
Backtrace:
emacs[0x507da9]
emacs[0x4ee96c]
emacs[0x50654e]
emacs[0x506773]
emacs[0x5067df]
/usr/lib/libpthread.so.0(+0x11080)[0x7f4d99ad0080]
emacs[0x5370ac]
emacs[0x5347d6]
emacs[0x431f15]
emacs[0x561d53]
emacs[0x59603b]
emacs[0x561b5b]
emacs[0x59603b]
emacs[0x561b5b]
emacs[0x59603b]
emacs[0x561b5b]
emacs[0x59603b]
emacs[0x56175d]
emacs[0x561b5b]
emacs[0x59603b]
emacs[0x56175d]
emacs[0x561b5b]
emacs[0x55d9aa]
emacs[0x561c59]
emacs[0x563186]
emacs[0x55dd1c]
emacs[0x561d37]
emacs[0x59603b]
emacs[0x56175d]
emacs[0x561b5b]
emacs[0x562fb8]
emacs[0x561c59]
emacs[0x59603b]
emacs[0x561b5b]
emacs[0x59603b]
emacs[0x561b5b]
emacs[0x561e5a]
emacs[0x4fd311]
emacs[0x5604b2]
emacs[0x4eedec]
emacs[0x560453]
...
zsh: segmentation fault (core dumped)  emacs problem.md
emacs problem.md  11.86s user 0.62s system 12% cpu 1:36.16 total
```
