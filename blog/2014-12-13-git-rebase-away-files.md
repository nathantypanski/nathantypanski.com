---
title: Git: rebasing apart history
tags: Git, workflow
...

I always appreciate it when people share some of the quirks of their development workflow,
particularly with Git, since the version control monster is so powerful that it can be
difficult to stumble your way through its features.

Here's something I do every so often: I'm working too quickly, and I add extra
unrelated files to an otherwise granular commit. Then I have to fix it, but I
usually don't want to rewrite the commit message. What's the solution?

Here's an example from my Emacs config:

``` {.sourceCode}
Refs: [master]

    eyecandy: just use zenburn theme

---
 .travis.yml                    | 11 +++++++++++
 config/eyecandy/my-eyecandy.el | 22 +++-------------------
 2 files changed, 14 insertions(+), 19 deletions(-)
```

I was partway through an attempt at adding simple [travis](https://travis-ci.org/) testing
to the repo, but accidentally included that `.travis.yml` along with a batch of
theme config changes.

Obviously my personal text editor config files can't get away with [bad commits](https://github.com/nathantypanski/emacs.d/commit/3597bfd5d1c1c561f019f7fce7ad6119b5edb07e)![^badcommit] I'll have to fix that.

[^badcommit]: Or I might not tolerate it in a few weeks. Ho, ho.

There are a handful of ways I could do this, but the method I inevitably choose
is [`git rebase`](http://git-scm.com/docs/git-rebase). Called with the `--interactive`
switch, `git rebase` gives you a list of commit messages since some point in the
tree, and lets you modify and rewrite them as you see fit.

What we're going to do here is this:

- start an interactive rebase,
- remove `.travis.yml` from the commit shown above,
- amend the old commit,
- create a new commit adding `.travis.yml` to the repository.

The reason I prefer using an interactive rebase is

1. I know how to do it,
2. it differs less from my everyday git usage than the alternatives.[^alternatives]

[^alternatives]:
    If I have to hack apart a tree in Git, the easiest way for me to do it is
    usually inside of an interactive rebase. Unlike memorizing a bunch of esoteric
    one-liners, this lets me work on a repository like I'm always "at its tip" -
    which is the normal state of a Git repo when you're working on new changes.

## Picking a rebase target

One of the most important choices before you start any interactive rebase is "how far back
am I rebasing." It's only safe to rewrite the parts of history that haven't been pushed to
a remote yet,[^remote] so the natural choice is to rebase against a remote.

I'll pretend that I'm working on a codebase with multiple people, purely for didactic
purposes, and do a fetch before I get started:

``` {.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git fetch origin
~/dotfiles/emacs/.emacs.d/ »
```

Since there was no output, that means I have the latest version of `origin`.
Great! Let's get started:

``` {.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git rebase -i origin/master
Cannot rebase: You have unstaged changes.
Please commit or stash them.
```

Woops.

[^remote]: You can get away with rewriting commits that are on a remote, but then you're
    doomed to `git push -f`'ing all over said remote. Without good reason, this is a
    pretty bad idea. If you have coworkers, it's a really bad idea.

## Cleaning your worktree

When you start an interactive rebase, you need to have a clean worktree.
To get there, I used to run `git stash` before the rebase, and `git stash apply`
afterwards to reapply my old changes after modifying the log.
As it turns out, there's a better way (from `man git-rebase`):

``` {.sourceCode}
--[no-]autostash
    Automatically create a temporary stash before the operation begins, and apply it
    after the operation ends. This means that you can run rebase on a dirty worktree.
    However, use with care: the final stash application after a successful rebase might
    result in non-trivial conflicts.
```

As mentioned, stashes don't necessarily apply cleanly after a rebase. This works best
if you know that your unstaged changes won't conflict with anything you're rebasing.
In my case, I only start rebases if I think my stash (if any) will apply cleanly, so
I'm going to make this a default in my config:[^autostash]

```{.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git config --get rebase.autostash
~/dotfiles/emacs/.emacs.d/ » git config --global rebase.autostash true
~/dotfiles/emacs/.emacs.d/ » git config --get rebase.autostash
true
```

## Blasting apart history

Now we can get started, and Git will `stash` unstaged changes automatically before
interactive rebases.

```{.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git rebase -i origin/master
```

We're dropped into the interactive rebase window:

```{.sourceCode}
pick 712edab eyecandy: just use zenburn theme

# Rebase 8edc222..712edab onto 8edc222
#
# Commands:
#  , pick = use commit
#  , reword = use commit, but edit the commit message
#  , edit = use commit, but stop for amending
#  , squash = use commit, but meld into previous commit
#  , fixup = like "squash", but discard this commit's log message
#  , exec = run command (the rest of the line) using shell
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#
# Note that empty commits are commented out
```

### Removing unrelated files

I want to remove a file from `712edab`, so I change that commit to `edit`:
```{.sourceCode}
edit 712edab eyecandy: just use zenburn theme
```
and save the file. Now we're back at the terminal:

```{.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git rebase -i origin/master
Created autostash: ce6634c
HEAD is now at 712edab eyecandy: just use zenburn theme
Stopped at 712edab4e039cf2bd784031f04a202eec5f2195a... eyecandy: just use zenburn theme
You can amend the commit now, with

        git commit --amend

Once you are satisfied with your changes, run

        git rebase --continue
```

This has dropped us into the history at a point in time "just after" `712edab` was committed.
We can modify the files that were changed in that commit, and then use `git commit --amend`
to replace the old commit with our new changes.

In my case, I still want to keep my `.travis.yml` changes, but wish to move them
into a separate commit. I'll remove the file from Git's index, but not my worktree,
using `git rm --cached`.

```{.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git rm --cached .travis.yml
rm '.travis.yml'
~/dotfiles/emacs/.emacs.d/ » git commit --amend
```

### Making new commits

Note how the addition of `.travis.yml` is gone now:

```{.sourceCode}
eyecandy: just use zenburn theme

# Changes to be committed:
#       modified:   config/eyecandy/my-eyecandy.el
#
```

But the file is still present on my machine:

```{.sourceCode}
[detached HEAD cc71d68] eyecandy: just use zenburn theme
 Date: Sat Dec 13 20:45:54 2014 -0500
 1 file changed, 3 insertions(+), 19 deletions(-)
~/dotfiles/emacs/.emacs.d/ » ls .travis.yml
.travis.yml
```

Great! We've rewritten the commit to remove the unrelated changes. It's worth
noting that `edit` actions in a rebase still let you make new commits, unlike
most "detached head" states in Git.

So we can just add the file back in right here, and finish the rebase:

```{.sourceCode}
~/dotfiles/emacs/.emacs.d/ » git add .travis.yml
~/dotfiles/emacs/.emacs.d/ » git commit -m 'test out travis CI for emacs'
[detached HEAD dab1f85] test out travis CI for emacs
 1 file changed, 11 insertions(+)
 create mode 100644 .travis.yml
~/dotfiles/emacs/.emacs.d/ » git rebase --continue
Successfully rebased and updated refs/heads/master.
Applied autostash.
```

## Conclusion

Now the repo is how I wanted it:

```{.sourceCode}
2014-12-14 03:46 Unknown         o Unstaged changes
2014-12-13 22:42 Nathan Typanski o [master] test out travis CI for emacs
2014-12-13 20:45 Nathan Typanski o eyecandy: just use zenburn theme
2014-12-13 19:09 Nathan Typanski o {origin/master} evil: fancy smart indent behavior
```

This is just a little snapshot of my Git workflow.
I wrote this post because I find myself constantly wondering if I'm using Git "the right way",
and find myself curious how other people solve problems like this one.
Perhaps there's a way to do this using only a handful of one-liners, and I'm
going overkill by using `git rebase` for this kind of thing (it certainly feels that way).

But then I realize there's no One True Workflow with Git.
Git usage is a [state function](https://en.wikipedia.org/wiki/State_function):
we went from our initial state to our desired result state, and nobody cares
how we got there. That's the whole point.

[^autostash]: From the man page for `git rebase`:
    ``` {.sourceCode}
    CONFIGURATION
           rebase.autostash
                      If set to true enable --autostash option by default.
    ```
