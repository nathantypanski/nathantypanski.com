---
title: Towards a Vim-like Emacs (pt. 1)
tags: emacs, software
...

I've [previously written](/blog/2014-07-02-switching-to-emacs.html) about my own process of transitioning from Vim to Emacs. It's fairly high-level, however, and doesn't cover the nitty-gritty: all those long, painful hours spent trying to smash Emacs into reasonable keybindings are lost.
You get the retrospective, the analysis, but not the same benefit of hindsight I've gathered since I switched. Unfortunately, there aren't many resources out there on how to do this *right*. People use Emacs. It's a tool. Most of its users don't write about it, and the percentage of Emacs users who try to emulate Vim and also write about it is even smaller.

There is [Bling](http://bling.github.io/), the vim-airline developer, who switched and ultimately convinced me to give Emacs a shot. But his tips just scratch the surface of what's necessary to replicate the finger-friendliness of a modern Vim workflow.

It's a well-known and overstated joke that the default Emacs bindings are bad.
If you're reading this post, you probably already agree with me here, but for the uninitiated: key combos are the devil. Any time you are pressing two keys at once, with the same hand, hundreds of times per day, you are setting yourself up for repetitive stress injury. As programmers, we need to take care of our hands or our careers will be over.

This is going to be a tutorial, from ground-zero to a working Evil configuration and generic development environment, that will teach you to understand Emacs and configure it to suit your needs. It's going to follow my journey so far on customizing Emacs, and will teach you everything you need to get started learning its guts and crafting a personalized editing environment in this text editor.

## First, some terminology

When you first fire up Emacs, you need to learn how they reference the keybindings. This is pretty simple, but can be confusing if you don't have the initial introduction.

- `M-x` means "press the `<ALT>` key, then press `x` while still holding that down". This brings up the Emacs command prompt, which gives you access to any of the functions that are declared **interactive** - that is, Elisp functions that may be run interactively by the user.
  - If someone gives you a command sequence like `M-x package-install <RET> evil <RET>`, that means to do the `M-x` keybinding, press enter, then type `evil` and press enter. Often the second `<RET>` will be omitted and taken as implicit.
- A **buffer** is a place where text may go. This is distinct from a **window** which is a visual area on screen which displays a buffer.
- A **frame** is another Emacs window in your window manager that is attached to the same Emacs process.

That's it for the basics. Let's see where we can go from here.

## Package archives

In order to install packages from things besides the default repos, you need to define a variable called `package-archives` with the URLs of the package sources in it. You do this in your `init.el` file, which is located at `~/.emacs.d/init.el` by default.

``` {.sourceCode }
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
```

[MELPA](http://melpa.milkbox.net/) is the most important source for up-to-date Emacs packages. It's kind of like the [Arch Linux](/blog/2014-07-25-all-your-daemons.html) of Emacs package archives - it builds directly from upstream all the time, and it works with the builtin Emacs package manager.

Once you've added the above to that file, you can evaluate it inside Emacs with `C-x C-e`, while the cursor is positioned outside and following that expression.

### Installing Evil

Once the above is finished, you can install `evil-mode` with `M-x package-install <RET> evil <RET>`. The compilation log that pops up at the bottom of the screen can be killed with `C-x 4 0`. That runs the function `kill-buffer-and-window`, which you can also call interactively at the `M-x` prompt. More on that later.

After that, Evil will be installed into the `~/.emacs.d` directory. To enable it, type `M-x evil-mode`. Now all your Vim bindings work, as long as you're in the same file.

You probably want to enable Evil more permanently. Add the following to the bottom of your `~/.emacs.d/init.el` file:

``` {.sourceCode }
(require 'evil)
(evil-mode 1)
```

This will automatically enable `evil-mode` every time you start Emacs. It's important that it goes at the bottom of the file. Evil relies on starting up after the rest of your packages, so that it can detect them and overlay its keybindings appropriately.

## Modes

Like Vim, Emacs uses the word "modes" to refer to an element of state in its editor. Unlike Vim, this is not referring to *modal editing*, but rather to the composition of various modules that make up your current editing environment.

A **major mode** defines your primary interaction with a buffer in Emacs. There can only be one of these active at once, and they are not all necessarily for editing text. For example, `dired` is a major mode for editing directories, and `magit` is a Git interaction interface. An important major mode to be aware of is `fundamental mode`, which is a major mode with no discerning features besides the ability to type text into a buffer.

**Minor modes** are compositional - there can be more than one of them, and minor modes loaded after others can override the settings of a previous one. Evil is one such example of a minor mode.

#### States

Evil uses the term *state* to refer to what Vim calls a "mode". There are more states than these, but here are a few to get you going:

- `evil-emacs-state`[^emacs-state]
- `evil-insert-state`
- `evil-normal-state`
- `evil-visual-state`

Each of these can be called interactively with `M-x` to enable them. The way Evil works is each of these states is bound to a different keymap, and those keymaps change the meanings of your keys to call different functions when they are pressed. This affords you the ability to remap bindings across different states, where they will only work when `evil-mode` is enabled and Evil is also in the right state to activate the keymap.

## Binding keys

Let's get started by adding some bindings to Evil. We want switching between windows to be easy, so we can bind the following:

``` {.sourceCode}
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
```

This binds `<CTRL>-hjkl` to window movement commands when Evil is in normal state.

There are a number of interesting things about these expressions. First, position your cursor over the word `define-key` and call `M-x describe-function`. It will provide the default value of the thing under your cursor, so you can just press `RET` again to bring up a buffer with help information. Read the help information it gives you, then move on. The buffer that appears will be in `help-mode`, which does not have Evil bindings, but you can use `q` to close it and keep working.

The proper term in Emacs for "your cursor" would be **point**. This is used extensively in the help documentation, so it's worth remembering. The "thing" at point that provided the default argument for `describe-function` was a **symbol**.[^symbols] A symbol is basically any thing in Emacs Lisp which has a *name* and can be hashed into a lookup table, where it then becomes an **object** in Lisp. These objects could be anything: functions, data, or text, for example, but symbols are interesting because we can give them names and refer to them later somehow.

From the [official manual](http://www.gnu.org/software/emacs/manual/html_node/elisp/Symbols.html):

> You can test whether an arbitrary Lisp object is a symbol with `symbolp`:
>
> — Function: **symbolp** *object*
>
>   This function returns `t` if *object* is a symbol, `nil` otherwise.

### Quoting

Readers with keen eyes may have noticed that one of the arguments in the above code has an apostrophe (`'`) before it. This is because `evil-window-left` is a function's name, and we want to be able to call it later by binding it to a key. Putting an apostrophe before it when passing it as an argument to a function means we're passing the **quoted** form of `evil-window-left` to `define-key`, not what it evaluates to.

Since our intent is to pass the *symbol* `'evil-window-left` into `define-key`, and not the evaluated form of `evil-window-left` itself, this is the right thing to do.

All arguments in Elisp expressions, without quoting, will be evaluated before they are passed into a function call. Quoting lets us avoid that and pass their symbols instead.

### Maps and keys

The two other arguments to each of those keybindings were unquoted. The keymap, `evil-normal-state-map`, goes directly into the function, since the purpose of `define-key` is to modify that map. The expression `(kbd "C-h")` is actually another separate function call that happens before the outside expression is evaluated. If we use `M-x describe-function` to look at the help documentation for `kbd`, we see:

``` {.sourceCode}
kbd is a compiled Lisp function in `subr.el'.

(kbd KEYS)

Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `edmacro-mode').
```

We need to use the `kbd` function to describe our keybindings, since they contain an extra operator (the `<CTRL>` key). While single character bindings can be looked up in keymaps directly, a key sequence like `C-h` is not stored in the map in that form.

You can look at the value of `(kbd "C-H")` by typing it on a line below those bindings, and then evaluating it with `C-x C-e` as before. In the **minibuffer**, the text area with a blank line at the bottom of the screen, `"^H"` is displayed. That tells you that the expression

``` {.sourceCode}
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
```

could also be written

``` {.sourceCode}
(define-key evil-normal-state-map "^H" 'evil-window-right)
```

but we will usually prefer the former, since it is more obvious your intent.

## Lisp navigation

Accessing help documentation using `M-x describe-function` is pretty awesome, but the interface for doing it is fairly clunky. We can make it better using `elisp-sime-nav-mode`, the [superior Lisp interaction mode](http://common-lisp.net/project/slime/) for Emacs. You can install it with `M-x package-install RET elisp-slime-nav`.

Slime mode needs to be enabled for it to be used. To do this, we will define a function that we'll tie to the hook for `emacs-lisp-mode`. A **hook** is a function that gets evaluated when something predefined happens in your editor. Most modes have hooks already configured, and all you will need to do is add functions to call with them using `add-hook` for them to become useful.

First we `require` the package `elisp-slime-nav`, which ensures that it is loaded when Emacs starts:

``` {.sourceCode}
(require 'elisp-slime-nav)
```

Then we define our function:[^hooks]

``` {.sourceCode}
(defun my-lisp-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
  )
```

Add that into your init file after the `require` call. Note that the function definition is prefixed by `my-`. While not all Emacs packages do this, Elisp does not have namespaces like Python or Ruby does. To counteract this, convention is to define all of your functions prefixed by your package name, and for personal code using `my-` is not uncommon.

Then, we add it to to the `emacs-lisp-mode-hook` using `add-hook`:

``` {.sourceCode}
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
```

This makes `elisp-slime-nav-mode` and `turn-on-eldoc-mode`[^turn-on-eldoc] get called every time `emacs-lisp-mode` is enabled. Now go ahead and evaluate these with `C-x C-e`. To make this easier, you can surround the block in a `(progn)`, which makes the whole thing get treated as one big expression for evaluation:

``` {.sourceCode}
(progn
  (require 'elisp-slime-nav)
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode)
    )
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
)
```

While the `(progn)` here doesn't serve any practical purpose, it might help you group blocks of code for evaluation while you're hacking at them.

Since `emacs-lisp-mode` is already running (you're in a `.el` file), you can force your hook's evaluation by typing `(my-lisp-hook)` into the buffer and evaluating it manually.

You'll notice that when you hover over functions now, `eldoc-mode` shows their signature in the minibuffer. If you want to see the help information like we'd been doing manually before, though, you still need to type `describe-function` into the `M-x` prompt. Now that we have Slime, we can simplify this with another keybinding:

``` {.sourceCode}
(evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
  'elisp-slime-nav-describe-elisp-thing-at-point)
```

Evaluating the above will do contextual lookup on the thing at point whenever you press `K` in Evil's normal state.

The syntax here is different from when we defined the [window movement bindings](/blog/2014-08-03-a-vim-like-emacs-config.html#binding-keys) earlier. This is because we want to define `K` to call `elisp-slime-nav-describe-elisp-thing-at-point` only when we are in `emacs-lisp-mode-map` and Evil is in normal state. The `evil-define-key` function lets us do this, by providing arguments for both the Evil state and the keymap for the binding to be active in.

Now you have the tools necessary to explore Emacs Lisp code without gettig lost. Doing documentation lookups on everything you see that you don't understand is a great way to start familiarizing yourself with Elisp code.

Before we move on, lookup the descriptions for `progn`, `require`, `add-hook`, and `emacs-lisp-mode-hook` and read them. Don't be afraid if you encounter words you don't understand. Seeing the bigger picture in all the lingo takes time, but just doing this will help you start to understand it.

## Managing buffers

You can list the buffers that are currently open by typing `M-x list-buffers`. Using the bindings we defined earlier, you can switch to the window that is opened and hit `q` to close it when you're done. To switch to one of these buffers, typing `M-x switch-to-buffer` will bring up a minibuffer prompt where you can type the name of a buffer to switch to it.

For a better listing, you can use `M-x ibuffer`.

![IBuffer with the default settings](/images/2014-08-ibuffer.png)

IBuffer has all sorts of useful features for browsing, filtering, and editing open buffers. But at least for a Vim user, its keybindings won't at all be intuitive. Typing `j` accidentally will call `jump-to-buffer`, which is almost certainly not what you want. If you felt like typing the name of the buffer at a prompt, you probably wouldn't have opened IBuffer to begin with!

Aside from being unnatural for Vim users, the interface to IBuffer isn't actually that bad. The keybindings are mostly single-key sequences, unlike more poorly-designed Emacs modes, and the functions allow for quick actions that will save you tons of work.

### Stealing IBuffer's keymap

When I previously wrote about my experience switching to Evil, I mentioned there are a [couple of different ways](/blog/2014-07-02-switching-to-emacs.html#new-habits) to deal with major mode keymaps being inconsistent with Evil. I'll be covering the full remapping approach here, since it has a few benefits:

- Your major modes will be more tightly integrated with Evil.
- You'll have the ability to redefine keys at will, easily and quickly.
- Remapping keys this way offers a chance at better understanding the functions in a major mode, which might just be missed otherwise. For core components of Emacs, like IBuffer and dired, building a more useful keymap can be a rewarding learning experience in itself.

With `ibuffer` open, type `M-x describe-mode` to pull up the help info below.

![A link to `ibuffer.el` in `help-mode`](/images/2014-08-ibuffer-link.png)

Following that link will take you to the source code for IBuffer. Somewhere in there, you'll find the keymap:

![Default IBuffer keymap](/images/2014-08-ibuffer-default-map.png)

Any one of these major mode keymaps is bound to be really large, so I won't reproduce it here. The idea is to redefine these into a map within Evil, so IBuffer will respect your Evil keymaps and still have its functionality. The `evil-define-key` function we used earlier has the option of taking an unlimited number of arguments. Now, copy lines like the following out of `ibuffer.el`:

``` {.sourceCode}
(define-key map (kbd "m") 'ibuffer-mark-forward)
(define-key map (kbd "t") 'ibuffer-toggle-marks)
(define-key map (kbd "u") 'ibuffer-unmark-forward)
(define-key map (kbd "=") 'ibuffer-diff-with-file)
(define-key map (kbd "j") 'ibuffer-jump-to-buffer)
(define-key map (kbd "M-g") 'ibuffer-jump-to-buffer)
(define-key map (kbd "M-s a C-s") 'ibuffer-do-isearch)
(define-key map (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp)
;; ...
```

and drop them into `~/.emacs.d/init.el`, after `(require 'evil)`, like follows:


``` {.sourceCode}
(evil-define-key 'normal ibuffer-mode-map
  (kbd "m") 'ibuffer-mark-forward
  (kbd "t") 'ibuffer-toggle-marks
  (kbd "u") 'ibuffer-unmark-forward
  (kbd "=") 'ibuffer-diff-with-file
  (kbd "j") 'ibuffer-jump-to-buffer
  (kbd "k") 'evil-previous-line
  (kbd "J") 'ibuffer-jump-to-buffer
  (kbd "M-g") 'ibuffer-jump-to-buffer
  (kbd "M-s a C-s") 'ibuffer-do-isearch
  (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
  ;; ...
  )
```

I used a Vim macro and visual block selection to do the rewrite into `evil-define-key` form for me, and it only took about a minute to perform.

To ensure IBuffer will use Evil's keybindings, add the line

``` {.sourceCode}
(evil-set-initial-state 'ibuffer-mode 'normal)
```

to your init file. You can use this to set Evil's initial state for any major mode - before you know what to bind in a mode, sometimes using Emacs state isn't a bad idea.

Now, if you try to actually use this configuration, it won't work.[^ibuffer-errs] The reason is we need to ensure the Evil bindings are not applied until *after* IBuffer is loaded, since it needs to do some special setup. The solution here is to surround the bindings in an `eval-after-load` block, like follows:

``` {.sourceCode}
(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
       (kbd "m") 'ibuffer-mark-forward
       (kbd "t") 'ibuffer-toggle-marks
       (kbd "u") 'ibuffer-unmark-forward
       (kbd "=") 'ibuffer-diff-with-file
       (kbd "j") 'ibuffer-jump-to-buffer
       (kbd "M-g") 'ibuffer-jump-to-buffer
       (kbd "M-s a C-s") 'ibuffer-do-isearch
       (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
       ;; ...
       )
     )
   )
```

This ensures your custom keymap does not try to set itself until after ibuffer has already initialized it properly.

Your goal after this section is to read the help information for `eval-after-load`, as well as for some of the IBuffer bindings. Just pick ones that seem interesting, and again - don't worry if you don't understand things. Just read a few of them anyway.

Once you've done your reading, get the custom keymap to work by evaluating the block with `C-x C-e`, and ensure it's functional in `evil-normal-state` by calling `M-x evil-normal-state` manually within IBuffer.

### Rebinding

You might think it's crazy to place all of this configuration in one lone init file. We'll get to that in the next section. For now, keeping things in `init.el` will get the job done and let us focus on keybindings.

The obvious thing to do, now that we've copied the keymap, is to add proper `hjkl` bindings. Usually in major modes like this you want to bind `j` and `k` to `evil-next-line` and `evil-previous-line`, respectively.

- Since `J` wasn't taken, I changed `ibuffer-jump-to-buffer` to `J` and rebound `j` to `evil-next-line`.
- The binding for `k` is less obvious: it's tied to `ibuffer-do-kill-lines`.

Looking at the help documentation:

> `ibuffer-do-kill-lines` is an interactive autoloaded compiled Lisp function in
> `ibuf-ext.el`.
>
> `(ibuffer-do-kill-lines)`
>
> Hide all of the currently marked lines.

IBuffer has a marking system that lets you select multiple buffers on which to perform actions. Personally, I don't care very much for hiding marked lines, so I simply removed that binding. Now we have

``` {.sourceCode}
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
```

bound, which covers the absolute basics. But what about `l`? I'm used to [ranger](/blog/2013-08-12-ranger.html), a Vim-inspired file manager, where `l` takes you "into" the current item. In IBuffer, `l` is bound to `ibuffer-redisplay`, a useless function that redisplays the current buffers without loading new ones.[^ibuffer-refresh] Deleting that line lets us rebind it to

``` {.sourceCode}
    (kbd "l") 'ibuffer-visit-buffer
```

and now we can navigate IBuffer much more naturally.

Another Ranger replication: by default, `ibuffer-toggle-marks` is bound to `t`, and `v` is bound to `ibuffer-do-view`, which opens the buffer at point fullscreen and hides the others. I never need to do that, so I've added

``` {.sourceCode}
    (kbd "v") 'ibuffer-toggle-marks
```

to my config.

I'll add that it's not really necessary to duplicate the entire keymap in your init file. As you'd guess, you only need to explicitly define keys in Evil if:

- Evil would overwrite them otherwise, replacing them with something useless
- You need to change them

If neither of these apply, then you can get rid of the bindings. But pasting them in to start with, then trimming the bindings down to what you're actually interested in gives you the chance to examine the default keymap and rebind things that sound useful.

Now, read [this answer](http://stackoverflow.com/a/3145824) to the StackOverflow question [Emacs: help me understand file/buffer management](http://stackoverflow.com/questions/3145332/emacs-help-me-understand-file-buffer-management). It will familiarize you with filter groups, which will let you narrow down IBuffer's listings to only the buffers you're interested in when working on a project. It'll also help you filter buffers and mark them for performing actions.

## Separating init files

As I said before, putting everything in one init file gets old after a while. Emacs has a variable called `load-path` that you can use to customize where it looks for files. This is similar to the `$PATH` variable in most UNIX-derived shells.

Adding the following to the beginning of your `~/.emacs.d/init.el` file will make Elisp files in the directory `~/.emacs.d/config` available for loading:

``` {.sourceCode}
(add-to-list 'load-path (concat user-emacs-directory "config"))
```

Now you can create a file, `~/.emacs.d/config/my-ibuffer.el`, and move your IBuffer configuration over there. It won't be loadable by default, though, unless you declare it at the bottom of the file:


``` {.sourceCode}
;; code goes here

(provide 'my-ibuffer)
```

Then, in `~/.emacs.d/init.el`, you'll need to `require` the file, as we did with Evil and Slime, for it to be loaded automatically.

``` {.sourceCode}
(require 'my-ibuffer)
```

Now your IBuffer settings will be loaded at startup, but they won't clutter your main init file.

## Dired

[Dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html) is one of Emacs' killer apps - a full-featured file manager built in to the editor. No package installation necessary. But the default bindings in Evil get some core things wrong, and not just in respect to my Vim-like preferences.

Dired is very good about using single-key bindings for the most important things. Changing them to be better is easy: you just have to know what functions to bind.

First, opening a subdirectory in Dired spawns a new Dired buffer. The sane action, `dired-find-alternate-file`, is locked behind a feature gate that will prompt you the first time you use it. I rebound `l` to `dired-find-alternate-file`, which means that moving "forward" into a new Dired buffer happens in the same buffer (or window) as the current one. This does wonders in not cluttering up my buffer list with dead Dired exploration.

Repeating the steps used to build a new keymap for IBuffer, we can hack apart the Dired maps similarily so.

``` {.sourceCode}
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
```

You'll notice the mark toggling is rebound to `v` here as well, which gives me a somewhat consistent interface between `ranger`, Dired, and IBuffer.

Rebinding `n` is another important thing here. By default, `n` and `p` are used for navigation within Dired, but with `hjkl` bindings set that's not really useful. Changing `n` and `N` to Evil's search commands gives us Vim-like searching within the file manager.

### Dired-x

Many incredibly useful Dired features are disabled by default, like `dired-jump`, which jumps to a Dired buffer in the same place as the current file.

Adding the following to your init file will enable these "advanced" features, making Dired a much nicer environment to work in:

``` {.sourceCode}
(require 'dired-x)
```

Some of my Dired-related keybindings, like `dired-jump` below, require `dired-x` loaded to work.

## Evil-Leader

[Evil-Leader](https://github.com/cofi/evil-leader) lets you define a leader key within Evil. I use `,` as my Evil leader key, where `\` is reserved as the leader key for Emacs (per Bling's [Emacs as my \<Leader\>](http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/) post series). As usual, `M-x package-install RET evil-leader` will install this for you.

You should `require` Evil-Leader and run `(global-evil-leader-mode)` *before* Evil is loaded, or otherwise it won't work in buffers like `*scratch*` and `*messages*`.

The first obvious thing to do is to bind `w` and `q` to save and quit functions, as many people do in Vim:

``` {.sourceCode}
(evil-leader/set-leader ",")
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'kill-buffer-and-window)
```

Some other useful Evil-Leader bindings that work with all the packages we've seen so far:

``` {.sourceCode}
(evil-leader/set-key "h" 'dired-jump)
(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "," 'other-window)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "x" 'helm-M-x)
```

With these set, we can eliminate two-key combos in most of our daily editing tasks.

## Use-package

As you start collecting Emacs plugins, your startup times will begin to grow. In addition, your ability to deploy on multiple machines (which may not have those plugins installed) and handle dependencies properly will decrease significantly over time.

[Use-package](https://github.com/jwiegley/use-package) lets you avoid that problem by automating the bad parts of configuring a package, and establishing deferred bindings so that you can evaluate code after a given package is loaded, and trigger a package loading on the evaluation of arbitrary commands or opening of arbitrary filetypes.

With it, the top section of my init file looks like this:

``` {.sourceCode}
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
```

The effect of this is that it automatically downloads and installs `use-package` from MELPA if it isn't already present. Then you can use `:ensure` within a `use-package` declaration to do the same for packages you would normally install manually.

``` {.sourceCode}
(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        ;; bindings from earlier
        )
      )
```

There's pretty detailed help in the README on GitHub, but I'll summarize some of it here:

+------------+--------------+
|**argument**| **meaning**  |
+------------+--------------+
|            |              |
| `:ensure`  | Automatically|
|            |fetch and     |
|            |install the   |
|            |package if it |
|            |is not already|
|            |installed.    |
+------------+--------------+
| `:mode`    | Defer the    |
|            |loading until |
|            |a given file  |
|            |extension is  |
|            |found.        |
+------------+--------------+
| `:commands`| Define the   |
|            |given         |
|            |commands, but |
|            |don't load the|
|            |package until |
|            |they are      |
|            |called.       |
+------------+--------------+
| `:init`    |Command to be |
|            |executed      |
|            |immediatley   |
|            |when the      |
|            |expression is |
|            |found.        |
+------------+--------------+
| `:config`  | Commands to  |
|            |be executed   |
|            |after the     |
|            |package is    |
|            |loaded.       |
+------------+--------------+

The colons are necessary because these are *optional, named arguments* to an Elisp expression. You could use `use-package` with nothing but the first argument, although that wouldn't be very different from using `require`.

Also note that you generally want `:init` and `:config` to be wrapped in a `(progn)`, since they only take one command normally.

## Flycheck

Do

``` {.sourceCode}
(use-package flycheck
  :ensure flycheck
  :init
  (progn
        (add-hook 'after-init-hook #'global-flycheck-mode))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
    )
  )
```

## Magit

I've talked about Magit before. It's the only Git interface I've ever liked for doing actual work, and I found it 3 years after I started using Git.

Giving it proper `jk` bindings is simple:

``` {.sourceCode}
(use-package magit
  :ensure magit
  :config
  (progn
    (evil-define-key 'normal magit-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)))
```

## Tips and tricks

### Avoid using customize

[Easy customization](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html) is a "feature" where Emacs autogenerates lisp code for you, then places that in a specific file with your graphical customizations.

Since this breaks customizing Emacs programmatically, I recommend avoiding it in most cases. It's better to use Customize to browse for useful settings and copy the generated code into your real init files, that way you're free to modify it.

### Ditch the terminal

It's a little-known fact among hardcore [tmux](http://tmux.sourceforge.net/) users that window managers serve a useful purpose besides displaying a web browser. That was how I felt, at least. I used Vim inside the shell, since the graphical version was annoying: it had a different font, clunky menus, and I couldn't use tmux's copy-paste functions to move text around.

If you're moving to Emacs full-time, be prepared to change that mindset. Emacs is better to use in a graphical window. It will actually play nicely with your graphical environment.

Typing `M-x new-frame` opens a new window (in your window manager, not Emacs) that is attached to the same Emacs process. This behavior gives you the best of both worlds: you can use Emacs and split it up however you'd like to, but you can take advantage of the fancy features of your [window manager](/software-choice.html#window-manager).

Just trust me. Give it a shot. I promise it's better.

### Hide the startup messages

``` {.sourceCode}
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
```

### After

Useful when managing package dependencies and controlling load order. Taken from [milkbox](http://milkbox.net/note/single-file-master-emacs-configuration/):

``` {.sourceCode}
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))
```

### Better word wrapping

Emacs has a long history of being terrible at word wrapping. The situation has improved lately, however, and visual line mode gives you sane word wrapping in almost all cases.

``` {.sourceCode}
(visual-line-mode 1)
```

### Enforce trailing newlines

Some software (e.g., [newsbeuter](http://www.newsbeuter.org/)) breaks if you don't provide a trailing newline.

``` {.sourceCode}
(setq require-final-newline t)
```

### Hide the toolbar

The toolbar is useful at first, but once you're familiar with the major mode you're working in, you might want to disable it to preserve vertical space.

``` {.sourceCode}
(tool-bar-mode -1)
```

[^hooks]: Often you'll see people defining hooks using a `lambda` construct. This is counterproductive, since once you add a hook to a list it can be complicated to get it out. If you messed anything up with your lambda, you can have a hard time removing it.
Giving all of your hooks explicit names lets you remove them easily with `remove-hook` without restarting your Emacs session.

[^emacs-state]: `evil-emacs-state` uses Emacs keybindings exclusively.

[^symbols]: Note that not everything in a given buffer is a *symbol*, even though everything in Elisp is an *object*. Since you were looking up a function definition, it *was* a symbol, but symbol refers exclusively to those objects that have names.

[^turn-on-eldoc]: As of Emacs 24.4, `turn-on-eldoc-mode` is a deprecated alias for `eldoc-mode`, which should be used instead.

[^ibuffer-errs]: I got errors about the `R` in `R E T` not being a valid prefix key when I was testing this. YMMV.

[^ibuffer-refresh]: ibuffer does not refresh itself by default. You need to call `ibuffer-update` for new buffers to be shown.
