---
title: "Streamlining eshell with popper, capf-autosuggest, and Enhanced Autocompletion"
author: ["James Dyer"]
lastmod: 2024-08-30T11:20:00+01:00
tags: ["eshell", "emacs", "elisp", "capf", "cape", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy.jpg"
---

In this post, I will describe the small enhancements and tweaks I have applied to `eshell` to make it feel more like a typical linux terminal using the fish shell and especially the inline autosuggestion feature that fish has out-of-the-box.

<!--more-->

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-27-30.jpg" width="100%" >}}

For clarity, the autosuggestions in fish enable autocompletion suggestion previews as you type, for example :

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-52-32.jpg" width="100%" >}}

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-52-48.jpg" width="100%" >}}

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-52-57.jpg" width="100%" >}}

Fish will suggest a single command to the right of the cursor dynamically in grey, its just like the terminal built-in readline C-r/C-s, but inline and for your most recent command based on the text input. As a bonus of course, fish will have readline built in and therefore a familiar Emacs keybinding `C-e` will complete the suggested completion for you!

Over the years, I have progressed through possibly all the terminal shells, from csh and ksh through to bash and zsh, and now I use fish. Similarly to the popularity of more native-based light-weight Emacs plugins, such as vertico, corfu, and cape, the fish shell is lightweight and targets important functions right out of the box.

For years, I relied on readline shell functionality, with the keybinding C-r bringing up a command history which I could navigate. It is much like vertico, and although efficient, I now prefer the fish method of inline completion autosuggestion preview as I feel I can get to my desired commands faster.

With eshell, there is the native completion TAB based system but it is clunky, so lets see how we can make it more fishy! 😀 🐠


## Inline Fish-like autosuggestion {#inline-fish-like-autosuggestion}

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-07-51-34.jpg" width="100%" >}}

I'm going to dive straight in to enabling inline autosuggest preview for `eshell` to emulate Fish-like inline completion as described earlier.  I use a package called `capf-autosuggest`:

```elisp
(use-package capf-autosuggest
  :hook
  (eshell-mode . capf-autosuggest-mode))
```

There are other similar packages out there, but so far I have found this to be the best and seems to work out-of-the-box. It is shell (comint) based, so it won't work anywhere else in Emacs, but that is fine for me as I have plenty of other completion mechanisms to choose from.

Of course you will need to have an `eshell` history to use effectively, but it will also autosuggest commands found on your PATH.

_Note : that as of Emacs 30 there is a new function - `completion-preview-mode` which looks as though it does something similar to `capf-autosuggest` but for all buffers!_

> This mode automatically shows and updates the completion preview according to the text around point.

I haven't tested this fully yet but when Emacs 30 comes out I will probably have a closer look.


## Creating Custom eshell Buffers {#creating-custom-eshell-buffers}

There are other `eshell` tweaks I make to generally improve my workflow and I will now describe how I create eshell buffers. Below is a function that allows the creation of eshell buffers with defined names, making it easier to manage multiple shell sessions.

```elisp
(defun my/shell-create (name)
  "Create a custom-named eshell buffer with NAME."
  (interactive "sName: ")
  (eshell 'new)
  (let ((new-buffer-name (concat "*eshell-" name "*")))
    (rename-buffer new-buffer-name t)))

(global-set-key (kbd "M-o s") #'my/shell-create)
```

The reason for this is that I gain greater control over eshell buffer naming, which makes using tools like Popper (see below) and creating multiple eshells simpler. For example, opening an initial eshell in Emacs is easy, just run `eshell`, however, to create another unique eshell, you must remember to pass in a universal argument (which I always forget); otherwise, you will just switch to the existing eshell.  So this function will always create a new shell, I can always switch between eshell buffers as normal through Emacs anyway.


## Toggling eshell with Popper {#toggling-eshell-with-popper}

Popper provides a pop-up window manager, making it easy to toggle eshell in and out of view. It has been a recent discovery, but now I can't live without it.

```elisp
(use-package popper
  :init
  (setq popper-reference-buffers
        '("\\*eshell.*"
          flymake-diagnostics-buffer-mode
          help-mode
          compilation-mode))
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-window-height 15))

(bind-key* (kbd "C-;") #'popper-toggle)
```

Since I control my eshell naming, I can set up Popper with the requisite regex to pop up eshell when it is opened or created.

What I like about Popper is that it does what it says: it pops windows in and out. I also like to pop Flymake, Help, and Compilation windows, this keeps Emacs clean and uncluttered, and if, for example, I build something, if I need a terminal, or need some linting diagnostics, I can just quickly pop in and out and switch between each pop.

As a note, if more than one window has been "poppered" then switching between each "pop" is easy through a keybinding M-&lt;number&gt;, which is something I also use all the time.


## General eshell Configuration {#general-eshell-configuration}

Here is my general eshell setup. I don't think there is anything controversial here, but as you can see, I have enabled a hook to my own shell setup function, and yes, I know I could add capf-autosuggest-mode to this, but for clarity in this post, I kept it specifically in the capf-autosuggest section.

```elisp
(use-package eshell
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (setq-local tab-always-indent 'complete)
  (setq eshell-history-size 10000)
  (setq eshell-save-history-on-exit t) ;; Enable history saving on exit
  (setq eshell-hist-ignoredups t) ;; Ignore duplicates
  :hook
  (eshell-mode . my/eshell-hook))
```

Yes, I know, the history size is ridiculous! (default is 128) but why not build up a huge collection of my past commands for a more robust autosuggest mechanism?!


## eshell completion using consult {#eshell-completion-using-consult}

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-28-45.jpg" width="100%" >}}

What about other completion mechanisms?, for example emulating the C-r/C-s readline functionality?

For a while I used consult to bring up a `completing-read` searchable eshell history via `consult-history`, and although it does work well I found I wasn't really using it. However, I do still have it enabled:

```elisp
(define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))
```


## UI completion with Corfu and Cape {#ui-completion-with-corfu-and-cape}

Now for some completion UI setup to augment the capf-autosuggest functionality - we are in Emacs after all!

In this case I am choosing to use `corfu` but `company` is another option.  The idea here is to bring up a typical `corfu` completion popup UI when typing, for example:

{{< figure src="/emacs/20240827210257-emacs--Enhancing-Eshell-To-Be-More-Fishy/2024-08-30-08-27-30.jpg" width="100%" >}}

Here you can see it working along-side `capf-autosuggest`

Here is the config:

```elisp
(use-package cape)

(defun my/eshell-hook ()
  "Set up eshell hook for completions."
  (interactive)
  (setq-local completion-styles '(basic partial-completion))
  (setq-local corfu-auto t)
  (corfu-mode)
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'pcomplete-completions-at-point
                     #'cape-history)))
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))
```

I decided to have a simple set of completion styles, none of those fancy fuzzy, orderless, flex shenanigans as I can generally string a few characters of the command I want to use to quickly generate a suitable matching command.

`cape` at this point becomes useful as it exposes the function `cape-history` to `corfu`.  Once set up, typing in eshell now brings up the completion popup and you would use as expected.  This for me has now replaced `consult-history` as it feels more integrated and faster for my workflow while producing the same functionality.

_Note: I have explicitly turned on corfu autocompletion as in general I like to have it turned off. I have learnt that things can get tricky and laggy with autocompletion turned on when using eglot and communicating over an LSP, so I prefer to bind to a key and call the UI on demand.  Also I'm a little old school in that I don't like a busy work window, I'm not used to seeing a dynamic autocompletion appear when I am coding and would rather manually leverage this functionality when required._


## Combining all of it! {#combining-all-of-it}

The `corfu` functionality described above can run nicely along-side `capf-autosuggest`.  For some, this may seem cluttered and you may want to choose one or the other, but for me, for the moment, this suits my workflow.


## Conclusion {#conclusion}

By integrating `popper`, `capf-autosuggest`, `corfu`, and `cape` into my eshell setup, I think I may have finally been able to almost completely fully transition terminal usage to emacs through `eshell`.

In addition, if I am running Emacs on a system that doesn't have the fish shell available, for example windows, or a restricted linux development environment, then eshell can now increase my efficiency on the command line.
