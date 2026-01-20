---
title: "Cut / Copy between Windows using Dired Buffers"
author: ["James Dyer"]
lastmod: 2022-09-26T00:00:00+01:00
tags: ["emacs", "elisp", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2022-09-26_13-39.jpg"
---

The next step in my emacs journey is to move files around a little more easily, in fact more like a regular file explorer using the concept of file / folder selection copy and paste.  That concept seems a little more natural to me than `dired` file marking, renaming / copying and then entering the path of the destination address.

<!--more-->

{{< figure src="/emacs/2022-09-26_13-39.jpg" class="emacs-img" >}}

After a little research the following options present themselves:

1.  Vanilla Dired
    -   copy target directory to kill ring using `0 w`
    -   mark source files
    -   copy or rename files and at the directory destination prompt paste in from
        the kill ring
2.  package Dired+
3.  Own lisp implementation
4.  package Ranger like
5.  DWIM

One main criteria I have for this functionality is that if possible I want to avoid installing any additional packages as I always initially try and look for a way using vanilla emacs.

I have emacs on windows, at work and of course on Manjaro, all slightly different versions and I find relying on third party packages can cause a few problems especially on my work emacs where I have installed potentially incompatible versions of MELPA and emacs.  I always like to keep a common as possible initialisation file if possible across all my emacs.

So the choice is between options 1,3 or

1.

Trying number 1 worked well enough but still felt a little clunky and I really wanted to avoid copying and pasting directory names.  The number 5 **do what I mean** concept seems decent enough but seems to mainly rely on opening a window side by side which restricts me as I am a serial window chopper!.  Also I have in the back of my mind to explore the whole DWIM concept in the future, especially when related to running commands on selected files.

Therefore the winner is number 3.  "my own" lisp implementation.  Of course the quotes mean that I am going to borrow and then modify an answer I found on the internet!

The original lisp is as follows

```elisp
(defvar your-dired-copy-list nil)

(defun your-dired-copy ()
  (interactive)
  (setq your-dired-copy-list (dired-get-marked-files)))

(defun your-dired-paste ()
  (interactive)
  (when your-dired-copy-list
    (shell-command
     (mapconcat
      #'shell-quote-argument
      `("cp" "-r" ,@your-dired-copy-list ,default-directory)
      " "))
    (setq your-dired-copy-list nil)))
```

and then to `define-key` add to the `dired-mode-map`

I feel that my emacs journey is creeping ever onwards and my eyes and heart are opening up to creating my own lisp functions that do things that I want emacs to do.  This is a bit of an opportunity to poke a gentle toe into the clear crystal waters of lisp programming.

I have already been exposed to lisp at university although at almost 30 years ago now.  It maybe that it will be more of the 25 years of software engineering experience that will help me now!

I think I understand the initial implementation, and I like the concept of leveraging a shell command and hence the functionality of the underlying operating system.  But I also wish to move files and directories and not just copy them, so I need some form of flag on the **dired** marked files action and inform the paste routine which command I wish to perform.  So I created / modified my first lisp code as thus:

```elisp
(defvar my-dired-copy-list nil)

(defun my-dired-copy ()
  (interactive)
  (setq my-move-flag nil)
  (setq my-dired-copy-list (dired-get-marked-files)))

(defun my-dired-move ()
  (interactive)
  (setq my-move-flag t)
  (setq my-dired-copy-list (dired-get-marked-files)))

(defun my-dired-paste ()
  (interactive)
  (when my-dired-copy-list
    (if my-move-flag
        (shell-command
         (mapconcat
          #'shell-quote-argument
          `("mv" "-f" ,@my-dired-copy-list ,default-directory)
          " ")
         )
      (shell-command
       (mapconcat
        #'shell-quote-argument
        `("cp" "-r" ,@my-dired-copy-list ,default-directory)
        " "))
      )
    (setq my-dired-copy-list nil)
    )
  )
```

and with the following key defines:

```elisp
(define-key dired-mode-map "C" 'my-dired-copy)
(define-key dired-mode-map "R" 'my-dired-move)
(define-key dired-mode-map "Y" 'my-dired-paste)
```

So I just set a simple flag depending on if the file is to be moved or not and then IF'd my way to the relevant functionality.  Using `M` to move a file felt more natural but I kept the `dired` renaming notation of `R` for consistency and to avoid confusion with changing the marked files mode.

This implementation has the side effect of removing the simple **dired** single file rename functionality, but as I generally use `wdired` anyway I don't think this is too much of a problem.
