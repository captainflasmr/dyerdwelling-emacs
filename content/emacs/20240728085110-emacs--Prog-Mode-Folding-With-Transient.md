---
title: "Prog-Mode Folding With a Transient"
author: ["James Dyer"]
lastmod: 2024-07-28T08:51:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240728085110-emacs--Prog-Mode-Folding-With-Transient.jpg"
---

I'm looking on ways to enhance my Emacs coding experience and especially regarding the use of cmake and folding.  More on cmake in a future post, but regarding the folding aspect, my interest was rekindled when I looked into a new package called `outline-indent`, in principle it seems like a simple and cool idea to leverage the `outline-mode` to program fold based on indentation levels.

<!--more-->

{{< figure src="/emacs/20240728085110-emacs--Prog-Mode-Folding-With-Transient.jpg" width="100%" >}}

Of course org heavily leans on `outline-mode` for much of its folding and I've been using &lt;TAB&gt; and &lt;S-TAB&gt; for quite a while which toggle cycles headings, although less on the navigation aspect as I rely on org speed commands.

A while ago I found some simple agnostic folding elisp using `selective-display` :

```elisp
(defun my/fold ()
  "Fold text indented same of more than the cursor."
  (interactive)
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (1+ (current-column)))))
```

Which folds everything in the buffer based on the cursor position and just toggles back and forth.  I found this pretty cool when I found it but I do keep forgetting about it, well now I am going to try and go full throttle on some agnostic level folding in source code I might bring this into the general concept introduced by `outline-indent`

At the moment I'm having fun defining transients, I like the grouping of any set of commands together which is presented as a simple text menu and especially those infrequently used and hence forgotten.

So my idea here is to allow agnostic code folding on spaces and tabs (as my source code is always consistently indentally formatted), based on `outline-mode` along with an `outline-mode` transient menu which will also include extra cursor position folding using `selective-display`

I could use `outline=indent` to help me out here but I thought as just a general exercise to learn some more elisp and have fun defining a transient (which could be easily turned into an all purpose `outline-mode` transient).  Although I am using the following taken from `outline-indent` to enable the space / tab outline indenting mechanism:

```elisp
(setq-local outline-regexp (rx bol
                               (zero-or-more (any " \t"))
                               (not (any " \t\n"))))
```

I'm not too clear as to how much `outline-mode` is generally leveraged in different programming modes, and presumably based on different program specific syntactical structures.  The languages that I mainly use seem to for example define their own navigation system, and straight using `outline-mode` and especially then turning on outline cycling is problematic given the &lt;TAB&gt; use within general source code and hence the outline conflicting binding.

So given all these thoughts, this is what I have have so far:

```elisp
(transient-define-prefix my/transient-outlining-and-folding ()
  "Transient menu for outline-mode."
  ["Outline Mode Commands"
   ["Cycle / Folding"
    ("g" "Cycle" outline-cycle)
    ("O" "Cycle Buffer" outline-cycle-buffer)
    ("F" "Global Folding at Point"
     (lambda () (interactive)
       (if (eq selective-display (1+ (current-column)))
           (set-selective-display 0)
         (set-selective-display (1+ (current-column))))))]
   ["Visibility"
    ("o" "Toggle Children" outline-toggle-children)
    ("h" "Hide Sublevels" outline-hide-sublevels)
    ("s" "Show All" outline-show-all)
    ("i" "Hide Body" outline-hide-body)
    ("e" "Show Entry" outline-show-entry)
    ("H" "Hide Entry" outline-hide-entry)
    ("c" "Hide Leaves" outline-hide-leaves)
    ("k" "Show Branches" outline-show-branches)
    ("t" "Hide Subtree" outline-hide-subtree)
    ("S" "Show Subtree" outline-show-subtree)]
   ["Motion"
    ("n" "Next Visible Heading" outline-next-visible-heading)
    ("p" "Previous Visible Heading" outline-previous-visible-heading)
    ("u" "Up Heading" outline-up-heading)
    ("f" "Forward Same Level" outline-forward-same-level)
    ("b" "Backward Same Level" outline-backward-same-level)]
   ["Structure"
    ("t" "Promote Heading" outline-promote)
    ("d" "Demote Heading" outline-demote)
    ("P" "Move Subtree Up" outline-move-subtree-up)
    ("N" "Move Subtree Down" outline-move-subtree-down)]
   ["Edit"
    ("a" "Add Heading" outline-insert-heading)
    ("r" "Rename Heading" outline-insert-heading)
    ("m" "Mark Subtree" outline-mark-subtree)]])

(bind-key* (kbd "C-c o") 'my/transient-outlining-and-folding)

(defun my/prog-folding ()
  "Enable and configure outline minor mode for code folding.

This function sets up the outline minor mode tailored for
programming modes based on basic space / tab indentation."
  (interactive)
  (setq-local outline-minor-mode-use-buttons nil)
  (setq-local outline-regexp (rx bol
                                 (zero-or-more (any " \t"))
                                 (not (any " \t\n"))))
  (outline-minor-mode 1))

(add-hook 'prog-mode-hook 'my/prog-folding)
```

As you can see, the main transient is globally defined so can be used in anything that has `outline-mode` enable, for example org!

But sneakily also has  "Global Folding at Point" for some cursor position folding.

Often and when in a source code body I would really like to just get a top level outline of all the functions available, this should be always doable with  "Global Folding at Point" for just about any language as long as the cursor is positioned accordingly.  "Cycle Buffer" would mostly perform a similar function but could run into difficulties if newlines were just after function headers.

Also I often program in Ada and for procedure or function definitions the indent structure is not that compatible with an outline methodology and would start folding local variables and function / procedure bodies separately.  For Ada I would almost always used  "Global Folding at Point" and probably I'm sure for some other languages.  Well maybe the transient now caters for most programming folding?

Now the big question is... will I actually use the above? and then build it into my workflow / muscle memory? I have survived this far without code folding, and annoyingly is my recent development shennanigans in Visual Studio skewing / adjusting my expectations in an IDE?, do I really want this?, is it helpful? who knows, and actually it doesn't matter I have learned much about `outline-mode` regex using rx, transients, e.t.c
