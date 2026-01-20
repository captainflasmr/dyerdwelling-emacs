---
title: "Replacing deadgrep with consult-ripgrep"
author: ["James Dyer"]
lastmod: 2023-03-08T21:08:00+00:00
tags: ["ripgrep", "grep", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230225134207-emacs--Trying-out-consult-ripgrep-to-replace-deadgrep.jpg"
---

I have been evolving my way through many differing ways of grepping recently from standard built-in greps to a few ripgrep front ends until I finally settled on `deadgrep`

<!--more-->

{{< figure src="/emacs/20230225134207-emacs--Trying-out-consult-ripgrep-to-replace-deadgrep.jpg" class="emacs-img" >}}

I am currently an **ivy** user but as everyone seems to be talking about
**vertico** and the associated completion stack I thought I would give it a
try and therefore `consult-ripgrep` to see it can improve on my `deadgrep`
setup.

Previously I have created a couple of wrappers around deadgrep:

```elisp
(defun my/deadgrep ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (setq search-term
            (read-from-minibuffer "Search : "))
    (setq search-term
          (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
    )
  (deadgrep search-term home-dir)
  )

(defun my/grep ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (setq search-term
            (read-from-minibuffer "Search : "))
    (setq search-term
          (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
    )
  (deadgrep search-term)
  )
```

As a universal argument was passed to deadgrep which had a side effect of initially pausing the search, this meant I split my grepping between `S-f12` and `M-f12` one for a project grep and one for a local directory grep.

As part of trying out `consult-ripgrep` I think I would like to rewrite the functions above and I would want a quick and simple method to revert back to using deadgrep.

I settled on the following:

```elisp
(defun my/project-root ()
  (interactive)
  "Guess the project root of the given FILE-PATH."
  (let ((root default-directory)
        (project (project-current)))
    (when project
      (cond ((fboundp 'project-root)
             (setq root (project-root project)))))))

(defun my/grep (arg)
  (interactive "p")
  (if (equal major-mode 'dired-mode)
      (setq search-term
            (read-from-minibuffer "Search : "))
    (setq search-term
          (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
    )
  (if (> arg 1) ;; if C-u has been activated
      (consult-ripgrep default-directory search-term)
    (consult-ripgrep (my/project-root) search-term)))
;;   (progn
;;     (setq current-prefix-arg nil)
;;     (deadgrep search-term default-directory)
;;     )
;; (deadgrep search-term (my/project-root))))
```

To switch between deadgrep / consult-ripgrep I just need to uncomment / comment in and out the relevant bits and then re-evaluate.

As you can see I created a local function to find the project root directory if one exists and to then pass the result to the deadgrep / consult-ripgrep command meaning that they will both perform identically from a search directory perspective.

Note how I overcame the pass-through of the universal argument to deadgrep by just resetting the `current-prefix-arg` after I had already used the universal argument logic within the function.

I can now free up my original `M-f12` binding which searched from the local `default-directory` as I can `C-u` to the `S-f12` binding.

I'm not too sure about consult-ripgrep yet but the setup above gives me the following benefits:

1.  A common local find project root function that might become useful for future functions
2.  Flexibility with the universal argument meaning I could pass in multiple
    universal arguments or maybe even a numeric argument for enhanced
    functionality
3.  Able to quickly switch between ripgrep implementations by commenting / un-commenting
4.  Frees up a keybinding
