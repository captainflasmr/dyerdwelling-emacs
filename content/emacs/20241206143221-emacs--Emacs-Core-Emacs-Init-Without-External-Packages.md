---
title: "Core Emacs Init Without External Packages"
author: ["James Dyer"]
lastmod: 2024-12-06T15:00:00+00:00
tags: ["emacs-core", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241206143221-emacs--Emacs-Core-Emacs-Init-Without-External-Packages.jpg"
---

I thought I would share a simple concept that I have found very beneficial: creating an Emacs init file that remains almost fully functional for my use but doesn't include any external packages!

<!--more-->

{{< figure src="/emacs/20241206143221-emacs--Emacs-Core-Emacs-Init-Without-External-Packages.jpg" width="100%" >}}

This setup benefits two main use cases. The first is when using Emacs at work without internet access, and the second is configuring it for use on Windows (with an internet connection). For some reason, my Windows version always seems to struggle to connect to MELPA/ELPA for package downloads, prompting me to resort to an initial complete local repository download. Even then, I still encounter some inconsistencies. I don't often use the native Windows Emacs version anyway, and now that WSL is mature enough, I know a fully functioning version of Emacs can run seamlessly.

Anyway enough with the blithering.

I have realized that over the years, I have accumulated many Elisp snippets that can actually replace several packages I have downloaded from MELPA.

For example, while I couldn't live without **ace-window**, I only primarily use it for the window-jumping functionality. Now that I am familiar with the typical positions of the key identifiers in my window layout, I thought I would simply write my own implementation in Elisp:

```elisp
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
  Windows are labeled starting from the top-left window and proceed top to bottom left to right."
  (interactive)
  (let* ((window-list (my/get-windows))
         (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                (length window-list)))
         (window-map (cl-pairlis window-keys window-list))
         (key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
    (if-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
        (select-window selected-window)
      (message "No window assigned to key: %c" key))))

(defun my/get-windows ()
  "Return a list of windows in the current frame, ordered from top to bottom, left to right."
  (sort (window-list nil 'no-mini)
        (lambda (w1 w2)
          (let ((edges1 (window-edges w1))
                (edges2 (window-edges w2)))
            (or (< (car edges1) (car edges2)) ; Compare top edges
                (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                     (< (cadr edges1) (cadr edges2))))))))

(global-set-key (kbd "M-a") #'my/quick-window-jump)
```

So the `ace-window` jumping functionality is now usable in an environment without access to MELPA!

I thought I would present my "no package" Emacs config piece by piece one blog post at a time, showcasing 90% of the functionality I typically use day-to-day and how I have managed to either write some elisp, found out a built-in option, or adapted my workflow in some way.

I still have my "full-fat" version, of course, but I have now based it on the `core` version (hence the name) and added extra things like `magit`, etc.

However, `magit` is a good example on something I think I might be able to remove soon to switch to the built-in Emacs VC. I successfully use it at work with both Git and Subversion!

If you are interested in seeing the full "no fat" version, have a ganders at: <https://github.com/captainflasmr/Emacs-core>

There will be more to come on this topic in the coming weeks... 😀
