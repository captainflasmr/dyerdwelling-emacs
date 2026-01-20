---
title: "Simple Flexible Scrolling"
author: ["James Dyer"]
lastmod: 2023-02-08T21:21:00+00:00
tags: ["scrolling", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230205185836-emacs--Simple-Flexible-Scrolling.jpg"
---

I have written before about smooth scrolling using **good-scroll** and how I managed to find a semi satisfactory way of centering my cursor after a single scroll which meant I would then have a minimal amount of subsequent line movement to get to the line I want.

<!--more-->

{{< figure src="/emacs/20230205185836-emacs--Simple-Flexible-Scrolling.jpg" class="emacs-img" >}}

However, I have since returned to a simple concept, more of a basic `scroll-[up/down]-command` which in its bare form scrolls a whole page.  Now for me this is too much and I still want to have the ability to control how many lines I scroll which will still indicate to my brain that text is scrolling so I can still take in the overall form of a file.

I initially returned to my simple scrolling functions:

```elisp
(defun scroll-up-some ()
  (interactive)
  (scroll-up-command (window-some-height)))

(defun scroll-down-some ()
  (interactive)
  (scroll-down-command (window-some-height)))

(defun window-some-height ()
  (max 1 / (1- (window-height(selected-window))) 4))
```

which leveraged the number of lines as an argument to the `scroll-[up/down]-command`

I would however really like to simplify and still allow some centering, I therefore created the following bindings:

```elisp
(bind-key* (kbd "M-j")
           (lambda()
             (interactive)
             (next-line (/ (window-height) 12))
             (recenter)))

(bind-key* (kbd "M-k")
           (lambda()
             (interactive)
             (previous-line (/ (window-height) 12))
             (recenter)))
```

The only issue I have with this implementation is that when I move to the end of a file and scroll upwards the first scroll `recenter` is a bit jarring as moving to the `end-of-buffer` is not typically centred and has any number of blank "lines" below it.  Moving to `beginning-of-buffer` has no such issue as the cursor moves right to the top and subsequent scroll down recenters do not activate any scrolling until the cursor is past the center point of the window.

This issue however is easily fixed by:

```elisp
(bind-key* (kbd "M->")
           (lambda()(interactive)(end-of-buffer)(recenter)))
```

which forces an initial `recenter` whenever an `end-of-buffer` keybinding is activated, meaning that the first scroll upwards is now seamless!
