---
title: "Improvements to Selected Window Accent"
author: ["James Dyer"]
lastmod: 2024-01-01T21:12:00+00:00
tags: ["window", "selected-window-accent-mode", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231221210441-emacs--Selected-Window-Accent.jpg"
---

Given my previous post regarding accenting the current window:

<!--more-->

[Selected Window Accent]({{< ref
"/emacs/20231221210441-emacs--Selected-Window-Accent.md" >}})

> "This routine provides a coloured left fringe accent on the selected window to emphasize the current working window."

I had a little issue with `visual-fill-column-mode` in that :

> Note that I also had to set the margins to make the window traversal less visually disturbing and it seems to affect `visual-fill-column-mode` in the fact that it doesn't work, but maybe I can just try and not use it or eventually figure it out, but for now this is a starting point and I'm sure I will refine it over time.

With some trial and error the issue was that my new accent routine applied margins that overrode `visual-fill-column-mode`, causing display issues on window navigation.

So to fix I just re-activated `visual-fill-column-mode` if it was active.

```elisp
(defun selected-window-accent ()
  (interactive)
  (set-face-background 'fringe "#77002e")
  (walk-windows
   (lambda (window)
     (if (eq window (selected-window))
         (progn
           (set-window-margins window 1 0)
           (with-selected-window window
             (if (eq visual-fill-column-mode t)
                 (visual-fill-column-mode t)))
           (set-window-fringes window 10 0 t nil))
       (progn
         (set-window-margins window 2 0)
         (with-selected-window window
           (if (eq visual-fill-column-mode t)
               (visual-fill-column-mode t)))
         (set-window-fringes window 0 0 t nil))
       )
     )
   nil t))

(add-hook 'window-configuration-change-hook 'selected-window-accent)
(add-hook 'window-state-change-hook 'selected-window-accent)
```

{{< figure src="/emacs/20231221210441-emacs--Selected-Window-Accent-2.jpg" width="100%" >}}
