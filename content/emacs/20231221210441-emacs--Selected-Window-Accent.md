---
title: "Selected Window Accent"
author: ["James Dyer"]
lastmod: 2023-12-22T20:55:00+00:00
tags: ["window", "selected-window-accent-mode", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231221210441-emacs--Selected-Window-Accent.jpg"
---

Borrowing from the concept of a tiling window manager I thought that emacs could better indicate the currently selected window by a form of highlight/emphasis like the following:

<!--more-->

{{< figure src="/emacs/20231221210441-emacs--Selected-Window-Accent-2.jpg" width="100%" >}}

> "This routine provides a coloured left fringe accent on the selected window to emphasize the current working window."

Since I have been using more of an emacs tab-bar based work-flow I found myself spending a second or two looking for my current window when switching tabs.   I would therefore like to more clearly visually identify the current working window (i.e, which window has my cursor in it) by the aid of some kind of accent or emphasis as is often the case in a tiling window manager.   A blinking cursor could help with this, but I tend to prefer a non blinking block and I find `hl-line-mode` too distracting.

I thought I would just cobble together a proof of concept, nothing fancy, programatically inefficient, but hopefully an effective solution that works well enough while adhering to the KISS principle.

---

There are packages out there, for example `pulsar` and `beacon` for pulsing or emphasizing the cursor area when window navigating or `auto-dim-other-buffers` which face dims aspects of the entire window set for emphasis and actually I like its description:

> The ‘auto-dim-other-buffers-mode’ is a global minor mode which makes windows
> without focus less prominent.  With many windows in a frame, the idea is that
> this mode helps recognise which is the selected window by providing
> a non-intrusive but still noticeable visual indicator.

which is pretty much what I am looking to achieve but by using an accent rather than dimming.

So the solution?, overlays didn't seem to fit and was more character point font based where I was initially looking to render some form of coloured graphical border overlay which is typically apparent in a standard tiling manager.

After a little investigation I decided on a simple (quick and dirty) solution and that is to utilise the left hand fringe.

I generally don't ever have fringes in my emacs session and in fact set `(set-fringe-mode '(0 . 0))`.

The function `set-fringe-margins` allows a left and right pixel width value to be set for an individual window which is a good starting point.  After a little investigation and trial and error I came up with the following which seemed to work well for me:

```elisp
(defun my/selected-window-accent ()
  (interactive)
  (set-face-background 'fringe "#77002e")
  (walk-windows
   (lambda (window)
     (if (eq window (selected-window))
         (progn
           (set-window-margins window 1 0)
           (set-window-fringes window 16 0 t nil))
       (progn
         (set-window-margins window 3 0)
         (set-window-fringes window 0 0 t nil))
       )
     )
   nil t))

(add-hook 'window-state-change-hook 'my/selected-window-accent)
```

On a window change notification (for example, standard window navigation) all windows are traversed with the current window explicitly displaying the left fringe only to a defined colour.  All the other non selected windows have their fringes removed or in this case the pixel sizes set to zero.

What we end up with here is a window accent on the left hand side of the selected window and when in combination with potentially a sensible mode-line configuration (maybe in the same accent colour) I think will serve as a decent visual window indicator.  The width and colour can be simply modified to taste within the function itself, this is a very simple solution.

Note that I also had to set the margins to make the window traversal less visually disturbing and it seems to affect `visual-fill-column-mode` in the fact that it doesn't work, but maybe I can just try and not use it or eventually figure it out, but for now this is a starting point and I'm sure I will refine it over time.
