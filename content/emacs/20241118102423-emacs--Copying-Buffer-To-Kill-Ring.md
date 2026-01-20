---
title: "Reducing Friction when Copying Whole Buffer To Kill Ring"
author: ["James Dyer"]
lastmod: 2024-11-18T10:24:00+00:00
tags: ["emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241118102423-emacs--Copying-Buffer-To-Kill-Ring.jpg"
---

Just a quick one today.

In keeping with the ongoing effort to reduce friction in the venerable Emacs text editor, I realized that a common action I often perform is copying the entire contents of the buffer, usually for pasting elsewhere.

<!--more-->

To perform this I have chained together some emacs commands, namely `(mark-whole-buffer)` and then `(kill-ring-save)`

{{< figure src="/emacs/20241118102423-emacs--Copying-Buffer-To-Kill-Ring.jpg" width="100%" >}}

The problem with pushing the buffer to the kill ring in this manner is that I lose the current cursor position/point and end up using `isearch` to navigate my way back. Strangely, it is only recently that I have found this annoying!

There are a few options to address this:

-   Use Emacs marks
-   Create a macro
-   Create a defun

Initially I tried the setting `mark` option meaning that I `C-<SPC> C-<SPC>` to set the mark at the current position and then `C-u C-<SPC>` to pop back to my previous mark set.  The only issue is that `(mark-whole-buffer)` creates a mark at the end of the selected region and my first mark pop would be to this position, so I have to mark pop again.

The benefit of this approach is that I will start becoming more familiar with setting marks and navigating more efficiently within Emacs, which I really think I should learn. However, it all feels a little clunky, and you know what? I'm just going to write a simple elisp defun and bind it.

`save-excursion`, in this case, can be extremely useful!

```elisp
(defun my/copy-buffer-to-kill-ring ()
  "Copy the entire buffer to the kill ring without changing the point."
  (interactive)
  (save-excursion
    (kill-ring-save (point-min) (point-max))))

(bind-key* (kbd "M-s z") #'my/copy-buffer-to-kill-ring)
```
