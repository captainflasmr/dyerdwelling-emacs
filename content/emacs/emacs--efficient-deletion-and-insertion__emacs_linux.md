---
title: "Efficient Deletion and Insertion"
author: ["James Dyer"]
lastmod: 2022-09-10T00:00:00+01:00
tags: ["emacs", "elisp", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--efficient-deletion__emacs_linux.jpg"
---

As my emacs keybindings journey continues to evolve and to delete more efficiently with delete word it has lead to an interesting issue for me.

<!--more-->

{{< figure src="/emacs/emacs--efficient-deletion__emacs_linux.jpg" class="emacs-img" >}}

Typically, especially in my coding life I will typically copy a string to the clipboard, then go in and delete the target area and insert from the clipboard/kill-ring.  Now I have moved from deleting with the delete key to the the M-delete-word concept it seems that deleting words automatically puts this to the clipboard/kill-ring, so when I paste in my original copy it pastes back in the last killed words.

Generally I only ever use kill-line when I want a kill to be copied to the kill-ring and I only ever use the word deletion to just delete in preparation for a yank.

A bit of hunting around on the Interwebs lead to the definition of two new functions to be remapped to avoid the kill-ring.

```elisp
(defun delete-word-back (arg)
  "Delete characters backward until encountering the beginning of a word.
     With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-word-forward (arg)
  "Delete characters forward until encountering the end of a word.
     With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
```
