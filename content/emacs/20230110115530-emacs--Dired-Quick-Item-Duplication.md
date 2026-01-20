---
title: "Dired Duplicate Here"
author: ["James Dyer"]
lastmod: 2023-01-10T00:00:00+00:00
tags: ["emacs", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230110115530-emacs--Dired-Quick-Item-Duplication.jpg"
---

`dired` can do most things for me especially now I have my DWIM image conversion scripts working and `image-dired` configured to my liking.

<!--more-->

{{< figure src="/emacs/20230110115530-emacs--Dired-Quick-Item-Duplication.jpg" class="emacs-img" >}}

However sometimes I do just prefer using a GUI file manager; for example, drag and drop, image file preview e.t.c.

I am currently using the Dolphin file manager on KDE and one context menu service that I have enabled and use often is the **Duplicate Here** function.  It is often that I like to back up a file / directory to a quick temporary directory especially when I am hacking around with the original and on windows it is often that I copy and then paste with Windows automatically applying a `Copy` to the original name.  Dolphin does something similar but applies a `(1)` when it suggests a new name.

Enough rambling!, simply I would like to reproduce this functionality in `dired` but with the improvement of not adding in spaces to the new name and being able to define any unique naming convention I like.

So in `dired` to achieve this, simply:

-   open dired
-   cursor over the desired item to rename
-   `w` (dired-copy-filename-as-kill)
-   `C` (dired-do-copy)
-   paste from the kill ring (yank)
-   modify the filename as desired
-   return

pretty simple and uses all standard `dired` functionality and of course can be converted to a macro for a certain reproducible naming conventions and then maybe an attachment to a keybinding.

Note that it might be worth adding the following to your emacs init file so no confirmation will be required when copying directories:

```elisp
(setq dired-recursive-copies 'always)
```
