---
title: "Recursively Listing Files in Size Order using find-name-dired"
author: ["James Dyer"]
lastmod: 2023-10-01T21:31:00+01:00
tags: ["find", "emacs", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231001210123-emacs--Dired-Listing-Files-Largest-First.jpg"
---

For a while now I have been looking for a simple method in emacs for trimming down my largest files, usually this means locating those large image files and compressing them as I like keeping my media compressed or deleting any large files I didn't know were lurking around my system.

<!--more-->

{{< figure src="/emacs/20231001210123-emacs--Dired-Listing-Files-Largest-First.jpg" class="emacs-img" >}}

The obvious choice for this is `find-name-dired` but for me always had the annoying habit of reordering the final result after finishing the recursive search.  Even if I set the `find-ls-option` correctly (not necessarily an easy task) to sort by size the final dired buffer output would always keep resetting to sort by file name.

It took me a while delving into the nuts and bolts of `find-name-dired` but I finally realised that after processing using the `find-ls-option` a function `find-dired-refine-function` is called which by default is set to `find-dired-sort-by-filename` which:

> Sorts entries in **Find** buffer by file name lexicographically.

Why this does this I have no idea (perhaps it is because the output of the find command ends up in a dired buffer which would always generally make sense to sort by name?), but as I will only ever want to use `find-name-dired` to list files in size order and then to prune any or compress them using standard `dired` commands then I can just do the following:

```elisp
(setq find-dired-refine-function 'nil)
(setq find-ls-option (cons "-exec ls -lSh {} +" "-lSh"))
```

Which will honour the intended `find-ls-option` to do exactly what it says on the tin and that is to:

```nil
-l : long list as =dired= will always need this form of listing
-S : Sort by file size
-h : Show size in human readable form, which  doesn't seem to affect the sorting
```

For me this seems to be a more transparent method and of course I guess other standard `ls` options can now be passed through to `dired`, for example for files in order of time, you would pass `-t`
