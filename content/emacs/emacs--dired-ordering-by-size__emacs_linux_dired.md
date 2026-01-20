---
title: "Dired Ordering by Size"
author: ["James Dyer"]
lastmod: 2022-11-09T00:00:00+00:00
tags: ["emacs", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--dired-ordering-by-size__emacs_linux_dired/2022-11-23_16-15.jpg"
---

By default `dired` orders its files in alphanumeric order and when **s** is selected it sorts by date according to :

<!--more-->

```elisp
(dired-sort-toggle-or-edit &optional ARG)
```

But recently I wanted to list files according to their size, which of course is a very common thing to do especially when you are undertaking a nice spring clean up.

The solution is to modify the `dired-listing-switches` to add in an **S** argument which can be found by passing in the universal argument to **s**, hence : `C-u s`

This presents the user with something like: `-al` at which point you can just tack `S` on the end to show `-alS`

as the ls man page states the following:

```nil
  -S     sort by file size, largest first
```

and this is what we get:

{{< figure src="/emacs/emacs--dired-ordering-by-size__emacs_linux_dired/2022-11-23_16-15.jpg" width="300px" >}}

a nice ordered listing, it is not something I do often and generally the standard dired sort toggle is all I need, but it is nice to know that this is available.
