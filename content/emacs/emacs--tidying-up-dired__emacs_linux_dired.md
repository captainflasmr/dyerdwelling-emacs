---
title: "Tidying up Dired"
author: ["James Dyer"]
lastmod: 2022-10-23T00:00:00+01:00
tags: ["emacs", "elisp", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--tidying-up-dired__emacs_linux_dired/2022-11-23_15-23.jpg"
---

Bit by bit I am getting to grips with `dired` and using this for more operations on my files.

<!--more-->

The next step is to reduce the listing width so that a listing fits better in a smaller window.

Here is the format of my current listing:

{{< figure src="/emacs/emacs--tidying-up-dired__emacs_linux_dired/2022-11-23_15-32.jpg" width="300px" >}}

By default `dired` uses `-al`, which gives a standard long listing.  Unfortunately for my work within emacs this is actually now too long, and also not just in emacs but in my terminal too.

After much hunting around the man page including potentially formatting the output and selecting the listing fields manually, I came to the conclusion that it is mainly the _group_ and _user_ I want to remove for the moment, and fortunately there are a couple of arguments that can do this for me:

```nil
  -g     like -l, but do not list owner
  -G, --no-group
         in a long listing, don't print group names
```

and while scrolling through the man page I also thought that making the sizes human readable would also be useful, so I have come up with the following in my `.emacs`

```elisp
(setq dired-listing-switches "-lGgha")
```

to give me the following dired output:

{{< figure src="/emacs/emacs--tidying-up-dired__emacs_linux_dired/2022-11-23_15-24.jpg" width="300px" >}}

This is much better, but can I now go further?

Well this is emacs, and you betcha!!!!

I know of the command `(` which when in dired mode hides the details and shows just the files.  Wouldn't it be great to have this enabled by default?

A quick `describe-key` gives me the following function:

```elisp
(dired-hide-details-mode &optional ARG)
```

But it is not clear how I can set this by default, there is nothing obvious in `customize-group` for dired, so now to the interwebs...

It looks as though I will need to add to the `dired-mode-hook` and set the details to be hidden when dired mode is activated as follows:

```elisp
;; dired hide long listing by default
(defun my-dired-mode-setup ()
  "show less information in dired buffers"
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my-dired-mode-setup)
```

Which now gives me:

{{< figure src="/emacs/emacs--tidying-up-dired__emacs_linux_dired/2022-11-23_15-23.jpg" width="300px" >}}

and now if I want show the details I just select `(` so effectively generally showing details rather than hiding them.

This makes more sense for my workflow in emacs, especially now my buffers and windows are getting a little more numerous and I am still really focussing on trying to use a single emacs frame on a laptop.
