---
title: "Sorting Org Tags using Org Mode!"
author: ["James Dyer"]
lastmod: 2023-04-07T16:04:00+01:00
tags: ["org", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230406210733-emacs--More-Sorting-Tags-Options.jpg"
---

Well as always a little more time with emacs a little feedback and then finding more about org I have now figured out (I think) how I can better sort tags in an org file.  In my previous post:

<!--more-->

{{< figure src="/emacs/20230406210733-emacs--More-Sorting-Tags-Options.jpg" class="emacs-img" >}}

[Sorting Org Tags]({{< ref
"/emacs/20230310120116-emacs--Sorting-Org-Tags.md" >}})

I made the following comment:

> I had assumed that org-mode came with the built-in ability to sort tags but I couldn't find any evidence of this

Well as it turns out there is evidence of this! and it takes the form of:

```elisp
(setq org-tags-sort-function 'org-string-collate-greaterp)
```

This is tied into `C-c C-q (org-set-tags-command)` of which I hadn't yet discovered but now I think I shall bring into my muscle memory. I had previously just been manually adding tags!

Further interwebs hunting and I came across the following function:

```elisp
(defun my/org-sort-tags ()
  "On a heading sort the tags."
  (interactive)
  (when (org-at-heading-p)
    (org-set-tags (sort (org-get-tags) #'string<))))
```

which more concisely accomplishes my original intention and will work when the cursor is anywhere on an org heading and therefore a macro or presumably an org-\* type iterator would neatly take care of the whole file for me.

Although I went down a little rabbit hole I found a few things down there, elisp improvement, digging more around org-mode and bewilderingly wondering where my journey will end.  Of course with emacs it never will, my little head will just peep out at the daylight sun every now and then but my little noggin will be wiser and more proficient 😃
