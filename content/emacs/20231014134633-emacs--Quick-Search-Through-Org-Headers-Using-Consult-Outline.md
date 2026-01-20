---
title: "Quick Search Through Org Headers using Consult Outline"
author: ["James Dyer"]
lastmod: 2023-11-18T15:30:00+00:00
tags: ["emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231014134633-emacs--Quick-Search-Through-Org-Headers-Using-Consult-Outline.jpg"
---

I've been on the lookout for an efficient way to swiftly scan through org headings exclusively for a specified input string, akin to the functionality of `isearch`. This would enable me to promptly navigate to a past blog post which is typically stored under an org heading.

<!--more-->

By default, a standard `isearch` examines the entire file, a behaviour that in this case I don't really want. I specifically need to limit the search to org headings only, allowing me to swiftly navigate to an org heading.

The best method I have found is to use `consult-outline` which I have bound to `C-o`

{{< figure src="/emacs/20231014134633-emacs--Quick-Search-Through-Org-Headers-Using-Consult-Outline.jpg" class="emacs-img" >}}

I had a play around with various org filtering options like `org-goto` and `org-agenda` but they didn't really quite do what I wanted.

As it turns out leveraging the built-in `outline-mode` through `consult-outline` also comes with the additional benefit of being applicable to source code files.

Also in combination with `embark-collect` it means I can create a separate outline buffer which can simply be `isearch`'d through to refine my search further.

I seem to be using `embark-collect` more often these days so I bound a key to the mini-buffer map so I won't necessarily need to go through the embark despatching mechanism each time I want to redirect the mini-buffer contents to a proper buffer:

```elisp
(define-key minibuffer-local-map (kbd "C-c e") 'embark-collect)
```

I have also added `consult-imenu` to my arsenal of search weapons which I have bound to `M-o`.  For example I have a very clearly defined commenting format in emacs init and sway configuration files so to jump through a file I can use `imenu` with `consult` instead.
