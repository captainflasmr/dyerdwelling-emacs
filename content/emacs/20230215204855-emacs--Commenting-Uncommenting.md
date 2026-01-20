---
title: "Commenting Un-commenting"
author: ["James Dyer"]
lastmod: 2023-02-15T20:57:00+00:00
tags: ["emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230215204855-emacs--Commenting-Uncommenting.jpg"
---

After watching an interesting video by [EmacsElements](https://www.youtube.com/watch?v=vTdbb7tsvQc) regarding commenting and un-commenting I have to say that I wholeheartedly agree.  I really don't like the way `comment-dwim` works and made me think back to one of the first elisp functions I commandeered from the interwebs :

<!--more-->

{{< figure src="/emacs/20230215204855-emacs--Commenting-Uncommenting.jpg" class="emacs-img" >}}

```elisp
(defun my/comment-or-uncomment ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)(region-end))
    (comment-or-uncomment-region
     (line-beginning-position)(line-end-position))))
```

and it is a command that I constantly use and had forgotten that it isn't part of the emacs default functionality from a `M-;`
