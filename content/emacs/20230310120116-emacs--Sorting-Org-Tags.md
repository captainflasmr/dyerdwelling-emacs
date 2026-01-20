---
title: "Sorting Org Tags"
author: ["James Dyer"]
lastmod: 2023-04-01T12:55:00+01:00
tags: ["org", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230310120116-emacs--Sorting-Org-Tags.jpg"
---

I use a package called **org-rainbow-tags** which adds random colours to org tags to provide a consistent colour between identical tags.  This helps to identify common tags throughout the file but has the side effect of emphasising the lack of my coherent tag ordering.

<!--more-->

{{< figure src="/emacs/20230310120116-emacs--Sorting-Org-Tags.jpg" class="emacs-img" >}}

I would like to order the tags consistently, just for my own peace of mind! 😀

I had assumed tkhat org-mode came with the built-in ability to sort tags but I couldn't find any evidence of this so I decided to create a method using my own function.  My preferred default method is in descending order as I commonly use a year tag which I would always like to be on the right hand side.

Just select the region containing the tags and run my function passing in the universal argument if you fancy ordering the other way!

```elisp
(defun my/sort-org-tags-region (beg end &optional reversed)
  "In active region sort tags alphabetically in descending order.
  With prefix argument REVERSE order."
  (interactive "r\nP")
  (unless (region-active-p) (user-error "No active region to sort!"))
  (let* ((str (s-trim (buffer-substring-no-properties beg end)))
         (wrd (split-string str ":" t " "))
         (new
          (concat
           ":"
           (s-join ":" (sort wrd (if reversed #'string< #'string>)))
           ":")
          )
         )
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert new)
      )
    )
  )
```

j
