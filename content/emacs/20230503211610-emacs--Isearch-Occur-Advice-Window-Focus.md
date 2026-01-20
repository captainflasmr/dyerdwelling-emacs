---
title: "Initial focus in Occur Buffer"
author: ["James Dyer"]
lastmod: 2023-05-11T20:43:00+01:00
tags: ["occur", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230503211610-emacs--Isearch-Occur-Advice-Window-Focus.jpg"
---

Just a quick one today!

I am finding `occur` extremely useful, from building an index from my emacs init file to searching through org headers to generally just having my `isearch` all there in a single window.

<!--more-->

{{< figure src="/emacs/20230503211610-emacs--Isearch-Occur-Advice-Window-Focus.jpg" class="emacs-img" >}}

However I would rather the cursor would jump to the `*Occur*` buffer when invoked as it just feels a little more natural, so I added the following:

```elisp
(advice-add 'isearch-occur :after
            '(lambda (origin &rest args)
               (isearch-exit)
               (select-window (get-buffer-window "*Occur*"))
               (goto-char (point-min))
               ))
```
