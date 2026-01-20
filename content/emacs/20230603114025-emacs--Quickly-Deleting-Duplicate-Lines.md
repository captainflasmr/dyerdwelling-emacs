---
title: "Quickly Deleting Duplicate Blank Lines"
author: ["James Dyer"]
lastmod: 2023-06-03T21:38:00+01:00
tags: ["emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230603114025-emacs--Quickly-Deleting-Duplicate-Lines.jpg"
---

I am currently hacking around with my org files and in fact macro removing quite a few unnecessary lines.  However this has had the side effect of leaving some significant holes in the form of blank lines.  Sometimes just two duplicate blank lines and sometimes more!

<!--more-->

{{< figure src="/emacs/20230603114025-emacs--Quickly-Deleting-Duplicate-Lines.jpg" class="emacs-img" >}}

I found that I can quickly trim them down by using `delete-duplicate-lines` and making sure the identical lines must be adjacent argument is set by passing in a **C-u C-u** prefix.

So the process is:

-   open the org file
-   `mark-whole-buffer`
-   **C-u C-u** `delete-duplicate-lines`

and that's it!, the key here is the prefix argument otherwise all the blank lines will be deleted which is not what I want.

Of course this method would delete all duplicate adjacent lines and not just the blank ones so I guess you would generally need to be a little careful, but I know I don't have any of these and if I am not too sure then I can just inspect a git diff.
