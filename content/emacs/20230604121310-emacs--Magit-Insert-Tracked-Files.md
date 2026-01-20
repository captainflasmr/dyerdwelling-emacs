---
title: "Magit Status To Show Tracked Files"
author: ["James Dyer"]
lastmod: 2023-06-08T11:05:00+01:00
tags: ["magit", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230604121310-emacs--Magit-Insert-Tracked-Files.jpg"
---

While currently piecing together my git repositories and figuring out which files to commit I am invariably going to have some **Untracked files** but I also would like to see the **Tracked files** in `magit-status`

<!--more-->

Well emacs being emacs this can be easily achieved, I added the following to my `use-package magit` declaration.

```elisp
:config
(magit-add-section-hook
 'magit-status-sections-hook 'magit-insert-tracked-files nil 'append)
```

{{< figure src="/emacs/20230604121310-emacs--Magit-Insert-Tracked-Files.jpg" class="emacs-img" >}}
