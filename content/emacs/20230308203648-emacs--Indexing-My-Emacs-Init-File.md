---
title: "Indexing My Emacs Init File"
author: ["James Dyer"]
lastmod: 2023-04-12T14:36:00+01:00
tags: ["emacs", "elisp", "ada", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230308203648-emacs--Indexing-My-Emacs-Init-File.jpg"
---

Since I keep all my emacs configuration in a single `.emacs` file and in a hyper organised manner it means I have my init file split into different sections, for example :

<!--more-->

-   platform
-   packages
-   mail
-   calendar
-   completion
-   save-desktop
-   keys
-   modes
-   setqs

---

with each section delimited by a comment of the form:

```elisp
;;
;; -> platform`
;;
```

Now I am using `occur` more often, then why not write a function to produce a nice little Occur buffer containing an index of my init file sections, as thus:

```elisp
(defun my/index ()
  (interactive)
  (beginning-of-buffer)
  (occur ";;[[:space:]]->"))
```

I avoided the self index reference by explicitly using the **[:space:]** variant for the occur regex.

and this gives me the following:

{{< figure src="/emacs/20230308203648-emacs--Indexing-My-Emacs-Init-File.jpg" width="100%" >}}

The selection of each item takes me to the relevant section of my emacs configuration and as I have **f8** bound to `next-error` I can easily step through each section if I want to.
