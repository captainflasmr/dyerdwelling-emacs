---
title: "Moving Away From Deft"
author: ["James Dyer"]
lastmod: 2023-02-22T20:33:00+00:00
tags: ["emacs", "dired", "deft", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2023-02-04_14-32_t.jpg"
---

As the title says, it is with a heavy heart that I have decided to move away from **deft**.  There are a few reasons for this:

<!--more-->

{{< figure src="/emacs/20230204140603-emacs--Moving-Away-From-Deft/2023-02-04_14-32.jpg" class="emacs-img" >}}

-   All files I am interested in quickly locating and editing are now orgified (turned into org files)
-   All these files are located in a single directory
-   All these files have a sensible naming convention indicating the contents
-   I am now much more familiar with dired
-   I didn't ever use the deft facility for creating new files, I prefer to
    use dired
-   I didn't ever need to incrementally filter / search as I can use emacs / dired for this
-   Deft took a couple of seconds to initially load
-   My deft configuration was becoming a little bloated and I wanted to use
    vanilla emacs where I could; see below for my former deft configuration:

<!--listend-->

```elisp
(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :config (setq deft-text-mode 'org-mode
                deft-use-filename-as-title nil
                deft-auto-save-interval 0
                deft-use-filter-string-for-filename t
                deft-extensions '("org")
                deft-default-extension "org"
                deft-time-format "%Y-%m-%d %H:%M:%S"
                deft-new-file-format "%Y-%m-%dT%H%M%S"
                deft-strip-summary-regexp "\\([\n]\\|^#\\+.+:.*$\\)"
                deft-recursive t))
```

It is time to move onto something a little more simple, namely:

```elisp
(setq deft-directory (concat home-dir "/DCIM/content"))
(bind-key* (kbd "C-c d") (lambda()(interactive)(dired deft-directory)))
```

and yes I decided to keep the venerable name of deft in memoriam.

I think this is an example of over time learning that emacs has a lot of what you want already built in and with org mode it gently nudges you to organise files in a more coherent manner leading to a more simple agnostic digital way of life.
