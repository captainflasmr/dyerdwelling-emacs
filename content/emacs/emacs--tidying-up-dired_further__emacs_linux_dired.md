---
title: "Tidying up Dired Further"
author: ["James Dyer"]
lastmod: 2022-11-03T00:00:00+00:00
tags: ["emacs", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--tidying-up-dired_further__emacs_linux_dired/montage.jpg"
---

Something is still bugging me with my dired tidy up and I think it is wanting to add the ability to remove dot files.

<!--more-->

{{< figure src="/emacs/emacs--tidying-up-dired_further__emacs_linux_dired/montage.jpg" class="emacs-img" >}}

I am currently using the Dolphin file manager on linux and by default I tend not to show the dot files for a cleaner output, generally the only dot file of course I really care about are the emacs ones and I know where they are!

With some hunting around I copy pasted the following and it works perfectly:

```elisp
;; hide dotfiles and firefox.tmp
;; Toggle Hidden Files in Emacs dired with C-x M-o
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|firefox.tmp$"))

;; dired omit
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
```

I just now need to muscle memorise the key combination to toggle enable them again, hence:

```elisp
C-x M-o

(dired-omit-mode &optional ARG)

Toggle omission of uninteresting files in Dired (Dired-Omit mode).
```
