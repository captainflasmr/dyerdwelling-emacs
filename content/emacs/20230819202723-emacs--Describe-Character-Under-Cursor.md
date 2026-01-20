---
title: "Describe Character / Face Under Cursor"
author: ["James Dyer"]
lastmod: 2023-12-02T09:13:00+00:00
tags: ["emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230819202723-emacs--Describe-Character-Under-Cursor.jpg"
---

Every now and then I find myself tweaking the look of emacs and have started to build up a list of my own common faces in `custom-set-faces` to suit my needs, for example here is my current setup:

<!--more-->

{{< figure src="/emacs/20230819202723-emacs--Describe-Character-Under-Cursor.jpg" class="emacs-img" >}}

```elisp
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#ffffff" :inverse-video t))))
 '(ediff-current-diff-A ((t (:extend t :background "#b5daeb" :foreground "#000000"))))
 '(ediff-even-diff-A ((t (:background "#bafbba" :foreground "#000000" :extend t))))
 '(ediff-fine-diff-A ((t (:background "#f4bd92" :foreground "#000000" :extend t))))
 '(ediff-odd-diff-A ((t (:background "#b8fbb8" :foreground "#000000" :extend t))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-date ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:foreground "#8f4800"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "#5555ff" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight regular :height 0.7))))
 '(widget-button ((t (:inherit fixed-pitch :weight regular))))
 '(window-divider ((t (:foreground "black"))))
 '(vertical-border ((t (:foreground "#000000")))))
```

To help me work out the name of a certain face I use:

`(describe-char)`

typically accessed through `C-x =` (what-cursor-position) with the prefix argument, then accessing customize menu, saving changes, looking up the face changed in the init file and then copy and paste to my custom-set-faces above.
