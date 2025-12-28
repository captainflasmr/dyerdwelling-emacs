---
title: "Kurecolor-css-mode-fix"
author: ["James Dyer"]
lastmod: 2024-03-29T09:07:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240329090738-emacs--Kurecolor-css-mode-fix.jpg"
---

<!--more-->

```elisp

(defun kurecolor-replace-current (fn &rest args)
  "Get the current unspaced string at point.
Replace with the return value of the function FN with ARGS"
  (let (pos1 pos2 len replacement excerpt change)
    (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (when (looking-at "#") (forward-char 1))
        (setq pos1 (car (bounds-of-thing-at-point 'symbol))
          pos2 (cdr (bounds-of-thing-at-point 'symbol)))
        (when (and (> pos1 0) (not (equal major-mode 'css-mode)))
          (setq pos1 (- pos1 1)))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (if args
      (progn (setq change (car args))
        (setq replacement (funcall fn excerpt change)))
      ;; no args
      (setq replacement (funcall fn excerpt)))
    (delete-region pos1 pos2)
    (insert replacement)))
```
