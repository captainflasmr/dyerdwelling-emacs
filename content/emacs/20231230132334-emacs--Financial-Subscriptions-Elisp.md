---
title: "Financial-Subscriptions-Elisp"
author: ["James Dyer"]
lastmod: 2023-12-30T13:23:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20231230132334-emacs--Financial-Subscriptions-Elisp.jpg"
---

Description of how I keep track of my subscriptions while using elisp to sum them up, look at **misc-subs.org**

<!--more-->

```elisp
(let ((sum 0))
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward "\\([0-9]+\\.[0-9]+\\)" nil t)
                (not (save-excursion (beginning-of-line) (looking-at-p ": Sum:" ))))
      (setq sum (+ sum (string-to-number (match-string 1))))))
  (message "Sum: %.2f" sum))
```
