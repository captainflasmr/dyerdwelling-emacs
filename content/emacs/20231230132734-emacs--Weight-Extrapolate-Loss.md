---
title: "Weight-Extrapolate-Loss"
author: ["James Dyer"]
lastmod: 2023-12-30T13:27:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20231230132734-emacs--Weight-Extrapolate-Loss.jpg"
---

<!--more-->

```elisp
(defun subtract-weight (weight-str avg-loss)
  "Subtract AVG-LOSS pounds from WEIGHT-STR given in 'stones:pounds' format."
  (let* ((stones-pounds (split-string weight-str ":"))
         (stones (string-to-number (car stones-pounds)))
         (pounds (string-to-number (cadr stones-pounds)))
         (total-pounds (+ pounds (* stones 14)))      ;; Convert stones to pounds
         (new-total-pounds (- total-pounds avg-loss)) ;; Subtract weight loss
         (new-stones (truncate (/ new-total-pounds 14))) ;; Calculate new stones
         (new-pounds (mod new-total-pounds 14)))      ;; Calculate remaining pounds
    (format "%d:%d" new-stones new-pounds)))         ;; Format new weight

(defun extrapolate-weight-loss (num-weeks)
  "Extrapolate weight loss for NUM-WEEKS using the last 'av/pd' value in the org-table."
  (interactive "p")
  (save-excursion
    (let ((last-avg-loss 2.9)
          (last-date "")
          (week 0)
          (next-date ""))
      (print num-weeks)
      (when (org-table-p)
        (goto-char (org-table-end))
        ;; Find the last date and week number
        (search-backward-regexp "|\\s-?\\([0-9]+\\)\\s-?|\\s-?<\\([0-9-]+\\)" nil t)
        (setq week (string-to-number (match-string 1)))
        (setq last-date (match-string 2))
        (setq last-weight "16:10")

        (goto-char (org-table-end))

        ;; Loop for num-weeks to generate new lines
        (dotimes (i num-weeks)
          (setq next-date
                (format-time-string "<%Y-%m-%d %a>"
                                    (time-add (org-time-string-to-time last-date)
                                              (days-to-time (+ (* i 7) 7))))) ;; add 7 days per week
          (setq week (+ week 1))
          (insert (format "| %d | %s | %s | | | | | | |\n"
                          week next-date (subtract-weight last-weight (* (+ i 1) last-avg-loss)))))
        )
      )
    )
  (org-table-align))
```
