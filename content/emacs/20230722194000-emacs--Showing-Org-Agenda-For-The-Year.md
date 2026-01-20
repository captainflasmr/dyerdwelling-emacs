---
title: "Showing Org Agenda For The Year"
author: ["James Dyer"]
lastmod: 2023-08-04T06:21:00+01:00
tags: ["emacs", "elisp", "agenda", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230722194000-emacs--Showing-Org-Agenda-For-The-Year.jpg"
---

I am just starting to include more of my org files into `org-agenda`.  By default `C-c a a` gives a show for the next 7 days but I think for now I would like something of a more calendarish overview with a long form look of scheduled and completed tasks.

<!--more-->

{{< figure src="/emacs/20230722194000-emacs--Showing-Org-Agenda-For-The-Year.jpg" class="emacs-img" >}}

I thought I would write an `elisp` function:

```elisp
(defun display-year-agenda (&optional year)
  "Display an agenda entry for a whole year."
  (interactive (list (read-string "Enter the year: "
                                  (format-time-string "%Y" (current-time)))))
  (setq year (string-to-number year))
  (org-agenda-list)
  (org-agenda-year-view year)
  (setq this-year (string-to-number (format-time-string "%Y" (current-time))))
  (when (= year this-year)
    (org-agenda-goto-today)
    (recenter-top-bottom 10)))

;; Bind a key for easy access
(global-set-key (kbd "C-c y") 'display-year-agenda)
```

The default prompt is the current year but with the ability to enter any year desired.  I also thought that if calling up this current year then I would want to highlight today and recenter a little.

As with all things I will give this a go for a while and see if it fits nicely into my workflow.
