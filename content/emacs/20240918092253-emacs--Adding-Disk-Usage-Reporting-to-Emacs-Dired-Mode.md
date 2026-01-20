---
title: "Adding Disk Usage Reporting to Emacs Dired"
author: ["James Dyer"]
lastmod: 2024-09-18T09:30:00+01:00
tags: ["emacs", "elisp", "dired", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240918092253-emacs--Adding-Disk-Usage-Reporting-to-Emacs-Dired-Mode.jpg"
---

Just a quick one today.  One of the great strengths of Emacs is its extensibility, I have mentioned before in adding the ability to disk report on the size of a directory in `dired` but I thought I would quickly revisit the topic, demonstrating the function I currently use.

<!--more-->

This functionality will allow you to run disk usage reports directly within Dired, using the `du -hc` command. This will make it easier to figure out how much space directories are taking up.

The aim of this function is to run the Linux command `du -hc` on the directory currently under the cursor in Dired mode. The command `du -hc` provides a human-readable summary of disk usage for the specified directory.

Here's the function that accomplishes this task:

```elisp
(defun my/dired-du ()
  "Run 'du -hc' on the directory under the cursor in Dired."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))
    (if (file-directory-p current-dir)
        (dired-do-async-shell-command "du -hc" nil (list current-dir))
      (message "The current point is not a directory."))))
```

{{< figure src="/emacs/20240918092253-emacs--Adding-Disk-Usage-Reporting-to-Emacs-Dired-Mode.jpg" width="100%" >}}

and that is it!
