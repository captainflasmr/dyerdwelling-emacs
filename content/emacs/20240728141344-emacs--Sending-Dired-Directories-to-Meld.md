---
title: "Sending Dired Directories to Meld for Directory Comparison"
author: ["James Dyer"]
lastmod: 2024-07-28T16:43:00+01:00
tags: ["meld", "emacs", "elisp", "dired", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240728141344-emacs--Sending-Dired-Directories-to-Meld.jpg"
---

A common activity for a Software Engineer is to compare two directories, especially those that might require a level of merging.  My preferred tool of choice in this instance is **Meld**

<!--more-->

{{< figure src="/emacs/20240728141344-emacs--Sending-Dired-Directories-to-Meld.jpg" width="100%" >}}

> Meld is a visual diff and merge tool that is widely used for comparing files, directories, and version-controlled projects.

For **Meld**, the workflow is generally straightforward: simply open a file explorer alongside and drag and drop the required directories.

But what about comparing directories seamlessly as part of the Emacs ecosystem I hear you say? 🫢

Well, I am trying to incorporate `ztree-diff` into my workflow, but after a few minutes of grappling with it, I always reach for Meld as I know it well (yes, I know this isn't really the Emacs way, in which an initial investment of blood, sweat and toil eventually reaps dividends).  I will persist with `ztree-diff`, but for now, to at least get a more seamless directory comparison experience from within Emacs I will have to just fall back on the tried and tested method of invoking an external application, and in this case by sending directories marked in `dired` to **Meld**, here is some elisp:

```elisp
(require 'cl-lib)

(defun my/dired-meld-diff-all-dwim ()
  "Compare all marked directories in all visible Dired buffers using Meld.
   The order of directories respects the order suggested by `dired-dwim-target`."
  (interactive)
  (let ((files ()))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'dired-mode)
                   (dired-get-marked-files))
          (setq files (append files (dired-get-marked-files))))))
    (if (or (<= (length files) 1)
            (not (cl-every 'file-directory-p files)))
        (message "Please mark at least two directories.")
      (apply 'start-process "meld" nil "meld" files))))

(define-key dired-mode-map (kbd "C-c m") 'my/dired-meld-diff-all-dwim)
```

_\*edit: <span class="timestamp-wrapper"><span class="timestamp">&lt;2024-07-29 Mon&gt; </span></span> every is now obsolete, replaced with cl-every_

This method has the advantage of honouring `dired-dwim-target` through the `(window-list)` function which I am now pretty familiar with as I often use `dired-do-copy` and `dired-do-rename`

So how does this work?, well now I can split windows, bring up some dired buffers, mark directories to compare, and then make sure I am in the dired buffer that is to appear on the LHS of the Meld comparison before I then run the command.  The destination directory would then be defined according to `dired-dwim-target` which I have set to `t` which is the next dired visible buffer in a clockwise direction, but can be adjusted as desired.

With a little experimentation I found out that `(dired-get-marked-files)` retrieves the dired item that the cursor is over, therefore each directory doesn't necessary even need to be explicitly marked!, so you could just split with two dired buffers, leave the cursor over the destination directory, switch to the source dired buffer and leave the cursor over the source directory before running the command above.

More experimentation reveals that even three directories can be sent to Meld for comparison!, just open the dired buffers, hover cursor or mark, make sure they are visible, set up in the clockwise order and then focus the cursor on the source directory.

Is it a little weird that I'm talking about experimentation when this is a function I wrote myself? 😀  A three directory comparison is not something I have ever used but I deliberately left the function open and flexible enough and it doesn't really do any harm...
