---
title: "Emacs asynchronous copying using dired-async-mode"
author: ["James Dyer"]
lastmod: 2024-01-20T09:38:00+00:00
tags: ["modeline", "emacs-29", "emacs", "elisp", "dired", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240120084016-emacs--Dired-Async-Mode.jpg"
---

For a while now I've been using an `rsync` based `dired-copy` replacement for large copy asynchronous operations within emacs.  It is not uncommon for me to want to copy large files in emacs and rather than waiting for the operation to finish I leveraged `async-shell-command` to perform an rsync copy as thus:

<!--more-->

{{< figure src="/emacs/20240120084016-emacs--Dired-Async-Mode.jpg" width="100%" >}}

```elisp
(define-key dired-mode-map (kbd "C") 'my/rsync)

(defun my/rsync (dest)
  "Rsync copy."
  (interactive
   (list
    (expand-file-name (read-file-name "rsync to:"
                                      (dired-dwim-target-directory)))))
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        (command "rsync -arvz --progress --no-g "))
    (dolist (file files)
      (setq command (concat command (shell-quote-argument file) " ")))
    (setq command (concat command (shell-quote-argument dest)))
    (async-shell-command command "*rsync*")
    (dired-unmark-all-marks)
    (other-window 1)
    (sleep-for 1)
    (dired-revert)
    (revert-buffer nil t nil)))
```

Generally this has worked pretty well for me and as a plus I could see the progress in a buffer named `*rsync*` (although I would typically dismiss it quickly using `winner-undo` or maybe adding something to `display-buffer-alist`)

After watching the always excellent and informative video from [Emacs Elements](https://www.youtube.com/@emacselements) - [Best Way to Sort and Play Videos Is with Emacs](https://www.youtube.com/watch?v=1jCNrpp_STM&t=80s) it opened my eyes to a built-in feature of emacs 29, namely `dired-async-mode` (Do dired actions asynchronously) which seems to pretty much do what it says.

To activate, rather than having to map all those dired commands to async versions, the following just needs to be set up :

```elisp
(dired-async-mode 1)
```

Now when a copy via dired is activated it will happen asynchronously! - and also with other dired commands like rename but really it is generally a copy that will take the time.

Out of the box a normal copy in dired using `C` activates the asynchronous task and the modeline displays something like the following in all windows :

```nil
[1 Async job(s) running]
```

When finished the modeline updates with a copy complete message and then after three seconds disappears.

Three seconds you say? well how do I know that?, well I played around a little with the modeline finish message as of course this can be configured, this was my initial attempt:

```elisp
(defun my/dired-async-mode-line-message (text face &rest args)
  "Notify end of async operation in `mode-line'."
  (let* ((my/message "Dired Async has finished!!")
         (mode-line-format (concat
                            " " (propertize
                                 my/message
                                 'face
                                 '(:background "#ff0000" :foreground "#ffffff" :inherit bold)
                                 ))))
    (message my/message)
    (force-mode-line-update)
    (sit-for 8)
    (force-mode-line-update)))

(setq dired-async-message-function 'my/dired-async-mode-line-message)
```

I just felt I could try something a little more prominent to indicate the end of a copy but in the end I kept the default setup.

The only issue I have observed thus far is that on an initial emacs startup the modeline for all windows displays **[0 Async job(s) running]** and only seems to disappear after the first copy.  It is a little annoying and even with the use of the `diminish` package my modeline can look a little cluttered.  I might need to look at this.

---

Update **<span class="timestamp-wrapper"><span class="timestamp">&lt;2024-01-20 Sat 12:05&gt;</span></span>**
Actually I do know why the above is happening, when I was messing around with the `my/dired-async-mode-line-message` function above I turned on `(dired-async--modeline-mode 1)` ooops! well now I have commented it out no modeline async message initially appears! - I thought I would just leave all this stuff in here 😀
