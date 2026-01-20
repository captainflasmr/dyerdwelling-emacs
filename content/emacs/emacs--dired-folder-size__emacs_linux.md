---
title: "Dired folder size"
author: ["James Dyer"]
lastmod: 2022-10-08T00:00:00+01:00
tags: ["emacs", "elisp", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--dired-folder-size__emacs_linux.jpg"
---

My `dired` replacement of Dolphin / linux terminal continues.  The next thing is something useful to me but uncommon and something that dired didn't seem to be able to do out of the box.

<!--more-->

{{< figure src="/emacs/emacs--dired-folder-size__emacs_linux.jpg" class="emacs-img" >}}

That is to be able to recursively display the size of a folder / files.

Every now and again I like to keep my files and data under control especially as I am using syncthing to backup my core data to my phone.

It is very easy to let a large download or maybe an emacs exported large file sneak through.  My tools of choice have generally been the following and in time order:

-   Terminal using `du -h --max-depth 1 <folder>`
-   Gnome Disk Usage Analyser (boabab)
-   Filelight - now I have moved to KDE

So how do I achieve this?

I could perhaps try something from emacs packages but as you already know my first port of call is to try and preserve vanilla emacs where possible which might mean in this case finding / or writing some elisp.

The `www.emacswiki.org` comes to the rescue once again! with yet another elisp snippet as thus:

```elisp
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)
```

I don't need anything too fancy here, for example a full dired integration like Solid Explorer on android but just showing the total size in the minibuffer is fine and why not leverage the existing operating system commands rather than completely depending on elisp!
