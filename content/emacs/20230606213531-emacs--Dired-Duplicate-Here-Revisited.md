---
title: "Dired Duplicate Here Revisited"
author: ["James Dyer"]
lastmod: 2023-07-04T21:13:00+01:00
tags: ["elisp", "emacs", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230606213531-emacs--Dired-Duplicate-Here-Revisited.jpg"
---

It is not uncommon for me to want the ability to quickly duplicate a file, especially if I am hacking around and just want a quick snapshot of a working file.

<!--more-->

Previously I was using `dired` in a slightly convoluted manner, namely:

-   open dired
-   cursor over the desired item to rename
-   `w` (dired-copy-filename-as-kill)
-   `C` (dired-do-copy)
-   paste from the kill ring (yank)
-   modify the filename as desired
-   return

But I never actually used this functionality so I think its time for a different approach.

Since I have recently been delving into **elisp** I thought I would write an interactive function:

```elisp
(defun my/dired-duplicate-file (arg)
  "Duplicate the current file in Dired."
  (interactive "p")
  (let ((filename (dired-get-filename)))
    (setq target (concat (file-name-sans-extension filename)
                         "-old"
                         (if (> arg 1) (number-to-string arg))
                         (file-name-extension filename t)))
    (if (file-directory-p filename)
        (copy-directory filename target)
      (copy-file filename target))
    )
  )

(define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)
```

The function first gets the name of the current file in the Dired buffer using \`dired-get-filename\`. It then creates a new filename for the duplicated file by appending "-old" to the base filename (i.e., without the extension), followed by the number specified by the \`arg\` prefix argument, and the original file extension.

For example, if the \`arg\` argument is 3, and the original filename is "example.txt", the duplicated filename would become "example-old3.txt".

The function then checks if the file is a directory using \`file-directory-p\`. If the file is a directory, it copies the entire directory to the new target directory. If the file is a regular file, it copies the file to the new target file.

The function is interactive, meaning it can be invoked with a key press or command invocation. The function does not return a value.

---

Could you tell I used ChatGPT to describe this function?, well it was accurate, very precise, but very clinical, possibly useful for the quick documenting of written functions though.

I prefer the following more human approach:

> By default a quick flick of the fingers would duplicate rename a file or directory via emacs `dired` adding the keyword `old` and potentially a numeric value by means of the universal argument.

Most of the time I just want a quick single backup but for more duplicates there is the option of a quick C-u (adds a 4) and a C-u C-u (a 16) e.t.c

{{< figure src="/emacs/20230606213531-emacs--Dired-Duplicate-Here-Revisited.jpg" width="300px" >}}

That should do me for now, lets see how I get on!
