---
title: "More Improvements to Dired Duplicate Here"
author: ["James Dyer"]
lastmod: 2023-10-13T16:10:00+01:00
tags: ["emacs", "elisp", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231013153639-emacs--More-Flexible-Duplicate-Thing-Function.jpg"
---

On a previous post I created an elisp function to quickly duplicate a file or directory in `dired`, by default it would copy the `dired` item under the cursor to an `old` suffix or append a number based on the universal argument.

<!--more-->

[Dired Duplicate Here Revisited]({{< ref
"/emacs/20230606213531-emacs--Dired-Duplicate-Here-Revisited.md" >}})

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
```

This worked well for a while but now I want something a little more robust and after working within Krita for a while I decided that I would like to implement its incremental save naming convention.

It's pretty simple really, just an incremented integer, zero padded to a width of 3, inserted just before the extension:

{{< figure src="/emacs/20231013153639-emacs--More-Flexible-Duplicate-Thing-Function.jpg" width="100%" >}}

So I just need to apply a little counter logic but still have the potential to pass in the universal argument and of course avoid overriding any existing backup as it did before.

```elisp
(defun my/dired-duplicate-file (arg)
  "Duplicate a file from dired with an incremented number.
  If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))
```

I can now create any number of dired file/directory backups quickly from `dired` (well up to 100).
