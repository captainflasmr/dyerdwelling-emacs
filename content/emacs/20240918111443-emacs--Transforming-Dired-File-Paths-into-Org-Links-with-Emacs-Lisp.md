---
title: "Transforming Dired File Paths into Org Links"
author: ["James Dyer"]
lastmod: 2024-10-17T15:36:00+01:00
tags: ["org", "hugo", "emacs", "elisp", "dired", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240918111443-emacs--Transforming-Dired-File-Paths-into-Org-Links-with-Emacs-Lisp.jpg"
---

I am of course using org mode for organizing my tasks, notes and other paraphernalia.

<!--more-->

{{< figure src="/emacs/20240918111443-emacs--Transforming-Dired-File-Paths-into-Org-Links-with-Emacs-Lisp.jpg" width="100%" >}}

In addition, I generate static web pages from org using **`hugo`**, with each org heading representing a single post.

I often find myself needing to refer to files stored in a directory, especially image files that can be org embedded and then passed through to Hugo by the `ox-hugo` package.

Navigating to the file and manually generating the appropriate org link can be just a little cumbersome - to initially pick up the file name and then insert the relevant org attributes.

`dired-copy-filename-as-kill` is a function I often use in `dired`, which copies the filename under point. I can then use something like `tempel` to insert the attributes. However, while the combination of copying the filename in `dired` and then remembering the `tempel` shortcode is quite efficient, I think this could be accomplished more cleanly with some Elisp and a `dired-mode` mapping.

Here is a typical embedded image form, which restricts the image within the org document to 300 pixels and to 100% width when exported through hugo to my web site.

```nil
#+attr_org: :width 300px
#+attr_html: :width 100%
[[file:static/emacs/20240918111443-emacs--Transforming-Dired-File-Paths-into-Org-Links-with-Emacs-Lisp.jpg]]
```

Lets walk through a custom function, `my/dired-file-to-org-link`, which transforms the `dired` file path under cursor/point into an org link and copies it to the kill ring for easy pasting.

```elisp
(defun my/dired-file-to-org-link ()
  "Transform the file path under the cursor in Dired to an Org mode
link and copy to kill ring."
  (interactive)
  (let ((file-path (dired-get-file-for-visit)))
    (if file-path
        (let* ((relative-path (file-relative-name file-path
                                                  (project-root (project-current t))))
               (org-link (concat "#+attr_org: :width 300px\n"
                                 "#+attr_html: :width 100%\n"
                                 "file:" relative-path "\n")))
          (kill-new org-link)
          (message "Copied to kill ring: %s" org-link))
      (message "No file under the cursor"))))
```

When `my/dired-file-to-org-link` is called in a dired buffer, it gets the file path under the point, transforms it into an Org link with specific attributes, and copies the link to the kill ring. This enables you to quickly paste it into your Org file.

I map this to the letter `b` in the `dired-mode` mapping, which for me mnemonically refers to (b)log and the b key is not one I currently use in dired.

Note also the use of project.el with the project functions, as I have a project top level defined for my website content, allowing for an easily generated relative path name.

Therefore for example,  `20240918111443-emacs--Transforming-Dired-File-Paths-into-Org-Links-with-Emacs-Lisp.jpg` from dired in an `emacs` directory is transformed into :

```nil
#+attr_org: :width 300px
#+attr_html: :width 100%
[[file:static/emacs/20240918111443-emacs--Transforming-Dired-File-Paths-into-Org-Links-with-Emacs-Lisp.jpg]]
```
