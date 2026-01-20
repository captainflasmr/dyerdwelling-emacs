---
title: "Efficient File Searching in Emacs: Leveraging completing-read with Customizable Methods"
author: ["James Dyer"]
lastmod: 2024-08-04T09:05:00+01:00
tags: ["ripgrep", "find", "emacs", "elisp", "dired", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240804075952-emacs--Finding-Files-Using-Completing-Read.jpg"
---

I thought I would share a little bit of elisp that I've been using for a while now that allows me to quickly find a file. It leverages `completing-read`, allows the selection of the find method and will search recursively.

<!--more-->

When called you can select which search type you prefer:

{{< figure src="/emacs/20240804075952-emacs--Finding-Files-Using-Completing-Read.jpg" width="100%" >}}

You can select the built-in `find-name-dired` but as you see I can also choose an external find tool such as `find`, `fd` or `rg` and it should be flexible enough to add in any others by expanding `find-options` in the function below:

```elisp
(defun my/find-file ()
  "Find file from current directory in many different ways."
  (interactive)
  (let* ((find-options '(("find -type f -printf \"$PWD/%p\\0\"" . :string)
                         ("fd --absolute-path --type f -0" . :string)
                         ("rg --follow --files --null" . :string)
                         ("find-name-dired" . :command)))
         (selection (completing-read "Select : " find-options))
         (metadata '((category . file)))
         (file-list)
         (file))
    (pcase (alist-get selection find-options nil nil #'string=)
      (:command
       (call-interactively (intern selection)))
      (:string
       (setq file-list (split-string (shell-command-to-string selection) "\0" t))
       (setq file (completing-read (format "Find file in %s: " (abbreviate-file-name default-directory))
                                   (lambda (str pred action)
                                     (if (eq action 'metadata)
                                         `(metadata . ,metadata)
                                       (complete-with-action action file-list str pred)))
                                   nil t nil 'file-name-history)))
      (when file (find-file (expand-file-name file))))))
```

To be completely honest, I don't really use Emacs' built-in find functions, such as `find-file` and `find-name-dired`. Here are a few reasons why:

-   Speed and Efficiency
    -   Tools like `fd` and `rg` are incredibly fast and efficient.
    -   They allow easy filtering through a simple `.ignore` file configuration.

<!--listend-->

-   Enhanced File Search
    -   I can leverage `completing-read` to process the file list.
    -   Using `vertico`, I gain the advantage of fuzzy completion, making file search more efficient and user-friendly.

<!--listend-->

-   Alternative Methods
    -   I seldom need to find a single file using `find-file`
    -   Instead, I rely on bookmarks, `dired` and `recentf`

<!--listend-->

-   Dired Benefits
    -   While `find-name-dired` offers the benefits of a `dired` buffer, I generally don't need to perform specific actions on files found this way.
    -   Most of the time, I just need to jump to a file quickly.
    -   A dired bufer can be generated through `embark-export` anyway when going through a file fuzzy search at any point.

<!--listend-->

-   Customization and Flexibility
    -   External tools often provide more customization options, which align better with my workflow.
    -   These tools integrate seamlessly with my Emacs setup, enhancing my overall productivity.

By considering these reasons, I've found that external tools better fit my workflow compared to the built-in Emacs find functions when wanting to find a file quickly.

**P.S.** I still have a fondness for the basic `find` command, which I continue to use, particularly on the command line and especially when operating through SSH. However, I still can't remember how to prune directories!, but then who can? 😀
