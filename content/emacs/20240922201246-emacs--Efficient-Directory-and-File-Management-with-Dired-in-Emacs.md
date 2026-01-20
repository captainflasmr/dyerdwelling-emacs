---
title: "Simple Directory and File Creation in Dired"
author: ["James Dyer"]
lastmod: 2024-09-24T20:00:00+01:00
tags: ["emacs", "dired", 2024, "elisp"]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240922201246-emacs--Efficient-Directory-and-File-Management-with-Dired-in-Emacs.jpg"
---

Today I'll simply delve into two custom functions that I've had in my init file for quite some time, `my/dired-create-directory` and `my/dired-create-empty-file`

<!--more-->

{{< figure src="/emacs/20240922201246-emacs--Efficient-Directory-and-File-Management-with-Dired-in-Emacs.jpg" width="100%" >}}

I use them frequently enough that I thought they were worth mentioning and they augment my `dired` workflow when mapped to `dired-mode-map` keybindings.

For many years I would create a new file or folder in one of two ways, through the command line, or a GUI file explorer (with either a click in or bound F2 to edit the name), Emacs comes with it's own directory manager so why not augment it with these features?

What about `find-file` I hear you say?, just press C-x C-f. This will prompt for a filename using the current directory of the current buffer and if you type a unique name it will create the file for you.  Unfortunately though this will not always work in a simple predictable manner when the minibuffer completion mechanism kicks in.

**my/dired-create-directory**

This custom function simplifies creating a new directory by prompting for a name in the minibuffer, bypassing the default completion mechanism to avoid unwanted name suggestions.

```emacs-lisp
(defun my/dired-create-directory ()
  "Wrapper to dired-create-directory to avoid minibuffer completion."
  (interactive)
  (let ((search-term
         (read-from-minibuffer "Dir : ")))
    (dired-create-directory search-term)))
```

-   **Purpose:** This function is a wrapper around the `dired-create-directory` function, designed to avoid the potential confusion of minibuffer completion and directly ask for the directory name.
-   **Usage:** When invoked, it prompts the user to input a directory name and creates it using the `dired-create-directory` function.

**my/dired-create-empty-file**

Similarly, this custom function facilitates the creation of an empty file by asking for the filename in the minibuffer without the default completion.

```emacs-lisp
(defun my/dired-create-empty-file ()
  "Wrapper to dired-create-empty-file to avoid minibuffer completion."
  (interactive)
  (let ((search-term
         (read-from-minibuffer "File : ")))
    (dired-create-empty-file search-term)))
```

-   **Purpose:** This function wraps around `dired-create-empty-file`, providing a straightforward prompt for creating a new file.
-   **Usage:** When triggered, it asks the user to input a file name and then creates an empty file with that name.

These functions are bound to `dired-mode-map` as follows:

-   `_`: Create an empty file using `my/dired-create-empty-file`
-   `+`: Create a new directory using `my/dired-create-directory`

This gives me a good ergonomic LShift+&lt;Right hand of number row&gt; combination and is therefore not too taxing on the hands and each key command are just next to each-other to emphasize their similar nature.

In addition the dired mode map keybinding for creating a new directory is bound to + so there is consistency there too.

I suspect there are probably many other ways to achieve this functionality in Emacs, in this case I am specifically relying on `dired` function calls here but there is also the `make-directory` function and the `shell-command` for example if you want to drop in to using the operating systems tool set.
