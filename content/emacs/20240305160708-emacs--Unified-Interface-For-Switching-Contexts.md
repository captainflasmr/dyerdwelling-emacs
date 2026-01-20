---
title: "Unified Interface for Switching Contexts - Switch to Thing"
author: ["James Dyer"]
lastmod: 2024-03-05T17:56:00+00:00
tags: ["recentf", "emacs", "elisp", "consult", "buffer", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240305160708-emacs--Unified-Interface-For-Switching-Contexts.jpg"
---

Now I have ditched `save-desktop` for `recentf` which gives me a faster startup time but still being able to quickly access my most common files (i.e. those I have most recently opened) I have realised that I would like to have quick access to other Emacs resources after a startup.

<!--more-->

{{< figure src="/emacs/20240305160708-emacs--Unified-Interface-For-Switching-Contexts.jpg" width="100%" >}}

This has led me to creating a unified interface of sorts to switch to different contexts through a single keybinding.  If you think that sounds vague, well you are correct!, I shall try and explain myself.

---

`save-desktop` has served me well for many many years as a quick method to call up my last session including recreating buffers in their last positions and thus repopulating the ibuffer list and re-enabling the default Emacs `switch-to-buffer` mechanism.

Unfortunately as I am now fiddling around with elisp it means that restarting Emacs has become much more commonplace and I don't really want to wait around for `save-desktop` to recreate itself after each reload.  In addition there were the odd peculiarities that I could never seem to resolve.

Out of the box `recentf` has its own basic opening mechanism not tied into anything particularly modern with a list of files populated for selection in a separate buffer, but `consult-recent-file` solves this by giving me a nice completing-read experience.

So now I can recall my most recent files in a nice completing-read way how about preserving a familiar switching experience, I could bind `consult-recent-file` to `C-x b`, hence `switch-to-buffer` for continuity, but then of course I can't switch to buffer!  should I have separate keybinding for `consult-recent-file`?, well maybe, but what else would I like to quickly switch to?

Well of course buffers! I shouldn't get carried away without forgetting the Emacs staple switching mechanism and in fact `switch-to-buffer` is perfectly fine and is completing-read enabled (I could also use `consult-buffer`)

Bookmarks! (can I start a sentence like that?).  This is a very underrated feature that I have become more accustomed to using and of course the USP being that a bookmark can reference a file, info page, directory, or pretty much anything you would want to access in Emacs.  Well that has a separate selection mechanism too via `bookmark-bmenu-list` which displays a list of existing bookmarks in a separate buffer.  Again the brilliant `consult-bookmark` comes to the rescue converting the bookmark mechanism into a completing-read, but how do I access this list?, another keybinding?, well maybe.

I am also one of those people that switches themes very often, like several times a day, well how do I do this?, `load-theme`, `customize-themes`?.  Out of the box theme selecting has a separate selection mechanism too which displays a list of existing themes in a separate buffer.  Again the brilliant `consult-theme` comes to the rescue giving a nice completing-read selection mechanism, but how do I access this list?, another keybinding?, well maybe.

So I have four contexts to switch to and I don't want a single keybinding for each one so can I merge all of these contexts into a single completing-read under a single keybinding, with the completion mechanism of choice performing the heavy lifting to give me a fast unified method of quick switching? (and breathe! 😵‍💫), the answer of course is - _"this is Emacs dummy, of course you can!"_.

I thought this would also be a perfect opportunity to further my understanding of elisp by tackling this problem myself, which is how I came up with `my/switch-to-thing`, which is described below:

---


## my/switch-to-thing {#my-switch-to-thing}

The provided Emacs Lisp function \`my/switch-to-thing\` offers a unified interface within Emacs to quickly switch contexts. It enables a user to perform one of several actions based on the user's selection from a prompted completion list. The specific actions that can be taken are:

1.  ****Switching to an Open Buffer****: If the selection matches the name of an open buffer, the function switches the current window to display that buffer.

2.  ****Opening a Recent File****: If the selection matches an entry in the list of recently opened files, that file is opened in the current window.

3.  ****Jumping to a Bookmark****: If the selection matches a bookmark's name, Emacs navigates to that bookmark.

4.  ****Changing the Emacs Theme****: If the selection starts with the prefix "Theme: ", the function interprets the rest of the selection as the name of a theme. It then loads and applies that theme.

Here is a break down of its behavior:

-   ****Initialization****: It first creates lists of current buffer names (\`buffers\`), recent files (\`recent-files\`), bookmark names (\`bookmarks\`), and available themes formatted with a "Theme: " prefix (\`themes\`). These lists are then concatenated into \`all-options\`, forming a comprehensive list of choices for the user.

-   ****User Input****: It prompts the user with a "\`Switch to:\`" message using \`completing-read\`, offering autocomplete functionality based on the \`all-options\` list. This prompt also integrates with Emacs' \`file-name-history\` for enhanced usability.

-   ****Action Selection****: Based on the user's choice (\`selection\`), the function uses \`pcase\` to pattern-match and decide the action:
    -   If the selection is an open buffer, it switches to that buffer (\`switch-to-buffer\`).
    -   If the selection is a bookmark, it jumps to that bookmark (\`bookmark-jump\`).
    -   If the selection is prefixed with "Theme: ", it extracts the theme name, converts it back to a symbol, and loads that theme (\`load-theme\`).
    -   If none of the above conditions are met, it interprets the selection as a file path/name and attempts to open it (\`find-file\`).

This function effectively encapsulates multiple navigation and customization actions within Emacs into a single command, streamlining the user's workflow by providing a centralized interface for common tasks.

```elisp
(defun my/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark, or change the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (themes (custom-available-themes))
         (all-options (append buffers recent-files bookmarks
                              (mapcar (lambda (theme) (concat "Theme: " (symbol-name theme))) themes)))
         (selection (completing-read "Switch to: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           '(metadata . ((category . file)))
                                         (complete-with-action action all-options str pred)))
                                     nil t nil 'file-name-history)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      ((pred (lambda (sel) (string-prefix-p "Theme: " sel)))
       (load-theme (intern (substring selection (length "Theme: "))) t))
      (_ (find-file selection)))))
```
