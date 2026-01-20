---
title: "Why-Ediff-Is-My-Favourite-Microish-Feature"
author: ["James Dyer"]
lastmod: 2024-08-09T21:00:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240809210013-emacs--Why-Ediff-Buffers-Is-My-Favourite-Microish-Feature.jpg"
---

There have been some interesting posts recently regarding favourite micro-features in Emacs. These are seemingly very small built-in features, probably single functions, that someone can't live without and which are not really present in any other text editor.

<!--more-->

I saw \`fill-paragraph\` mentioned and yes, I would certainly agree. Not for reorganizing text, but more for pulling apart a long line alist or a long log string that I need to define more clearly.

When I first started to consider mine, a feature naturally jumped out at me. However, this probably wouldn't be defined as a micro-feature or something that isn't built into another editor (although that might not necessarily be a prerequisite). It isn't as substantial as something like Magit or Org-mode, but as a software engineer, it is something I can't live without and makes file comparison and merging a breeze. Of course, I'm talking about **ediff** and its associated **diff-mode**.

`diff-mode` in Emacs provides an interface for viewing and interacting with diffs, which are changes between file versions and I use it so much that I have bound it to permanent keybindings for the different variants that I commonly use:

```elisp
(global-set-key (kbd "C-x v e") 'vc-ediff)
(global-set-key (kbd "M-s x") #'diff-buffer-with-file)
(global-set-key (kbd "M-s =") #'ediff-buffers)
```

Note that I have deliberately provided a version control keybinding extension for `vc-ediff` to fit more easily into the other "C-x v" keybinding set.

As is often with Emacs there is a little configuration required to get it to work optimally for a users taste, here is my config:

```elisp
(use-package diff-mode
  :hook
  ((diff-mode . (lambda ()
                  (define-key diff-mode-map (kbd "M-j") nil)
                  (define-key diff-mode-map (kbd "M-k") nil)
                  (define-key diff-mode-map (kbd "M-h") nil)
                  (define-key diff-mode-map (kbd "M-l") nil)))
   (ediff-prepare-buffer . org-fold-show-all)
   (ediff-prepare-buffer . (lambda () (visual-line-mode -1))))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-highlight-all-diffs t)
  (ediff-split-window-function 'split-window-horizontally))
```

Note that I have removed the default keybindings \`M-j\`, \`M-k\`, \`M-h\`, and \`M-l\` in =diff-mode- which clash with my default M=&lt;vim-keybindings&gt; that I am currently using for navigation.

When preparing a buffer for `ediff` I can use the prepare-buffer hooks which are run for each file in the comparison. I find that for me it is not uncommon to compare an org file, especially my Emacs literate init file, and therefore it is beneficial to unfold all org-mode content to facilitate better alignment and ensuring all content in org-mode files are visible during the `ediff` session.

Talking of alignment, depending on the context I may or may not have `visual-line-mode` enabled for a file to be compared, so with the prepare buffer hooks I also disable `visual=line-mode` for clarity which prevents line wrapping, making differences more readable.

I have also set some custom variables as follows:

-   **Simple Window Setup**: `ediff-window-setup-function` is set to `ediff-setup-windows-plain` to use a straightforward, no-frills window layout which is all within the a single Emacs frame.  I use a tiling manager so it is especially annoying when a half screen control window pops up!  I think this is one of those not uncommon default Emacs settings that is somewhat questionable.
-   **Highlighting All Diffs**: Enabling `ediff-highlight-all-diffs` ensures that all differing regions are vividly highlighted.
-   **Horizontal Split**: Setting `ediff-split-window-function` to `split-window-horizontally` ensures side-by-side comparison of files, which again I think should be the Emacs default.
