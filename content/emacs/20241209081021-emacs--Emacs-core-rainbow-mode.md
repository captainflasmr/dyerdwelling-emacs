---
title: "Creating a small local elisp rainbow-mode solution"
author: ["James Dyer"]
lastmod: 2025-01-25T07:50:00+00:00
tags: ["emacs-core", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241209081021-emacs--Emacs-core-rainbow-mode.jpg"
---

In this post, as part of my ongoing mission to replace all (or as many as possible) external packages with pure elisp, I’ll demonstrate how to implement a lightweight alternative to the popular `rainbow-mode` package, which highlights hex colour codes in all their vibrant glory.  I use this quite often, especially when "ricing" my tiling window setup.

<!--more-->

{{< figure src="/emacs/20241209081021-emacs--Emacs-core-rainbow-mode.jpg" width="100%" >}}

Instead of relying on `rainbow-mode`, lets create a custom Emacs function to colourize hex values in the buffer, along with another function to clear these highlights. We'll also bind this functionality to modes like `prog-mode`, `org-mode`, and `conf-space-mode`.

---

Hexadecimal colour codes (like `#ff0000` for red, `#00ff00` for green, etc.) often appear in programming, configuration, or document files. Visualizing these colours directly within your buffer makes working with them much easier.

Here's how you can create a simple function to overlay these colours in Emacs:

The following function highlights any 3 or 6 character hex colour codes it finds in the current buffer (e.g., `#ff0000`, `#33aaff`, `#fff`). It uses Emacs overlays to render the colour as the background of the matched text.

```elisp
(defun my/rainbow-mode ()
  "Overlay colors represented as hex values in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max))
  (let ((hex-color-regex "#[0-9a-fA-F]\\{3,6\\}"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hex-color-regex nil t)
        (let* ((color (match-string 0))
               (overlay (make-overlay (match-beginning 0) (match-end 0))))
          (if (string-greaterp color "#888888")
              (overlay-put overlay 'face `(:background ,color :foreground "black"))
            (overlay-put overlay 'face `(:background ,color :foreground "white"))))))))
```

As an additional note, I have added a little logic regarding generating a contrasting foreground colour to help the hex text value remain prominent.

Sometimes you may want to remove all colour overlays from the current buffer. The following function clears the overlays applied by `my/rainbow-mode`:

```elisp
(defun my/rainbow-mode-clear ()
  "Remove all hex color overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max)))
```

Now that we have created `my/rainbow-mode`, let’s make it automatically activate in relevant modes like `prog-mode`, `org-mode`, and `conf-space-mode`. You can achieve this by adding hooks to these modes:

```emacs-lisp
(add-hook 'prog-mode-hook #'my/rainbow-mode)
(add-hook 'org-mode-hook #'my/rainbow-mode)
(add-hook 'conf-space-mode-hook #'my/rainbow-mode)
```

By doing this, whenever you open a buffer in a programming file, Org file, or certain configuration files, `my/rainbow-mode` will automatically highlight any hex colour codes present in the file.

---

And there we go!, by defining `my/rainbow-mode`, you can enjoy the benefits of colour-highlighting hex values in Emacs while staying true to a minimalist, package-free configuration!
