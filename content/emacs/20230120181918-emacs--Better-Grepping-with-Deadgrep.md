---
title: "More flexible grepping with deadgrep"
author: ["James Dyer"]
lastmod: 2023-01-20T00:00:00+00:00
tags: ["ripgrep", "grep", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230120181918-emacs--Better-Grepping-with-Deadgrep.jpg"
---

I seem to be grepping a lot recently and I think the way I use `deadgrep` can be improved a little.

<!--more-->

{{< figure src="/emacs/20230120181918-emacs--Better-Grepping-with-Deadgrep.jpg" class="emacs-img" >}}

Currently `deadgrep` defaults to a recursive `ripgrep` from the `default-directory` which is generally the current directory of the buffer, but I find that by default I tend to mostly want to grep from a top level directory (yes I know, almost like a project!).

I would like to have a typical **Find All References** type of functionality from my grepping and not to rely on `xref` as I will not necessarily ever know if any `xref` functionality is supported for any file that I am working on and for the moment I am not using any connection to an LSP server.  I would like a simple generic process that can be used across all files and I think `deadgdrep` can help me out with this.

I would like to bind to `S-f12` to grep from the "project" top level with the search set to whatever string is under my cursor; this should enable a quick workflow involving jumping around files within the top level directory structure bouncing back and forth between the `deadgrep` buffer.

I have chosen the binding of `S-f12` for consistency across IDEs as I am often required to use VSCode and Visual Studio for work.

In addition I would like to replace my usual local directory grepping where I use `grep` with `deadgrep` for a more unified approach.

So I created the following two functions:

```elisp
(defun my/deadgrep ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (setq search-term
            (read-from-minibuffer "Search : "))
    (setq search-term
          (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
    )
  (deadgrep search-term home-dir)
  )
```

```elisp
(defun my/grep ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (setq search-term
            (read-from-minibuffer "Search : "))
    (setq search-term
          (read-from-minibuffer "Search : " (thing-at-point 'symbol)))
    )
  (deadgrep search-term)
  )
```

home-dir as you might guess is where my top level directory resides.

I came to realise that when I am in a `dired` buffer I don't actually ever want to grep with the string under the cursor (which of course would most likely be a file or directory) but only when I am in a file.

I toyed around with the idea of having a single (interactive "p") function so it would accept a prefix command and then perform the following kind of logic:

```elisp
((equal current-prefix-arg nil)   ; no C-u
 (do top level grep))
((equal current-prefix-arg '(4))  ; C-u
 (do local grep))
((equal current-prefix-arg 1)     ; C-u 1
 (do some other grepping))
```

however this had the unintended consequence of pushing through the prefix command to `deadgrep` and therefore it would not start immediately but wait for user interaction.  I couldn't see a way round this so had to split my grepping into both a `S-f12` and `M-f12` for each function call; not much of a big deal.

As I am just running a single deadgrep instance using:

```elisp
(setq deadgrep-max-buffers 1)
```

I also needed to add the following as I will always want to kill the current process if I am starting a new one.

```elisp
(setq kill-buffer-query-functions nil)
```
