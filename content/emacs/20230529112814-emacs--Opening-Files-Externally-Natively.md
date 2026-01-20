---
title: "Opening Files Externally from dired"
author: ["James Dyer"]
lastmod: 2023-09-16T16:43:00+01:00
tags: ["emacs", "elisp", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230529112814-emacs--Opening-Files-Externally-Natively.jpg"
---

I have been using **`C-RET`** or **`W`** in `dired` for a while now to open a file externally via `browse-url-of-dired-file`.

<!--more-->

{{< figure src="/emacs/20230529112814-emacs--Opening-Files-Externally-Natively.jpg" class="emacs-img" >}}

I was never quite sure how that worked but it just worked, however now it doesn't work so I need to do something about it.

I suspect that the reason it doesn't work now is that I have been hopping around different window managers / compositors.  My tried and tested workhorse environment is generally kde plasma on arch where the `browse-url-of-dired-file` was working just fine, but now I have switched to **sway / wayland** or **i3 / x11** this functionality seems to be broken.

I have an idea it's probably related to the window manager and the way each one handles default applications through the XDG portal.  I think I need to find a more agnostic opening method.

My first thought was the **`openwith`** package but this had the side effect of opening every single file externally (especially when coming back from a `desktop-save`) which is not something I find desirable.  I prefer the default dired mechanism for opening a file in emacs itself and I want an explicit mechanism to push a file to a native external application.

My next thought was `dired-do-shell-command` which can be activated by `!` or `X` in `dired`.  This brings up a default opening application called `xloadimage` which I'm assuming runs through possibly an XDG default application opening mechanism linked to the running window manager.  Well I don't really want to set a default application for each window manager / environment so I'm not sure that this is right approach either.

My next thought was how does `dired` determine which opening mechanism to use, I mean where does `xloadimage` come from?

Well the answer is twofold, firstly `dired-guess-shell-alist-default` and secondly `dired-guess-shell-alist-user`

The first list includes many lines in an alist format, for example:

```elisp
("\\.jpe?g\\'" "xloadimage")
```

I don't really want to touch the default settings so lets have a look at the user variant which is described as follows:

> User-defined alist of rules for suggested commands.
> These rules take precedence over the predefined rules in the variable
> ‘dired-guess-shell-alist-default’ (to which they are prepended).

I therefore appended the prepended list! (even though it was empty anyway)

```elisp
(setq dired-guess-shell-alist-user
      (append '(("\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\)$" "gwenview")
                ("\\.\\(mp4\\|mkv\\|avi\\|mov\\|wmv\\|flv\\|mpg\\)$" "mpv"))
              dired-guess-shell-alist-user))
```

Now to the testing stage...

---

I seem to have an intermittent problem when running `! (dired-do-shell-command)` in that the file isn't always opened in gwenview, however mpv opens every single time!.  I literally have no idea what is going on here but `& (dired-do-async-shell-command)` opens every time! so lets use that then, it might be better to run the file opening asynchronously anyway.

However, another issue presents itself.  By default the async shell window always opens a new buffer / window and is polluting my emacs window layout!!!!.  Well certainly I can't have that so I decide to use the `display-buffer-alist` as follows:

```elisp
(defvar go-away-repl-regexp
  (rx bos "*" (or "Async")
      (zero-or-more nonl))
  "Regexp for matching windows to disappear")

(add-to-list 'display-buffer-alist
             `(,go-away-repl-regexp
               display-buffer-no-window
               (inhibit-same-window . t)))
```

Done.
