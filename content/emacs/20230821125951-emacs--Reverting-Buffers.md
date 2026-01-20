---
title: "Reverting Buffers"
author: ["James Dyer"]
lastmod: 2023-08-21T12:59:00+01:00
tags: ["emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230821125951-emacs--Reverting-Buffers.jpg"
---

It is not uncommon for me to want to revert my current buffer.

<!--more-->

{{< figure src="/emacs/20230821125951-emacs--Reverting-Buffers.jpg" class="emacs-img" >}}

For a long while I have used `(find-alternate-file)` which by default is bound to `C-x C-v`

Recently I switched to mapping this keybinding to `revert-buffer` but now I have found out about:

```nil
(revert-buffer-quick &optional AUTO-SAVE)

Like ‘revert-buffer’, but asks for less confirmation.
If the current buffer is visiting a file, and the buffer is not
modified, no confirmation is required.
```

It is bound to `C-x x g` and has the added benefit of not always asking for confirmation!

I will try and add this new command to my muscle memory but shall I just unbind my old `revert-buffer` key-mapping or rebind `C-x C-v` to this new function also?, in other words should I fully commit?
