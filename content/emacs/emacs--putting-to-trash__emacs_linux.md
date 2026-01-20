---
title: "Putting to Trash"
author: ["James Dyer"]
lastmod: 2022-09-15T00:00:00+01:00
tags: ["wastebasket", "trash", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--putting-to-trash__emacs_linux.jpg"
---

I was recently reading a post about deleting files from within emacs and pushing them to the local Trash, this seems like a good idea especially now I am using dired more often.

<!--more-->

{{< figure src="/emacs/emacs--putting-to-trash__emacs_linux.jpg" class="emacs-img" >}}

After using `describe-function` and typing **_trash_** there was a single completion, namely `move-file-to-trash` from this I figured out I should add the following my .emacs:

```elisp
(setq trash-directory "~/.local/share/Trash/files")
(setq delete-by-moving-to-trash t)
```

I am running this on OpenSuse so the location of the Trash folder may vary from system to system.

For a more bespoke copying mechanism there is potentially the ability to define system-move-file-to-trash accepting **_file_** as an argument, but currently I don't need anything to be that sophisticated.
