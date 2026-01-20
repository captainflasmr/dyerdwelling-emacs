---
title: "Winner Undo to Tab Bar History"
author: ["James Dyer"]
lastmod: 2024-02-03T10:56:00+00:00
tags: ["winner-mode", "tab-bar", "mastering-emacs", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240114145517-emacs--Winner-Undo-To-Tab-Bar-History.jpg"
---

Now I am using the `tab-bar` workflow I noticed that `winner-mode` was not working per tab in that the winner undo would return to the previous state of another tab, pretty annoying! 😕

<!--more-->

{{< figure src="/emacs/20240114145517-emacs--Winner-Undo-To-Tab-Bar-History.jpg" width="100%" >}}

But there is an easy fix, replace `(winner-mode 1)` with `(tab-bar-history-mode 1)`

and for example I replaced my following keybindings:

```elisp
(global-set-key (kbd "M-u") 'winner-undo)
(global-set-key (kbd "M-i") 'winner-redo)
```

with

```elisp
(global-set-key (kbd "M-u") 'tab-bar-history-back)
(global-set-key (kbd "M-i") 'tab-bar-history-forward)
```

Ironically I had just been reading a related section in **Mastering Emacs** and this configuration was recommended to avoid confusion and general vexation.

I would also recommend increasing the number of tab bar history elements remembered, the default is 10 and I quickly found myself running out.

Therefore I put in:

```elisp
(setq tab-bar-history-limit 100)
```
