---
title: "Window Divider Mode"
author: ["James Dyer"]
lastmod: 2023-11-05T16:01:00+00:00
tags: ["window", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231006165956-emacs--Divider-Mode.jpg"
---

I was playing around with the look of emacs and thought it might be nice to have more control over the border between windows (I have now learnt that this is referred to as a window divider!)

<!--more-->

{{< figure src="/emacs/20231006165956-emacs--Divider-Mode.jpg" width="100%" >}}

Although I could change the colour using `custom-set-faces` `(vertical-border ((t (:foreground "#444444"))))` I wanted the default thin dividing window line to be larger, but initially I couldn't seem to figure out how do change this.

I tried the vertical-border face `:width` `:height` at varying weights and in fact through the custom settings I toggled on and off pretty much everything, but to no avail.

This eventually led me to the built-in `window-divider-mode` which actually specifically handles the cosmetic divider issues, so I set the following:

```elisp
(setq window-divider-default-bottom-width 8)
(setq window-divider-default-right-width 8)
(setq window-divider-default-places t)

(window-divider-mode 1)
```

I decided that if I was going to have a window divider then I might as well have it "around" every window, hence the default-places set to `t` rather than a very specific position,  `right-only` or `bottom-only`
