---
title: "Syncing Tab Bar To Theme"
author: ["James Dyer"]
lastmod: 2024-08-24T11:00:00+01:00
tags: ["emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2024-08-24-10-55-54.jpg"
---

Using tabs as part of my workflow has enabled a nice encapsulation and collection of files per tab that I can quickly switch to.

<!--more-->

An issue I consistently run into however is tab theming. I'm a constant theme switcher, not just in Emacs but also operating system wide, and I often switch wallpapers.

Unfortunately, with the advent of tabs in version 27.1, many Emacs themes lack styles for the new tab faces, resulting in a wonky disjointed appearance.

For example:

{{< figure src="/emacs/2024-08-24-10-55-54.jpg" width="100%" >}}

Elisp can come to the rescue again of course, this is my attempt at making the tabs fit in a little better with an incompatible theme:

```elisp
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive)))
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
```

which does its best to fit tab themes to the loaded theme, to give something like:

{{< figure src="/emacs/2024-08-24-10-56-26.jpg" width="100%" >}}

There is a bit of logic here, but essentially, for the whole tab-bar theming, I just pick up the foreground and background faces defined in the theme and for the selected tab `tab-bar-tab`, the default theme foreground switches to ensure the selected tab will always stand out.

For inactive tabs, I set the foreground to that used in the `mode-line-inactive`.

Now that the `defun` is available, how to activate it? My first thought was to add a hook for when a theme is loaded, but there doesn't seem to be such a feature!?  I also tried advising around \`load-theme\`, but that didn’t go well either, it worked for themes that were not loaded, but for those that were, a change in state was not detected, and the tab theme tweak was not applied.

My current solution is simply to make the function interactive and build up some muscle memory to run it every time I switch themes and the tab theming looks wonky.  I guess I could wrap around `load-theme` with my own theme loading function, then activate `my/sync-tab-bar-to-theme` afterwards, but I would rather keep the default theme selection function and run this function later.

P.S I also have a little idea to perhaps add this functionality to [selected-window-accent-mode](<https://github.com/captainflasmr/selected-window-accent-mode>) as this is related to clearly defining a selected element in Emacs.
