---
title: "Syncing Tab Theme After Theme Load"
author: ["James Dyer"]
lastmod: 2024-10-01T09:50:00+01:00
tags: ["tab-bar", "tab", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241001092329-emacs--Syncing-Tab-Theme-After-Theme-Load.jpg"
---

Previously I wrote about the incompatibility of some Emacs themes with the tab-bar and my temporary fix:

<!--more-->

[Syncing Tab Bar To Theme]({{< ref
"/emacs/20240817082349-emacs--Syncing-Tab-Bar-To-Theme.md" >}})

{{< figure src="/emacs/20241001092329-emacs--Syncing-Tab-Theme-After-Theme-Load.jpg" width="100%" >}}

The tab bar was introduced in Emacs 27.1, during which I created a function to apply some simple logic to make the tabs look moderately decent and fit in with the new theme. The only thing I couldn't figure out was how to activate the function after a theme load, as there didn't seem to be a hook.

Well, actually there does seem to be such a hook and it seems to have gone into Emacs 29.  As with a lot of these little tweaks I found this on reddit in the emacs subreddit:

<https://www.reddit.com/r/emacs/comments/1ft3wwm/did_the_much_needed_theme_change_hook_arrive_with/>

From my previous post I wrote :

> Now that the `defun` is available, how to activate it? My first thought was to add a hook for when a theme is loaded, but there doesn't seem to be such a feature!?  I also tried advising around \`load-theme\`, but that didn’t go well either, it worked for themes that were not loaded, but for those that were, a change in state was not detected, and the tab theme tweak was not applied.

The solution is to simply add the following to your init file. Now that I know about this, I might even find some other use for it.

```elisp
(defun my/after-theme-loaded(theme)
  (my/sync-tab-bar-to-theme))

(add-hook 'enable-theme-functions 'my/after-theme-loaded)
```

with the original `my/sync-tab-bar-to-theme` as follows:

```elisp
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive))) ;; Fallback to mode-line-inactive
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
```

My tab theme fix is now activated on every theme load, and I no longer have to rely on muscle memory to make Emacs look colour-coherent when changing themes, which I do a lot!! 😀
