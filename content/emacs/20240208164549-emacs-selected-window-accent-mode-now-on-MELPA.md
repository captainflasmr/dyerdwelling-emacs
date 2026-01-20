---
title: "selected-window-accent-mode now on MELPA"
author: ["James Dyer"]
lastmod: 2024-02-09T14:58:00+00:00
tags: ["window", "selected-window-accent-mode", "melpa", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/selected-window-accent-mode-00.jpg"
---

The **selected-window-accent-mode** is now present on **MELPA** and is my first Emacs package!

<!--more-->

It is designed to visually distinguish the currently selected window by applying a unique accent color to its fringes, mode line, header line, and margins.

{{< figure src="/emacs/selected-window-accent-mode-00.jpg" width="100%" >}}

See <https://github.com/captainflasmr/selected-window-accent-mode> for more information and examples.


## Whats New {#whats-new}

**Version 0.6.0**

**DONE** `ISSUE #1` Do not apply highlighting when frame only contains 1 window when `selected-window-accent-smart-borders` is set

**DONE** define accent color saturation adjustment

**DONE** define accent color darken adjustment

**DONE** highlight selected tab with same accent color

**DONE** add to MELPA


## Quick Start {#quick-start}

To use left and bottom accent based on the themes highlight colour:

```elisp
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color nil)
  (selected-window-accent-mode-style 'subtle))
```

OR define your own colour:

```elisp
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color "#427900")
  (selected-window-accent-mode-style 'subtle))
```


## Alternative window highlighting packages {#alternative-window-highlighting-packages}

There exist a few Emacs packages that perform window highlighting but that don't quite provide the feature set of selected-window-accent.

selected-window-accent focusses more on clearly but non-intrusively highlighting the currently selected/focussed window by highlighting aspects of the window border without having to modify the appearance of non-selected windows, hence more akin to a tiling window manager.


### dimmer {#dimmer}

"This package provides a minor mode that indicates which buffer is currently active by dimming the faces in the other buffers."

This is the closest in functionality to selected-window-accent, the difference being that dimmer dims non selected windows rather than accent the selected window.

dimmer can be used in conjunction and will complement selected-window-accent to further enhance the emphasizing of the selected window.


### hiwin {#hiwin}

"This package provides a minor-mode to change the background colour of the non active window."

It uses overlays to highlight non active windows, so is similar to dimmer but is less subtle in its highlighting mechanism and hasn't been updated in excess of 10 years.


### color-theme-buffer-local {#color-theme-buffer-local}

"This package lets you set a color-theme on a per-buffer basis."

Unlike dimmer and hiwin this package isn't related to the concept of a selected window but more of defining different themes for different windows to distinguish them.


### solaire-mode {#solaire-mode}

"This package is designed to visually distinguish "real" buffers (i.e. file-visiting code buffers where you do most of your work) from "unreal" buffers (like popups, sidebars, log buffers, terminals, etc) by giving the latter a slightly different -- often darker -- background"

Unlike dimmer and hiwin this package isn't related to the concept of a selected window but more of distinguishing between collections of IDE like elements within Emacs.


## Roadmap / Improvements {#roadmap-improvements}

**TODO** add darken desaturated and tab highlight examples to README

**TODO** define accent color hue adjustment

**TODO** define compensating margin

**TODO** Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the 'default mode.

**TODO** header-line not shown on window split - I have a funny feeling this could be very difficult, if not impossible!

**TODO** restore modeline height when switching between modes

**TODO** adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.

**TODO** excess selected-window disruption in header-line. (not sure I can do much about this)

**TODO** define which theme face attribute to use as the main accent color - Currently the default is to use the `highlight` face

**WATCH** possible overheads of updating visual elements for each window?

**WATCH** careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.
