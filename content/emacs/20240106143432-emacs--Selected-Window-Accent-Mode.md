---
title: "My first emacs package - *selected-window-accent-mode*"
author: ["James Dyer"]
lastmod: 2024-01-06T21:00:00+00:00
tags: ["window", "selected-window-accent-mode", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/selected-window-accent-mode-00.jpg"
---

From my previous two posts regarding defining a Selected Window Accent some interesting posts from [irreal](https://irreal.org/blog) arose, especially:

<!--more-->

[Marking The Active Window](https://irreal.org/blog/?p=11867#comment-6354017310)

Where it looks like the simple `mode-line-active` and `mode-line-inactive` was a good way to indicate the current focussed window, and I do agree.

However to practice my elisp and to create my first emacs package I thought I would bring together my preference which is to have a tiling window manager type of focus involving typically a border around the current window while leveraging the usual customization options that come with emacs.

I have put it on [github/captainflasmr](https://github.com/captainflasmr/selected-window-accent-mode) for now and below is the **README** for a package called **`selected-window-accent-mode`**

It's a bit rough around the edges (pardon the pun!) but I think it might be a good starting point for further improvements.

---


## selected-window-accent-mode {#selected-window-accent-mode}


### Summary {#summary}

The Selected Window Accent Mode is an Emacs package designed to visually distinguish the currently selected window by applying a unique accent color to its fringes, mode line, header line, and margins.

{{< figure src="/emacs/selected-window-accent-mode-00.jpg" width="100%" >}}

---


### Quick Start (emacs 29) {#quick-start--emacs-29}

Add the following to the emacs init for a tiling window manager feel (see image above):

```elisp
(use-package selected-window-accent-mode
  :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color "#916941")
  (selected-window-accent-mode-style 'tiling))

(selected-window-accent-mode 1)
```

---


### Installation {#installation}


#### use-package (emacs 29) {#use-package--emacs-29}

Put the following into your emacs init file:

```elisp
(use-package selected-window-accent-mode
  :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode"))
```


#### use-package (MELPA) {#use-package--melpa}

-   TODO (see roadmap below)


#### from source {#from-source}

Download the \`.el\` file and place it in your Emacs \`load-path\`.

Then either manually load it or add it to your configuration to be loaded at startup.

```elisp
(require 'selected-window-accent-mode)
```

---


### Usage {#usage}

Interactively Toggle the mode on and off `M-x selected-window-accent-mode`

Interactively change the current style `M-x switch-selected-window-accent-style` which will present a `completing-read` selection in the minibuffer

The styles that are currently supported :

-   default
-   tiling
-   subtle

see **roadmap** below for a description.

Typically I have bound these two interactive functions to a new keymap where I keep all my emacs visual change functions.

```elisp
(defvar my-win-keymap (make-sparse-keymap))
(global-set-key (kbd "M-o") my-win-keymap)
(define-key my-win-keymap (kbd "a") 'selected-window-accent-mode)
(define-key my-win-keymap (kbd "y") 'switch-selected-window-accent-style)
```

---


### Examples {#examples}


#### Example 1 - Default / custom color {#example-1-default-custom-color}

{{< figure src="/emacs/selected-window-accent-mode-01.jpg" width="100%" >}}

To enable the accent mode automatically upon starting Emacs, add the following line to your \`.emacs\` or \`init.el\` file:

```elisp
(use-package selected-window-accent-mode
  :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
  :custom
  (selected-window-accent-custom-color "goldenrod")
  (selected-window-accent-mode-style 'default))

(selected-window-accent-mode 1)
```

This will accent the modeline only for the selected window with the `goldenrod` color.

---


#### Example 2 - Tiling / custom color / custom fringe thickness {#example-2-tiling-custom-color-custom-fringe-thickness}

{{< figure src="/emacs/selected-window-accent-mode-02.jpg" width="100%" >}}

```elisp
(setq selected-window-accent-fringe-thickness 6)
(setq selected-window-accent-custom-color "#4179b2")
(setq selected-window-accent-mode-style 'tiling)

(selected-window-accent-mode 1)
```

This will accent the full outline of the window with the color #4175b2 more akin to a tiling window manager.

---


#### Example 3 - Tiling / theme highlight color {#example-3-tiling-theme-highlight-color}

{{< figure src="/emacs/selected-window-accent-mode-03.jpg" width="100%" >}}

```elisp
(setq selected-window-accent-custom-color nil)
(setq selected-window-accent-mode-style 'tiling)

(selected-window-accent-mode 1)
```

This will accent the full outline of the window with the `highlight` color taken from the current theme.

---


#### Example 4 - Subtle / custom fringe thickness (thick) {#example-4-subtle-custom-fringe-thickness--thick}

{{< figure src="/emacs/selected-window-accent-mode-04.jpg" width="100%" >}}

```elisp
(setq selected-window-accent-fringe-thickness 40)
(setq selected-window-accent-custom-color nil)
(setq selected-window-accent-mode-style 'subtle)

(selected-window-accent-mode 1)
```

This will accent the modeline and just the left fringe and in this case be quite a pronounced thick accent.

---


### Customization {#customization}

Can be done through the customization interface:

**Selected Window Accent Group group:**

Customization group for the selected-window-accent package.

**Selected Window Accent Custom Color**

Custom accent color for the selected window. Set this variable to change the accent color.

-   `None` - color will be using the current `highlight` face
-   `Custom Color` - input color name or Hex

**Selected Window Accent Fringe Thickness:** Integer:

The thickness of the fringes in pixels.

**Selected Window Accent Mode**:  `Boolean`: Toggle

Non-nil if Selected-Window-Accent mode is enabled

**Selected Window Accent Mode Style**

Current style for accenting the selected window.

-   `default` - just modeline accent
-   `tiling` - window border accent
-   `subtle` - left and modeline accent

---


### Minor Mode {#minor-mode}

The `selected-window-accent-mode` is a global minor mode that you can toggle to enable or disable the accenting of the selected window.

When enabled, it distinguishes the selected window with a special accent color.

---


### Hooks {#hooks}

Two hooks are used to automatically update the window accents when the window configuration or state changes:

-   window-configuration-change-hook
-   window-state-change-hook

These are added when the `selected-window-accent-mode` is enabled and removed when disabled.

---


### BUGS {#bugs}

The current version is pretty rough and probably definitely pre-alpha.

Fix these to get to a tagged Version 0.1.

In order of priority

-   **TODO** header-line not shown on window split.
-   **TODO** adjust the not selected-window margin to avoid little window navigation. disruption, hence translating a fringe pixel width to a number of margin characters, not quite sure how I am going to do this yet.
-   **TODO** Incorporate `mode-line-active` and `mode-line-inactive` somehow as this would make more sense especially in the 'default mode.
-   **TODO** excess selected-window disruption in header-line.
-   **WATCHING** careful with removing header-line on all windows, for example magit commit window and probably some others may need to add some logic depending on mode.

---


### ROADMAP {#roadmap}


#### 1. add to MELPA {#1-dot-add-to-melpa}


#### 2. define more custom variables: {#2-dot-define-more-custom-variables}

-   accent color saturation adjustment
-   accent color darken adjustment
-   accent color hue adjustment


#### 3. define which theme face attribute to use as the main accent color {#3-dot-define-which-theme-face-attribute-to-use-as-the-main-accent-color}

Currently the default is to use the `highlight` face


#### 4. **DOING** implement accent styles {#4-dot-doing-implement-accent-styles}

-   **DONE** `default` - _bottom_ - full height modeline
-   **DOING** `tiling` - _top/right/bottom/left_ - typically a squished modeline and header line to a general accent thickness to provide a typical tiling window manager focussed outline experience
-   **DOING** `subtle` - _left/bottom_
-   **TODO** `full` - _top/right/bottom/left_ - full height modeline (currently implemented as `tiling` but will be moved when tiling is more "tiling")
