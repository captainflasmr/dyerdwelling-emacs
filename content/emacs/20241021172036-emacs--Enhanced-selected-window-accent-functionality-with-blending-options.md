---
title: "selected-window-accent-mode with window colour blending options"
author: ["James Dyer"]
lastmod: 2024-10-22T08:40:00+01:00
tags: ["selected-window-accent-mode", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/selected-window-accent-mode-07.jpg"
---

I have enhanced `selected-window-accent` functionality with blending options to blend colourize the selected window (see below):

<!--more-->

<img src="https://raw.githubusercontent.com/captainflasmr/selected-window-accent-mode/main/demos/selected-window-accent-mode-demo-blend.gif" width="80%" />

<br />
<br />

This means that not only will the mode-line, fringes and header be accent coloured, but also the whole selected window background colour, tinted to the alpha chosen.

The features added are as follows:

-   Introduced `selected-window-accent-use-blend-background` and `selected-window-accent-use-blend-alpha` to allow blending of the accent colour with the background of the selected window a selected alpha amount.
-   Introduced new toggle functions and added to transient:
    -   `selected-window-accent-toggle-blend-background`
    -   `selected-window-accent-toggle-pywal`
-   Added function `selected-window-accent-blend-colors` to support blending of two colours.
-   Updated `selected-window-accent--set-foreground-color` and `selected-window-accent` functions to incorporate new blending feature.
-   Improved existing functions for setting and toggling accent features, ensuring consistent style and formatting.
-   Cleaned up indentation and formatting inconsistencies across the file for better readability.

Example use of new selected window blending functionality:

```elisp
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-use-blend-background t)
  (selected-window-accent-use-blend-alpha 0.2)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color "cyan4")
  (selected-window-accent-mode-style 'default))
```

{{< figure src="/emacs/selected-window-accent-mode-07.jpg" width="100%" >}}

```elisp
(use-package selected-window-accent-mode
  :config (selected-window-accent-mode 1)
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-use-blend-background t)
  (selected-window-accent-use-blend-alpha 0.1)
  (selected-window-accent-tab-accent t)
  (selected-window-accent-custom-color "orange")
  (selected-window-accent-mode-style 'subtle))
```

{{< figure src="/emacs/selected-window-accent-mode-08.jpg" width="100%" >}}
