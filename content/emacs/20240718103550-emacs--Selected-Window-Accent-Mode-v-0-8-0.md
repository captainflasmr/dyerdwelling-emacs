---
title: "Selected Window Accent Mode v0.8.0"
author: ["James Dyer"]
lastmod: 2024-07-19T11:15:00+01:00
tags: ["selected-window-accent-mode", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240718103550-emacs--Selected-Window-Accent-Mode-v-0-8-0.jpg"
---

## Whats New {#whats-new}

What's New in Selected Window Accent Mode v0.8.0 - (selected-window-accent-mode) which is my window accent package in MELPA for Emacs, here is a quick summary before I dive into the new features:

<!--more-->

> The Selected Window Accent Mode is an Emacs package designed to visually distinguish the currently selected window by applying a unique accent color to its fringes, mode line, header line, and margins.

<https://github.com/captainflasmr/selected-window-accent-mode>


## Transient Map for Quick Access {#transient-map-for-quick-access}

In the latest release of Selected Window Accent Mode, version 0.8.0, users of Emacs 28.1 and above can enjoy the convenience of a transient map. This new feature allows you to access a transient menu by simply pressing \`C-c w\`:

```elisp
(global-set-key (kbd "C-c w") 'selected-window-accent-transient)
```

Upon invoking this key binding, the following transient menu will be displayed:

{{< figure src="/emacs/20240718103550-emacs--Selected-Window-Accent-Mode-v-0-8-0.jpg" width="100%" >}}


## Release Highlights {#release-highlights}


### v0.8.0 | 2024-07-15 {#v0-dot-8-dot-0-2024-07-15}

-   ****Transient Menu****: A transient menu has been added for interactive adjustments, simplifying the process of tweaking settings on the fly. Refer to the README for detailed usage instructions.
-   ****Emacs Dependency****: The Emacs dependency has been upgraded to version 28.1.
-   ****Custom Foreground Accent Color****: A new variable, \`selected-window-accent-fg-color\`, has been introduced for setting a custom foreground accent color.
-   ****Enhanced Customization Options****:
    -   \`selected-window-accent-foreground-adjust-factor\`
    -   \`selected-window-accent--use-complementary-color\`
    -   \`selected-window-accent--foreground-invert-state\`
    -   \`selected-window-accent--foreground-offset\`
-   ****Foreground Brightness Functions****:
    -   \`selected-window-accent-flip-foreground-color\`
    -   \`selected-window-accent-increment-foreground-color\`
    -   \`selected-window-accent-decrement-foreground-color\`
    -   \`selected-window-accent-toggle-complementary-color\`
    -   \`selected-window-accent-toggle-tab-accent\`
    -   \`selected-window-accent-toggle-smart-borders\`
-   ****Configuration Export****: A new function, \`selected-window-accent-output-selected-window-accent-settings\`, allows you to output the current settings for easy copying and pasting into your Emacs init file.
-   ****Miscellaneous****: Various improvements and refactoring have been made to enhance the overall functionality and maintainability.


### v0.7.0 | 2024-07-09 {#v0-dot-7-dot-0-2024-07-09}

-   ****Compatibility Fixes****: Addressed issues with other packages and restored modeline height when switching between modes.
-   ****Issue Resolutions****:
    -   ISSUE #4: Resolved compatibility issues with other packages.
    -   ISSUE #3: Fixed problems causing the package to break fringes.

With these updates, Selected Window Accent Mode continues to offer increased customization and improved user experience. Stay tuned for more exciting features in the upcoming versions! 😀
