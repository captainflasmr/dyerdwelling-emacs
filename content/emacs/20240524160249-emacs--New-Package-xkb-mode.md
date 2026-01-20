---
title: "New Package, xkb-mode, to edit X Keyboard Extension Files"
author: ["James Dyer"]
lastmod: 2024-05-24T16:12:00+01:00
tags: ["sticky-keys", "package", "melpa", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240524160249-emacs--New-Package-xkb-mode.jpg"
---

Based on my recent forays into ergonomic key-mapping in Emacs for SwayWM, and effectively for any Wayland compositor or X11-based system using the X Keyboard Extension (XKB) standard, I realized that there doesn't seem to be an Emacs mode to edit these keyboard configuration files (\*.xkb). This is surprising, considering they have been around for more than 30 years!

<!--more-->

So I wrote one, and its on MELPA.

---

<div class="ox-hugo-toc toc local">

- [Summary](#summary)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Customization](#customization)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)
- [ISSUES](#issues)
- [TODOs / ROADMAP](#todos-roadmap)
- [Testing](#testing)

</div>
<!--endtoc-->

---


## Summary {#summary}

A Major mode for editing X Keyboard Extension (XKB) files

xkb-mode is an Emacs major mode designed to facilitate the editing of XKB files, providing syntax highlighting and other useful editing features tailored specifically for XKB file format. Whether you're customizing keyboard layouts or diving into the details of X Keyboard Extension configurations, xkb-mode aims to make the task more efficient and enjoyable.


### Whats New {#whats-new}

Version 0.2.0

-   Correct version from 0.6.0 to 0.2.0 to reflect the actual stage of
    development.
-   Remove the 'Alternatives' section, streamlining the introduction.
-   Improve code formatting for better readability and maintenance. This
    includes more consistent use of newlines and arrangement of font lock
    faces.
-   Use \`#'\` prefix for function symbols as a best practice, explicitly
    marking \`xkb-indent-line\` as a function, enhancing code clarity.


### Screenshot {#screenshot}

{{< figure src="/emacs/20240524160249-emacs--New-Package-xkb-mode.jpg" width="100%" >}}


## Features {#features}

-   Syntax highlighting for XKB-specific keywords, modifiers, and structures.
-   Custom indentation logic for XKB code blocks.
-   Auto-detection of .xkb files to automatically enable the mode.


## Installation {#installation}

To install xkb-mode, you can use the following methods:


### use-package (MELPA) {#use-package--melpa}

```elisp
(use-package xkb-mode)
```


### use-package (emacs 29) {#use-package--emacs-29}

Put the following into your emacs init file:

```elisp
(use-package xkb-mode
  :vc (:fetcher github :repo "captainflasmr/xkb-mode"))
```


### from source {#from-source}

Download the \`.el\` file and place it in your Emacs \`load-path\`.

Then either manually load it or add it to your configuration to be loaded at startup.

```elisp
(require 'xkb-mode)
```


## Usage {#usage}

Opening any .xkb file with Emacs should automatically enable xkb-mode, providing you with syntax highlighting and indentation support for editing XKB files.


## Customization {#customization}

Currently, xkb-mode provides a basic set of features optimized for general usage. Future versions may include customizable options based on user feedback.


## Contributing {#contributing}

Contributions to xkb-mode are welcome! Whether it's bug reports, feature suggestions, or code contributions, feel free to reach out or submit pull requests on GitHub.


## License {#license}

xkb-mode is available under the terms of the GNU General Public License v3.0. See the included LICENSE file for more details.


## Contact {#contact}

For any questions or suggestions, please contact James Dyer at captainflasmr@gmail.com.

Visit our GitHub repository: <https://github.com/captainflasmr/xkb-mode> for more information and updates.

This README provides a concise but comprehensive overview of what the \`xkb-mode\` package is, how to get it installed, and how to use it, alongside encouraging community contributions and providing licensing info.


## ISSUES {#issues}

NONE


## TODOs / ROADMAP {#todos-roadmap}

TODO function to set xkb file to latched sticky keys

TODO function to set xkb file locked sticky keys

TODO function to map RAlt to Ctrl


## Testing {#testing}

See CHANGELOG.org
