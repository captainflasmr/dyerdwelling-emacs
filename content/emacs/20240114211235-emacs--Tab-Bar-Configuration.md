---
title: "Tab-Bar-Configuration"
author: ["James Dyer"]
lastmod: 2024-01-14T21:12:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240114211235-emacs--Tab-Bar-Configuration.jpg"
---

Configuring Emacs for Enhanced Tab Management

<!--more-->

---

<div class="ox-hugo-toc toc local">

- [Prerequisites](#prerequisites)
- [Configuration Setup](#configuration-setup)

</div>
<!--endtoc-->

---

Emacs, the extensible editor known for its configurability, often finds users looking to mimic behavior found in modern IDEs or browsers, especially when it comes to managing multiple buffers through a tabbed interface. Today, we're going to walk through a configuration setup for \`tab-bar-mode\`, a built-in feature introduced in Emacs 27 that allows users to work with tabs similar to how they might in web browsers.

```elisp
;;
;; -> tab-bar
;;
(use-package tab-bar
  :ensure nil ;; Since tab-bar is built-in, no package needs to be downloaded
  :init
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  :custom
  (tab-bar-format (remove 'tab-bar-format-history tab-bar-format))
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-history-limit 100)
  (tab-bar-auto-width-max '(100 20))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'my-tab-bar-tab-name-format)
  :config
  (defun my-tab-bar-tab-name-format (tab i)
    (propertize
      (format " %d " i)
      'face (funcall tab-bar-tab-face-function tab)))
  :bind
  (("M-H" . tab-bar-switch-to-prev-tab)
    ("M-L" . tab-bar-switch-to-next-tab)
    ("M-u" . tab-bar-history-back)
    ("M-i" . tab-bar-history-forward))
  :custom-face
  (tab-bar-tab ((t (:inherit tab-bar :box (:line-width (2 . 2) :color "#9c9c9c" :style flat)))))
  (tab-bar-tab-inactive ((t (:inherit tab-bar :box (:line-width (2 . 2) :color "#575757" :style flat))))))
```


## Prerequisites {#prerequisites}

Before proceeding with the following configuration, ensure that you're using Emacs 27 or higher since \`tab-bar-mode\` is not available in earlier versions.


## Configuration Setup {#configuration-setup}

The configuration below uses \`use-package\`, a declarative Emacs package manager that simplifies package setup. If \`use-package\` is not installed on your Emacs, make sure to do so before using this configuration.

Let's go through the configuration snippet by snippet:

```elisp
(use-package tab-bar
  :ensure nil ;; Since tab-bar is built-in, no package needs to be downloaded
```

Here, \`use-package\` is called with \`tab-bar\`, noting that this is built-in functionality (\`:ensure nil\`) and does not require downloading an additional package.

```elisp
:init
(tab-bar-mode 1)
(tab-bar-history-mode 1)
```

\`:init\` marks the section that is executed before the package is loaded. Enabling \`tab-bar-mode\` activates tabs within Emacs, while \`tab-bar-history-mode\` provides navigation through a history of the active tabs.

```elisp
:custom
(tab-bar-format (remove 'tab-bar-format-history tab-bar-format))
(tab-bar-new-button-show nil)
(tab-bar-close-button-show nil)
(tab-bar-history-limit 100)
(tab-bar-auto-width-max '(100 20))
(tab-bar-tab-hints t)
(tab-bar-tab-name-format-function #'my-tab-bar-tab-name-format)
```

In the \`:custom\` keyword, various settings for \`tab-bar-mode\` are defined:

-   \`tab-bar-format\` is customized to not show the history format.
-   The new and close tab buttons are hidden.
-   Sets a history limit to 100 tabs.
-   \`tab-bar-auto-width-max\` controls the maximum width of a tab.
-   Enabling \`tab-bar-tab-hints\` shows the index number of tabs.
-   Specifies a custom naming function for tabs.
    ```elisp
    :config
    (defun my-tab-bar-tab-name-format (tab i)
      (propertize
        (format " %d " i)
        'face (funcall tab-bar-tab-face-function tab)))
    ```

Under \`:config\`, the snippet defines a custom function \`my-tab-bar-tab-name-format\` that formats the tab name to simply be the tab index number, enhancing visibility with specific face settings.

```elisp
:bind
(("M-H" . tab-bar-switch-to-prev-tab)
  ("M-L" . tab-bar-switch-to-next-tab)
  ("M-u" . tab-bar-history-back)
  ("M-i" . tab-bar-history-forward))
```

Keybindings are set up under \`:bind\` for navigating the tab bar, allowing easy switching between tabs and traversing the tab history using \`alt\` combined with the appropriate letter.

```elisp
:custom-face
(tab-bar-tab ((t (:inherit tab-bar :box (:line-width (2 . 2) :color "#9c9c9c" :style flat)))))
(tab-bar-tab-inactive ((t (:inherit tab-bar :box (:line-width (2 . 2) :color "#575757" :style flat))))))
```

Finally, the \`:custom-face\` directive changes the appearance of active and inactive tabs, giving them a more distinct and flat style with a colored border.
