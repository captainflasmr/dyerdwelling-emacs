---
title: "Corfu-Eglot-Setup-Guide"
author: ["James Dyer"]
lastmod: 2024-08-28T20:16:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240828201640-emacs--Corfu-Eglot-Setup-Guide.jpg"
---

To set up Corfu (Completion Overlay Region FUnction) to work with Eglot
in Emacs, you can follow these steps:

<!--more-->

1.  **Configure Corfu for Eglot**: Corfu should work with Eglot by default
    when `completion-at-point` is invoked. If you want to ensure Corfu is
    used for Eglot's completions, add the following to your config:
    ```emacs-lisp
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                            (list (cape-capf-super #'eglot-completion-at-point)))))
    ```

2.  **Optional: Enable additional configurations**: If you want to add more
    sources for completion or tweak the behaviour further, you can
    explore and use the `cape` package, which adds extra capabilities to
    completion-at-point:
    ```emacs-lisp
    (use-package cape
      :defer t
      :init
      ;; Add more completion sources, like file, dabbrev, etc.
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      )
    ```

Now, when you're working in a programming mode with Eglot, Corfu should
provide an enhanced completion interface.
