---
title: "Emacs Quick Window Pt 3 - jumping between two windows"
author: ["James Dyer"]
lastmod: 2024-12-13T20:15:00+00:00
tags: ["emacs-core", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241213115239-emacs--Emacs-core-window-jumping-between-two-windows.jpg"
---

The original implementation of `my/quick-window-jump` worked perfectly for multi-window setups. It enabled you to assign character labels to each window, display them as overlays within the windows themselves, and jump to your desired window by typing its corresponding key. However, for setups with just two windows (a very common scenario in Emacs), this process felt unnecessarily complicated. Why go through the entire label-assignment process when a single key press could suffice?

<!--more-->

{{< figure src="/emacs/20241213115239-emacs--Emacs-core-window-jumping-between-two-windows.jpg" width="100%" >}}

This latest tweak to `my/quick-window-jump` introduces a simple improvement and in fact is currently what `ace-window` does: **direct switching for two windows**. If there are exactly two windows open, the function now skips the overhead of key prompts and overlay creation. Instead, it switches directly to the other window, this seems more logical to me, why the extra key when it is obvious where the point is going to jump to?

Here’s the core logic for handling two windows:

```elisp
(let* ((window-list (window-list nil 'no-mini)))
  (if (= (length window-list) 2)
      ;; If there are only two windows, switch to the other directly.
      (select-window (other-window-for-scrolling))
    ;; Otherwise, show the key selection interface.
    ...))
```

and here is the updated function:

```elisp
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
  If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (= (length window-list) 2)
        ;; If there are only two windows, switch to the other one directly.
        (select-window (other-window-for-scrolling))
      ;; Otherwise, show the key selection interface.
      (let* ((my/quick-window-overlays nil)
             (sorted-windows (sort window-list
                                   (lambda (w1 w2)
                                     (let ((edges1 (window-edges w1))
                                           (edges2 (window-edges w2)))
                                       (or (< (car edges1) (car edges2))
                                           (and (= (car edges1) (car edges2))
                                                (< (cadr edges1) (cadr edges2))))))))
             (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                    (length sorted-windows)))
             (window-map (cl-pairlis window-keys sorted-windows)))
        (setq my/quick-window-overlays
              (mapcar (lambda (entry)
                        (let* ((key (car entry))
                               (window (cdr entry))
                               (start (window-start window))
                               (overlay (make-overlay start start (window-buffer window))))
                          (overlay-put overlay 'after-string
                                       (propertize (format "[%s]" key)
                                                   'face '(:foreground "white" :background "blue" :weight bold)))
                          (overlay-put overlay 'window window)
                          overlay))
                      window-map))
        (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
          (mapc #'delete-overlay my/quick-window-overlays)
          (setq my/quick-window-overlays nil)
          (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)))))))
```
