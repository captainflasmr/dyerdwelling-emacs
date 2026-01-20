---
title: "Emacs Quick Window Pt 4 - Further Tweaks"
author: ["James Dyer"]
lastmod: 2025-01-03T20:20:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241213115239-emacs--Emacs-core-window-jumping-between-two-windows.jpg"
---

Previously, `my/quick-window-jump` was geared mostly toward managing two or more windows. The behaviour was straightforward:

-   If two windows were open, the function would directly jump to the other window.
-   For more than two windows, a key selection interface would allow you to pick a window to switch to.

However, the function didn’t handle a single-window scenario intelligently. A horizontal split from a single window is something I do very frequently, by frequently, I mean at least every minute! This split is currently bound to its own keybinding, but couldn't my `ace-window` defun clone do this? For example, I find that the transition from a single window to a split never happens vertically for me, so why not make `my/quick-window-jump` perform a horizontal split from a single window and then move the point to that window?

In addition, it is very rare for me to vertically split into more than two columns. I could probably drop my current horizontal split keybinding now. If I do require another horizontal split, the default `C-x 3` is already bound to muscle memory.

Lets take this function to the next level of convenience, with a very simple change, simply an extra (length window-list) and the relevant logic.

<!--more-->

Below is the updated implementation of `my/quick-window-jump`:

```elisp
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there is only a single window, split it horizontally.
If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (cond
     ;; If there is only a single window, split it horizontally.
     ((= (length window-list) 1)
      (split-window-horizontally)
      (other-window 1)) ;; Move focus to the new window immediately after splitting.
     ;; If there are only two windows, switch to the other one directly.
     ((= (length window-list) 2)
      (select-window (other-window-for-scrolling)))
     ;; Otherwise, present the key selection interface.
     (t
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
            (select-window selected-window))))))))
```

The updated `my/quick-window-jump` function now adapts to any window configuration dynamically. Here is now an overview on how it now works:

1.  **Single Window? Split It Automatically**
    -   If there’s only one window open, the function splits it horizontally and shifts focus to the new window.

2.  **Two Windows? Still Jump Directly**
    -   The behaviour for two windows remains unchanged. It efficiently switches focus to the other window directly without inefficiencies.

3.  **More Than Two Windows? Key Selection Interface**
    -   When multiple windows are open, the function presents the familiar key selection interface, allowing you to jump exactly where you intend.

Will there be more tweaks?, not sure, but this is the advantage of now creating my own defun for this piece of functionality!

{{< figure src="/emacs/20241213115239-emacs--Emacs-core-window-jumping-between-two-windows.jpg" width="100%" >}}
