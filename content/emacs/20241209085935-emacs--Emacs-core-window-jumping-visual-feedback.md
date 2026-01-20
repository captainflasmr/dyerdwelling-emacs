---
title: "Emacs Core Window Jumping With Visual Feedback"
author: ["James Dyer"]
lastmod: 2024-12-10T14:15:00+00:00
tags: ["emacs-core", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241209085935-emacs--Emacs-core-window-jumping-visual-feedback.jpg"
---

Already I think I can improve `my/quick-window-jump`: which was a window jumping mechanism I created in my previous post which uses a unique key identifier for window navigation just like `ace-window` but condensed into a minimal elisp defun implementation.

<!--more-->

While the original implementation worked, I recently revisited and refined it to improve its modularity, visual feedback, and overall usability.

The main headline of the improvement is porting the `ace-window` window label identifier navigation mechanism using overlays!, see below:

{{< figure src="/emacs/20241209085935-emacs--Emacs-core-window-jumping-visual-feedback.jpg" width="100%" >}}

In this post, I’ll detail the evolution of `my/quick-window-jump`, highlighting the changes made to simplify the code, provide immediate visual indicators, and improve the end-user experience.

---


## The Original Implementation {#the-original-implementation}


### Original Code {#original-code}

```emacs-lisp
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
Windows are labeled starting from the top-left window and proceed top to bottom left to right."
  (interactive)
  (let* ((window-list (my/get-windows)) ; Get sorted list of windows
         (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f") ; Assign key labels
                                (length window-list)))
         (window-map (cl-pairlis window-keys window-list)) ; Create key-to-window map
         (key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
    (if-let ((selected-window (cdr (assoc (char-to-string key) window-map)))) ; Jump to selected window
        (select-window selected-window)
      (message "No window assigned to key: %c" key))))

(defun my/get-windows ()
  "Return a list of windows in the current frame, ordered from top to bottom, left to right."
  (sort (window-list nil 'no-mini)
        (lambda (w1 w2)
          (let ((edges1 (window-edges w1))
                (edges2 (window-edges w2)))
            (or (< (car edges1) (car edges2)) ; Compare top edges
                (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                     (< (cadr edges1) (cadr edges2))))))))
```

While functional, several downsides existed in this implementation:

-   **Separation of Logic:** The `my/get-windows` function added an extra layer of abstraction that wasn’t strictly necessary, as sorting logic could be directly embedded into the main function.
-   **Lack of Visual Feedback:** The function provided no immediate indicator of which windows corresponded to which keys. Users had to guess or manually map the output of the key labels to the positions in their frame layout.

---


## The New and Improved Implementation {#the-new-and-improved-implementation}

The updated version of `my/quick-window-jump` refines the original function by:

1.  **Inlining Window Sorting Logic:** Sorting logic is now part of the main function rather than relying on a separate helper, simplifying maintenance and reducing cognitive overhead.

2.  **Adding Overlay Labels:** Temporary overlays are added to each window, displaying the assigned key visually within the actual window. This makes it easier for users to identify which key corresponds to which window without having to manually figure it out.

Here’s the updated code:


### Improved Code {#improved-code}

```emacs-lisp
(defun my/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
  (interactive)
  (let* ((my/quick-window-overlays nil) ; Temporary list for overlays
         ;; Sort windows by position (top-to-bottom, left-to-right)
         (window-list (sort (window-list nil 'no-mini)
                            (lambda (w1 w2)
                              (let ((edges1 (window-edges w1))
                                    (edges2 (window-edges w2)))
                                (or (< (car edges1) (car edges2))
                                    (and (= (car edges1) (car edges2))
                                         (< (cadr edges1) (cadr edges2))))))))
         ;; Assign key labels to windows
         (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                (length window-list)))
         (window-map (cl-pairlis window-keys window-list))) ; Create map of keys to windows
    ;; Add overlays to display key labels in each window
    (setq my/quick-window-overlays
          (mapcar (lambda (entry)
                    (let* ((key (car entry))
                           (window (cdr entry))
                           (start (window-start window)) ; Start position of window
                           (overlay (make-overlay start start (window-buffer window)))) ; Create overlay
                      (overlay-put overlay 'after-string  ; Add a visual label
                                   (propertize (format "[%s]" key)
                                               'face '(:foreground "white"
                                                                   :background "blue"
                                                                   :weight bold)))
                      (overlay-put overlay 'window window) ; Associate overlay with window
                      overlay))
                  window-map))
    ;; Read key input from user
    (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
      ;; Clear overlays and reset
      (mapc #'delete-overlay my/quick-window-overlays)
      (setq my/quick-window-overlays nil)
      ;; Select window based on key or show error
      (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
        (select-window selected-window)))))
```

---


## Key Improvements {#key-improvements}

1.  **Eliminating Redundancy**
    By folding the `my/get-windows` functionality into `my/quick-window-jump`, the updated function is now self-contained. This reduces dependency on external helpers and makes the logic easier to follow.

2.  **Enhancing User Experience with Visual Overlays**
    The use of temporary overlays provides immediate, intuitive feedback by displaying key mappings directly inside the appropriate windows. This reduces guesswork and makes navigation significantly faster.

    Each window is temporarily labeled with a key, using the following visual attributes:

    -   **Foreground:** White for contrast.
    -   **Background:** Blue for prominence.
    -   **Bold Font:** For easy readability.

---


## Why These Changes Matter {#why-these-changes-matter}

Well I guess they don't really, it's just for me and its just for fun! 😀

1.  **Reduced Mental Burden:** The new approach lets users navigate windows without having to remember or deduce key-to-window mappings. Visual feedback ensures immediate understanding of the layout.

2.  **Improved Modularity:** By embedding all functionality into a single function and handling cleanup directly, the updated implementation is more modular and self-contained.

3.  **Enhanced Readability and Maintainability:** Fewer moving parts and a more streamlined design make the function easier to maintain or extend in the future.
