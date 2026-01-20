---
title: "Building an Emacs Package from Scratch: Cursor Heatmap Tutorial Part 2"
author: ["James Dyer"]
lastmod: 2025-07-22T13:26:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250722132631-emacs--Building-a-New-Package-From-Scratch---Cursor-Heatmap-Pt2.jpg"
---

In our ongoing quest to construct a new fully functional Emacs package, in Part 1, we built the foundation and have a very basic package for our cursor heatmap package. Now we're ready to tackle the core challenge, actually tracking where the cursor is and detecting when it moves!

So, where is the cursor?, this seems simple, but with Emacs and of course generally software engineering there are several different approaches and concepts to understand and think about before we move on to some implementation.

1.  **Point**: The character position in the buffer (like character 1,847)
2.  **Line and column**: Traditional text coordinates (line 42, column 15)
3.  **Window coordinates**: Position relative to the current window
4.  **Frame coordinates**: Position relative to the entire Emacs frame
5.  **Pixel coordinates**: Exact pixel position on screen

For our heatmap, we want to visualize cursor movement patterns across the entire Emacs interface, so we'll focus on **frame-relative positioning**, both pixel-based (for GUI Emacs) and character-based (for terminal Emacs).


## Exploring Emacs Coordinate Systems {#exploring-emacs-coordinate-systems}

Let's start by understanding what information Emacs can give us about cursor position. Add this exploration function to your package:

```elisp
(defun cursor-heatmap-debug-position ()
  "Show detailed information about current cursor position."
  (interactive)
  (let* ((point-pos (point))
         (line-col (cons (line-number-at-pos) (current-column)))
         (window-edges (window-edges))
         (pixel-pos (when (display-graphic-p)
                      (window-absolute-pixel-position))))
    (message "Debug - Point: %d | Line/Col: %s | Window edges: %s | Pixels: %s"
             point-pos line-col window-edges pixel-pos)))
```

Try this function in different buffers and notice how the values change. You'll see that:

-   **Point** changes based on buffer content and cursor position within text
-   **Line/column** reflects traditional text editing coordinates
-   **Window edges** show the window boundaries in character units
-   **Pixel position** (GUI only) gives exact screen coordinates


## Building Our Position Detection System {#building-our-position-detection-system}

Now let's create our core position detection function. We'll handle both GUI and terminal modes:

```elisp
(defun cursor-heatmap--get-cursor-position ()
  "Get cursor position as frame-relative coordinates.
Returns (x . y) cons cell or nil if position should be excluded."
  (when (and (not (minibufferp))  ; Skip minibuffer
             (get-buffer-window)) ; Only track visible buffers
    (condition-case err
        (if (display-graphic-p)
            (cursor-heatmap--get-pixel-position)
          (cursor-heatmap--get-character-position))
      (error
       ;; If anything goes wrong, return nil silently
       nil))))
```

Notice several important design decisions here:

**Error Handling**: We wrap everything in `condition-case` because cursor position detection can fail in various edge cases (buffer switching, window operations, etc.).

**Exclusion Logic**: We skip the minibuffer because it's typically used for brief interactions, not sustained editing work.

**Display Mode Detection**: We use different strategies for GUI vs. terminal Emacs.


## Implementing Pixel-Based Position Detection {#implementing-pixel-based-position-detection}

For GUI Emacs, we can get precise pixel coordinates:

```elisp
(defun cursor-heatmap--get-pixel-position ()
  "Get cursor position using pixel coordinates in GUI mode."
  (let* ((pixel-pos (window-absolute-pixel-position))
         (frame (selected-frame))
         (frame-pixel-width (frame-pixel-width frame))
         (frame-pixel-height (frame-pixel-height frame)))

    (when (and pixel-pos
               (> frame-pixel-width 0)
               (> frame-pixel-height 0))
      (let* ((frame-x-pixel (car pixel-pos))
             (frame-y-pixel (cdr pixel-pos))
             ;; Normalize to 0.0-1.0 range
             (normalized-x (/ (float frame-x-pixel) frame-pixel-width))
             (normalized-y (/ (float frame-y-pixel) frame-pixel-height)))

        ;; Return as percentages for easier grid mapping later
        (cons normalized-x normalized-y)))))
```

This function:

1.  Gets the absolute pixel position of the cursor
2.  Normalizes it to a 0.0-1.0 range relative to the frame size
3.  Returns coordinates that we can easily map to any grid size


## Implementing Character-Based Position Detection {#implementing-character-based-position-detection}

Terminal Emacs doesn't have pixel coordinates, so we use character positions:

```elisp
(defun cursor-heatmap--get-character-position ()
  "Get cursor position using character coordinates in terminal mode."
  (let* ((window (selected-window))
         (window-edges (window-edges window))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         (point-pos (window-point window))
         (total-frame-width (frame-width))
         (total-frame-height (frame-height)))

    (when (and (> total-frame-width 0)
               (> total-frame-height 0))
      (save-excursion
        (goto-char point-pos)
        (let* ((current-column (current-column))
               (window-line (count-lines (window-start window) point-pos))
               ;; Calculate frame-relative position
               (frame-column (+ window-left current-column))
               (frame-row (+ window-top window-line))
               ;; Normalize to 0.0-1.0 range
               (normalized-x (/ (float frame-column) total-frame-width))
               (normalized-y (/ (float frame-row) total-frame-height)))

          (cons normalized-x normalized-y))))))
```

This is more complex because we need to:

1.  Get the current column position (handling tabs correctly)
2.  Calculate which line we're on relative to the window
3.  Convert window-relative coordinates to frame-relative coordinates
4.  Normalize to the same 0.0-1.0 range as pixel coordinates


## Adding Movement Detection {#adding-movement-detection}

Having a position is only half the battle, we need to detect when the cursor actually moves. Let's add state tracking:

```elisp
(defvar cursor-heatmap--last-position nil
  "Last recorded cursor position for movement detection.")

(defvar cursor-heatmap--movement-count 0
  "Total number of cursor movements detected.")

(defun cursor-heatmap--detect-movement ()
  "Check if cursor has moved and record the movement."
  (let ((current-pos (cursor-heatmap--get-cursor-position)))
    (when current-pos
      (when (and cursor-heatmap--last-position
                 (not (equal current-pos cursor-heatmap--last-position)))
        ;; This is a real movement!
        (cl-incf cursor-heatmap--movement-count)
        (message "Movement detected! Total movements: %d"
                 cursor-heatmap--movement-count))

      ;; Update last position for next comparison
      (setq cursor-heatmap--last-position current-pos))))
```


## Setting Up the Hook System {#setting-up-the-hook-system}

Emacs provides several hooks we can use to detect cursor movement. The most reliable is `post-command-hook`, which runs after every command:

```elisp
(defun cursor-heatmap--setup-tracking ()
  "Set up hooks to track cursor movement."
  (add-hook 'post-command-hook #'cursor-heatmap--detect-movement))

(defun cursor-heatmap--stop-tracking ()
  "Remove movement tracking hooks."
  (remove-hook 'post-command-hook #'cursor-heatmap--detect-movement))
```


## Creating Our First Interactive Commands {#creating-our-first-interactive-commands}

Let's add commands users can run to test our tracking:

```elisp
;;;###autoload
(defun cursor-heatmap-start-tracking ()
  "Start tracking cursor movements."
  (interactive)
  (cursor-heatmap--setup-tracking)
  (setq cursor-heatmap--last-position (cursor-heatmap--get-cursor-position)
        cursor-heatmap--movement-count 0)
  (message "Cursor movement tracking started"))

;;;###autoload
(defun cursor-heatmap-stop-tracking ()
  "Stop tracking cursor movements."
  (interactive)
  (cursor-heatmap--stop-tracking)
  (message "Cursor movement tracking stopped. Total movements: %d"
           cursor-heatmap--movement-count))

;;;###autoload
(defun cursor-heatmap-show-stats ()
  "Show current tracking statistics."
  (interactive)
  (let ((current-pos (cursor-heatmap--get-cursor-position)))
    (message "Position: %s | Movements: %d | Tracking: %s"
             (if current-pos
                 (format "%.3f, %.3f" (car current-pos) (cdr current-pos))
               "unknown")
             cursor-heatmap--movement-count
             (if (memq #'cursor-heatmap--detect-movement post-command-hook)
                 "ON" "OFF"))))
```

The `;;;###autoload` comments are special, they tell Emacs to make these functions available even before the package is fully loaded.


## Handling Edge Cases {#handling-edge-cases}

Real-world usage reveals edge cases we need to handle:

```elisp
(defcustom cursor-heatmap-exclude-minibuffer t
  "Whether to exclude minibuffer from tracking."
  :type 'boolean
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-exclude-special-buffers t
  "Whether to exclude special buffers (starting with space or asterisk)."
  :type 'boolean
  :group 'cursor-heatmap)

(defun cursor-heatmap--should-track-buffer-p ()
  "Return non-nil if current buffer should be tracked."
  (and (not (and cursor-heatmap-exclude-minibuffer
                 (minibufferp)))
       (not (and cursor-heatmap-exclude-special-buffers
                 (string-match "^[ *]" (buffer-name))))
       (get-buffer-window))) ; Only track visible buffers
```

Update our position function to use this check:

```elisp
(defun cursor-heatmap--get-cursor-position ()
  "Get cursor position as frame-relative coordinates.
Returns (x . y) cons cell or nil if position should be excluded."
  (when (cursor-heatmap--should-track-buffer-p)
    (condition-case err
        (if (display-graphic-p)
            (cursor-heatmap--get-pixel-position)
          (cursor-heatmap--get-character-position))
      (error nil))))
```


## Testing Our Movement Detection {#testing-our-movement-detection}

Let's create a comprehensive test function:

```elisp
(defun cursor-heatmap-test-detection ()
  "Interactive test of cursor position detection."
  (interactive)
  (if (memq #'cursor-heatmap--detect-movement post-command-hook)
      (message "Tracking already active. Use cursor-heatmap-stop-tracking to stop.")
    (cursor-heatmap-start-tracking)
    (message "Move your cursor around, then run cursor-heatmap-show-stats")))
```


## Our Complete Progress So Far {#our-complete-progress-so-far}

Here's what we've built in this post:

```elisp
;; Add these to your cursor-heatmap.el file after the customization variables

(require 'cl-lib) ; For cl-incf

;; State variables
(defvar cursor-heatmap--last-position nil
  "Last recorded cursor position for movement detection.")

(defvar cursor-heatmap--movement-count 0
  "Total number of cursor movements detected.")

;; New customization options
(defcustom cursor-heatmap-exclude-minibuffer t
  "Whether to exclude minibuffer from tracking."
  :type 'boolean
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-exclude-special-buffers t
  "Whether to exclude special buffers (starting with space or asterisk)."
  :type 'boolean
  :group 'cursor-heatmap)

;; Core detection functions
(defun cursor-heatmap--should-track-buffer-p ()
  "Return non-nil if current buffer should be tracked."
  (and (not (and cursor-heatmap-exclude-minibuffer
                 (minibufferp)))
       (not (and cursor-heatmap-exclude-special-buffers
                 (string-match "^[ *]" (buffer-name))))
       (get-buffer-window)))

(defun cursor-heatmap--get-pixel-position ()
  "Get cursor position using pixel coordinates in GUI mode."
  (let* ((pixel-pos (window-absolute-pixel-position))
         (frame (selected-frame))
         (frame-pixel-width (frame-pixel-width frame))
         (frame-pixel-height (frame-pixel-height frame)))

    (when (and pixel-pos
               (> frame-pixel-width 0)
               (> frame-pixel-height 0))
      (let* ((frame-x-pixel (car pixel-pos))
             (frame-y-pixel (cdr pixel-pos))
             (normalized-x (/ (float frame-x-pixel) frame-pixel-width))
             (normalized-y (/ (float frame-y-pixel) frame-pixel-height)))
        (cons normalized-x normalized-y)))))

(defun cursor-heatmap--get-character-position ()
  "Get cursor position using character coordinates in terminal mode."
  (let* ((window (selected-window))
         (window-edges (window-edges window))
         (window-left (nth 0 window-edges))
         (window-top (nth 1 window-edges))
         (point-pos (window-point window))
         (total-frame-width (frame-width))
         (total-frame-height (frame-height)))

    (when (and (> total-frame-width 0)
               (> total-frame-height 0))
      (save-excursion
        (goto-char point-pos)
        (let* ((current-column (current-column))
               (window-line (count-lines (window-start window) point-pos))
               (frame-column (+ window-left current-column))
               (frame-row (+ window-top window-line))
               (normalized-x (/ (float frame-column) total-frame-width))
               (normalized-y (/ (float frame-row) total-frame-height)))
          (cons normalized-x normalized-y))))))

(defun cursor-heatmap--get-cursor-position ()
  "Get cursor position as frame-relative coordinates.
Returns (x . y) cons cell or nil if position should be excluded."
  (when (cursor-heatmap--should-track-buffer-p)
    (condition-case err
        (if (display-graphic-p)
            (cursor-heatmap--get-pixel-position)
          (cursor-heatmap--get-character-position))
      (error nil))))

(defun cursor-heatmap--detect-movement ()
  "Check if cursor has moved and record the movement."
  (let ((current-pos (cursor-heatmap--get-cursor-position)))
    (when current-pos
      (when (and cursor-heatmap--last-position
                 (not (equal current-pos cursor-heatmap--last-position)))
        (cl-incf cursor-heatmap--movement-count))
      (setq cursor-heatmap--last-position current-pos))))

(defun cursor-heatmap--setup-tracking ()
  "Set up hooks to track cursor movement."
  (add-hook 'post-command-hook #'cursor-heatmap--detect-movement))

(defun cursor-heatmap--stop-tracking ()
  "Remove movement tracking hooks."
  (remove-hook 'post-command-hook #'cursor-heatmap--detect-movement))

;; Interactive commands
;;;###autoload
(defun cursor-heatmap-start-tracking ()
  "Start tracking cursor movements."
  (interactive)
  (cursor-heatmap--setup-tracking)
  (setq cursor-heatmap--last-position (cursor-heatmap--get-cursor-position)
        cursor-heatmap--movement-count 0)
  (message "Cursor movement tracking started"))

;;;###autoload
(defun cursor-heatmap-stop-tracking ()
  "Stop tracking cursor movements."
  (interactive)
  (cursor-heatmap--stop-tracking)
  (message "Cursor movement tracking stopped. Total movements: %d"
           cursor-heatmap--movement-count))

;;;###autoload
(defun cursor-heatmap-show-stats ()
  "Show current tracking statistics."
  (interactive)
  (let ((current-pos (cursor-heatmap--get-cursor-position)))
    (message "Position: %s | Movements: %d | Tracking: %s"
             (if current-pos
                 (format "%.3f, %.3f" (car current-pos) (cdr current-pos))
               "unknown")
             cursor-heatmap--movement-count
             (if (memq #'cursor-heatmap--detect-movement post-command-hook)
                 "ON" "OFF"))))

;;;###autoload
(defun cursor-heatmap-test-detection ()
  "Interactive test of cursor position detection."
  (interactive)
  (if (memq #'cursor-heatmap--detect-movement post-command-hook)
      (message "Tracking already active. Use cursor-heatmap-stop-tracking to stop.")
    (cursor-heatmap-start-tracking)
    (message "Move your cursor around, then run cursor-heatmap-show-stats")))

;; Debug helper
(defun cursor-heatmap-debug-position ()
  "Show detailed information about current cursor position."
  (interactive)
  (let* ((point-pos (point))
         (line-col (cons (line-number-at-pos) (current-column)))
         (window-edges (window-edges))
         (pixel-pos (when (display-graphic-p)
                      (window-absolute-pixel-position)))
         (our-pos (cursor-heatmap--get-cursor-position)))
    (message "Point: %d | Line/Col: %s | Edges: %s | Pixels: %s | Our pos: %s"
             point-pos line-col window-edges pixel-pos our-pos)))
```


## What We've Accomplished {#what-we-ve-accomplished}

In this post, we've built the core tracking system:

✅ **Position detection**: Works in both GUI and terminal modes
✅ **Movement detection**: Only counts actual cursor movements
✅ **Error handling**: Gracefully handles edge cases
✅ **Hook system**: Reliable tracking using Emacs' event system
✅ **Interactive testing**: Commands to start, stop, and debug tracking
✅ **Customization**: User control over what gets tracked


## Testing Your Implementation {#testing-your-implementation}

Try these exercises to verify everything works:

1.  **Load and test**: Reload your package and run `cursor-heatmap-test-detection`
2.  **Move around**: Navigate between buffers, windows, and frames
3.  **Check stats**: Run `cursor-heatmap-show-stats` to see movement counts
4.  **Debug position**: Use `cursor-heatmap-debug-position` to see raw coordinate data
5.  **Test exclusions**: Try the minibuffer and special buffers


## Common Issues and Solutions {#common-issues-and-solutions}

**"Position always nil"**: Check that you're in a visible buffer and not a special buffer.

**"No movements detected"**: Make sure tracking is active and you're making actual movements, not just staying in one place.

**"Coordinates seem wrong"**: This is normal, we're getting normalized coordinates (0.0-1.0) that we'll map to grids later.


## Looking Ahead {#looking-ahead}

In Part 3, we'll transform these normalized coordinates into a grid system and start building our data structures for the heatmap. We'll cover:

-   Mapping coordinates to grid cells
-   Creating efficient storage for movement data
-   Handling different grid sizes
-   Building the foundation for visualization

The tracking system we built today is the engine that will power our entire heatmap, everything else builds on this foundation.


## Your Turn: Hands-On Exercise {#your-turn-hands-on-exercise}

Extend the tracking system:

1.  **Add buffer tracking**: Modify the system to track which buffer movements occur in
2.  **Add time tracking**: Record timestamps with movements
3.  **Create a movement log**: Store the last 10 movements for debugging
4.  **Experiment with hooks**: Try different hooks beyond `post-command-hook`


## Next Time {#next-time}

In Part 3, we'll build the grid mapping system that transforms our position data into the structure needed for heatmap visualization.
