---
title: "Speed Reading in Emacs: Building an RSVP Reader"
author: ["James Dyer"]
lastmod: 2026-01-18T10:30:00+00:00
tags: ["emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20260116182841-emacs--Speed-Reading-in-Emacs:-Building-an-RSVP-Reader.jpg"
---

I recently came across a fascinating video titled "How Fast Can You Read? - Speed Reading Challenge" that demonstrated the power of RSVP (Rapid Serial Visual Presentation) for speed reading. The concept  is quite nice and simple and I vaguely remember seeing something about it a few years back.  Instead of your eyes scanning across lines of text, words are presented one at a time in a fixed position. This eliminates the mechanical overhead of eye movements and can dramatically increase reading speed!

<!--more-->

{{< figure src="/emacs/20260116182841-emacs--Speed-Reading-in-Emacs:-Building-an-RSVP-Reader.jpg" width="100%" >}}

So, I immediately wondered, could I build this into Emacs?, actually no, firstly I thought, are there any packages for Emacs that can do this?, of course there are!, the **spray** package from MELPA is a more mature, feature-rich option if you're looking for production-ready RSVP reading in Emacs, and also there is **speedread**.  However, there's something satisfying about having a compact, single-function solution that does exactly what you need, so lets see if I can build one!

RSVP works by displaying words sequentially in the same location on screen. Your eyes remain stationary, focused on a single point, while words flash by at a controlled pace. This technique can boost reading speeds to 300-600+ words per minute, compared to typical reading speeds of 200-300 WPM.

The key innovation is the **Optimal Recognition Point (ORP)** - typically positioned about one-third into each word. This is where your eye naturally fixates when reading. By aligning each word's ORP at the same screen position, RSVP creates an optimal visual flow.

Given Emacs' extensive text processing capabilities, this sounds something that Emacs could eat for breakfast. Here is what I came up with:

Here is a quick video of my implementation:

{{< figure src="/emacs/20260118100321--screen-recording.gif" width="100%" >}}

and the defun:

```elisp
(defun rsvp-minibuffer ()
  "Display words from point (or mark to point) in minibuffer using RSVP.
Use f/s for speed, [/] for size, b/n to skip, SPC to pause, q to quit."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning) (point)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties start end))
         (wpm 350) (font-size 200) (orp-column 20)
         (word-positions '()) (pos 0) (i 0)
         (message-log-max nil))  ; Disable message logging
    ;; Build word positions list
    (dolist (word (split-string text))
      (unless (string-blank-p word)
        (when-let ((word-start (string-match (regexp-quote word) text pos)))
          (push (cons word (+ start word-start)) word-positions)
          (setq pos (+ word-start (length word))))))
    (setq word-positions (nreverse word-positions))
    ;; Display loop
    (while (< i (length word-positions))
      (let* ((word (car (nth i word-positions)))
             (word-pos (cdr (nth i word-positions)))
             (word-len (length word))
             (delay (* (/ 60.0 wpm)
                      (cond ((< word-len 3) 0.8) ((> word-len 8) 1.3) (t 1.0))
                      (if (string-match-p "[.!?]$" word) 1.5 1.0)))
             (orp-pos (/ word-len 3))
             (face-mono `(:height ,font-size :family "monospace"))
             (face-orp `(:foreground "red" :weight normal ,@face-mono))
             (padded-word (concat
                          (propertize (make-string (max 0 (- orp-column orp-pos)) ?\s) 'face face-mono)
                          (propertize (substring word 0 orp-pos) 'face face-mono)
                          (propertize (substring word orp-pos (1+ orp-pos)) 'face face-orp)
                          (propertize (substring word (1+ orp-pos)) 'face face-mono))))
        (goto-char (+ word-pos word-len))
        (message "%s" padded-word)
        (pcase (read-event nil nil delay)
          (?f (setq wpm (min 1000 (+ wpm 50))))
          (?s (setq wpm (max 50 (- wpm 50))))
          (?\[ (setq font-size (max 100 (- font-size 20))))
          (?\] (setq font-size (min 400 (+ font-size 20))))
          (?b (setq i (max 0 (- i 10))))
          (?n (setq i (min (1- (length word-positions)) (+ i 10))))
          (?\s (read-event (format "%s [PAUSED - WPM: %d]" padded-word wpm)))
          (?q (setq i (length word-positions)))
          (_ (setq i (1+ i))))))))
```

The function calculates the ORP as one-third through each word and highlights it in red. By padding each word with spaces, the ORP character stays perfectly aligned in the same column, creating that crucial stationary focal point.

To ensure pixel-perfect alignment, the function explicitly sets a monospace font family for all displayed text. Without this, proportional fonts would cause the ORP to drift slightly between words, although I think at times there is a little waddle, but it is good enough.

Also, Not all words are created equal:

-   Short words (&lt; 3 characters) display 20% faster
-   Long words (&gt; 8 characters) display 30% slower
-   Words ending in punctuation (`.!?`) get 50% more time

This mimics natural reading rhythms where you'd naturally pause at sentence boundaries.

While reading, you can try these keybindings: (which I borrowed off **spray**)

-   `f` / `s` - Speed up or slow down (±50 WPM)
-   `[` / `]` - Decrease or increase font size
-   `b` / `n` - Skip backward or forward by 10 words
-   `SPC` - Pause (press any key to resume)
-   `q` - Quit
-   `C-g` - Emergency quit

Also The function tracks each word's position in the original buffer and updates `point` as you read. This means:

-   You can see where you are in the text
-   When you quit, your cursor is at the last word you read
-   You can resume reading by running the function again

To use it, simply:

1.  Position your cursor where you want to start reading (or select a region)
2.  Run `M-x rsvp-minibuffer`
3.  Watch the words flow in the minibuffer

The function works from point to end of buffer, or if you have an active region, it only processes the selected text.

If you're curious about RSVP reading, drop this function into your Emacs config and give it a try. Start at 300-350 WPM and see how it feels. You might be surprised at how much faster you can consume text when your eyes aren't constantly moving across the page.

The code is simple enough to customize - adjust the default WPM, change the ORP colour, modify the timing multipliers, or add new controls. That's the beauty of Emacs, if you can imagine it, you can build it.
