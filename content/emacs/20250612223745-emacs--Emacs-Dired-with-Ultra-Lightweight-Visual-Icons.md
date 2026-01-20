---
title: "Emacs dired with Ultra-Lightweight Visual Icons"
author: ["James Dyer"]
lastmod: 2025-06-12T22:37:00+01:00
tags: ["emacs", 2025, "dired"]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250612223745-emacs--Emacs-Dired-with-Ultra-Lightweight-Visual-Icons.jpg"
---

If you spend any time browsing files in Emacs, you've probably wished for a bit more visual distinction in dired buffers?. While packages like `all-the-icons-dired` provide beautiful icon sets, they come with dependencies, font requirements, and potential compatibility issues. What if you could get meaningful visual file type indicators with just a few simple lines of pure Elisp? I have adapted the original idea provided by `Emacs-solo` and focused more on the earlier Unicode characters that are most likely to always be present in an Emacs environment.

Popular dired icon packages often require:

-   Installing specific icon fonts
-   Managing font fallbacks across different systems
-   Dealing with alignment issues in terminal Emacs
-   Large dependency chains that slow down startup
-   Compatibility headaches when sharing configs

I have also noticed that Emacs can just crash if it is dealing with an icon that isn't installed on the system.

Here's an implementation that adds visual file type indicators using only standard Unicode characters. Of course the icons-map isn't exhaustive but might be a sensible minimal starting point.

```elisp
(defvar dired-icons-map
  '(("el" . "λ") ("rb" . "◆") ("js" . "○") ("ts" . "●") ("json" . "◎") ("md" . "■")
    ("txt" . "□") ("html" . "▲") ("css" . "▼") ("png" . "◉") ("jpg" . "◉")
    ("pdf" . "▣") ("zip" . "▢") ("py" . "∆") ("c" . "◇") ("sql" . "▦")
    ("mp3" . "♪") ("mp4" . "▶") ("exe" . "▪")))

(defun dired-add-icons ()
  (when (derived-mode-p 'dired-mode)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (and (not (eobp)) (< (line-number-at-pos) 200))
          (condition-case nil
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (and (> (length line) 10)
                           (string-match "\\([rwxd-]\\{10\\}\\)" line)
                           (dired-move-to-filename t)
                           (not (looking-at "[▶◦λ◆○●◎■□▲▼◉▣▢◇∆▦♪▪] ")))
                  (let* ((is-dir (eq (aref line (match-beginning 1)) ?d))
                         (filename (and (string-match "\\([^ ]+\\)$" line) (match-string 1 line)))
                         (icon (cond (is-dir "▶")
                                    ((and filename (string-match "\\.\\([^.]+\\)$" filename))
                                     (or (cdr (assoc (downcase (match-string 1 filename)) dired-icons-map)) "◦"))
                                    (t "◦"))))
                    (insert icon " "))))
            (error nil))
          (forward-line))))))

(add-hook 'dired-after-readin-hook 'dired-add-icons)
```

The icons use only standard Unicode geometric shapes and mathematical symbols that have been supported in every font since the 1990s. No special fonts required, it works identically in:

-   Terminal Emacs
-   GUI Emacs on any platform
-   Ancient and modern Emacs versions

There are some advantages to be had:

-   **Zero dependencies**: Pure Elisp, no external packages
-   **Tiny footprint**: 30 lines vs hundreds in full icon packages
-   **Instant startup**: No font loading or icon caching
-   **Robust error handling**: Gracefully skips problematic files
-   **Performance limits**: Processes max 200 files to prevent freezing but of course that can be modified to taste

Once added to your config, it just works. No font updates, no package maintenance, no breaking changes from upstream dependencies.

Here is a typical example:

After:

{{< figure src="/emacs/20250612223745-emacs--Emacs-Dired-with-Ultra-Lightweight-Visual-Icons.jpg" width="100%" >}}

Before:

{{< figure src="/emacs/20250612223745-emacs--Emacs-Dired-with-Ultra-Lightweight-Visual-Icons2.jpg" width="100%" >}}

Want different icons? Just modify the `dired-icons-map`:

```elisp
;; Prefer asterisks for code files?
("js" . "*") ("py" . "*") ("rb" . "*")

;; Like filled shapes for documents?
("md" . "▪") ("txt" . "▪") ("pdf" . "▪")

;; Add your own file types
("log" . "□") ("cfg" . "○") ("bak" . "▫")
```

Simply copy the code into your `init.el` and off you go!
