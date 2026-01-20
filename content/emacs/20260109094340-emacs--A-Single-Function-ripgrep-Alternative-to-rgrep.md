---
title: "A single function ripgrep alternative to rgrep"
author: ["James Dyer"]
lastmod: 2026-01-09T09:43:00+00:00
tags: ["emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20260109094340-emacs--A-Single-Function-ripgrep-Alternative-to-rgrep.jpg"
---

For years, `rgrep` has been the go-to solution for searching codebases in Emacs. It's built-in, reliable, and works everywhere. But it's slow on large projects and uses the aging `find` and `grep` commands.

Packages like `deadgrep` and `rg.el` provide ripgrep integration, and for years I used `deadgrep` and really liked it. But what if you could get ripgrep's speed with just a single function you paste into your config?

<!--more-->

{{< figure src="/emacs/20260109094340-emacs--A-Single-Function-ripgrep-Alternative-to-rgrep.jpg" width="100%" >}}

This post introduces a ~100 line `defun` that replaces rgrep, no packages, no dependencies, just pure Elisp. It's fast, asynchronous, works offline, and mimics rgrep's familiar interface so it can leverage `grep-mode`

So, why not just use rgrep?

I think that rgrep has three main limitations:

Firstly, speed. On a project with 10,000+ files, rgrep can take 15-30 seconds. Ripgrep completes the same search in under a second.

Secondly, file ignoring, rgrep requires manually configuring `grep-find-ignored-directories` or `grep-find-ignored-files`, I had the following typical configuration for rgrep, but it wasn't as flexible as I would like it to be:

```elisp
(eval-after-load 'grep
  '(progn
     (dolist (dir '("nas" ".cache" "cache" "elpa" "chromium" ".local/share" "syncthing" ".mozilla" ".local/lib" "Games"))
       (push dir grep-find-ignored-directories))
     (dolist (file '(".cache" "*cache*" "*.iso" "*.xmp" "*.jpg" "*.mp4"))
       (push file grep-find-ignored-files))
     ))
```

Ripgrep automatically respects an `.ignore` file. Just create an `.ignore` file in your project root and list patterns to exclude, this is just a simple text file, universally applied across all searches and any changes can be easily applied.

Thirdly, modern features. Ripgrep includes smart-case search, better regex support, and automatic binary file detection. Of course, there is a context that can be displayed around the found line, but in order to get ripgrep to work with grep-mode, this is not really doable, and it's not something I need anyway.

---

Here is the complete ripgrep implementation that you can paste directly into your `init.el`:

```elisp
(defun my/grep (search-term &optional directory glob)
  "Run ripgrep (rg) with SEARCH-TERM and optionally DIRECTORY and GLOB.
If ripgrep is unavailable, fall back to Emacs's rgrep command. Highlights SEARCH-TERM in results.
By default, only the SEARCH-TERM needs to be provided. If called with a
universal argument, DIRECTORY and GLOB are prompted for as well."
  (interactive
   (let* ((univ-arg current-prefix-arg)
          (default-search-term
           (cond
            ((use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end)))
            ((thing-at-point 'symbol t))
            ((thing-at-point 'word t))
            (t ""))))
     (list
      (read-string (if (string-empty-p default-search-term)
                       "Search for: "
                     (format "Search for (default `%s`): " default-search-term))
                   nil nil default-search-term)
      (when univ-arg (read-directory-name "Directory: "))
      (when univ-arg (read-string "File pattern (glob, default: ): " nil nil "")))))
  (let* ((directory (expand-file-name (or directory default-directory)))
         (glob (or glob ""))
         (buffer-name "*grep*"))
    (if (executable-find "rg")
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (setq default-directory directory)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n" directory))
              (if (not (string= "" glob))
                  (insert (format "[o] Glob: %s\n\n" glob)))
              (insert "Searching...\n\n"))
            (grep-mode)
            (setq-local my/grep-search-term search-term)
            (setq-local my/grep-directory directory)
            (setq-local my/grep-glob glob))

          (pop-to-buffer buffer)
          (goto-char (point-min))

          (make-process
           :name "ripgrep"
           :buffer buffer
           :command `("rg" "--color=never" "--max-columns=500"
                      "--column" "--line-number" "--no-heading"
                      "--smart-case" "-e" ,search-term
                      "--glob" ,glob ,directory)
           :filter (lambda (proc string)
                     (when (buffer-live-p (process-buffer proc))
                       (with-current-buffer (process-buffer proc)
                         (let ((inhibit-read-only t)
                               (moving (= (point) (process-mark proc))))
                           (setq string (replace-regexp-in-string "[\r\0\x01-\x08\x0B-\x0C\x0E-\x1F]" "" string))
                           ;; Replace full directory path with ./ in the incoming output
                           (setq string (replace-regexp-in-string
                                         (concat "^" (regexp-quote directory))
                                         "./"
                                         string))
                           (save-excursion
                             (goto-char (process-mark proc))
                             (insert string)
                             (set-marker (process-mark proc) (point)))
                           (if moving (goto-char (process-mark proc)))))))
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   ;; Remove "Searching..." line
                   (goto-char (point-min))
                   (while (re-search-forward "Searching\\.\\.\\.\n\n" nil t)
                     (replace-match "" nil t))

                   ;; Clean up the output - replace full paths with ./
                   (goto-char (point-min))
                   (forward-line 3)
                   (let ((start-pos (point)))
                     (while (re-search-forward (concat "^" (regexp-quote directory)) nil t)
                       (replace-match "./" t t))

                     ;; Check if any results were found
                     (goto-char start-pos)
                     (when (= (point) (point-max))
                       (insert "No results found.\n")))

                   (goto-char (point-max))
                   (insert "\nRipgrep finished\n")

                   ;; Highlight search terms using grep's match face
                   (goto-char (point-min))
                   (forward-line 3)
                   (save-excursion
                     (while (re-search-forward (regexp-quote search-term) nil t)
                       (put-text-property (match-beginning 0) (match-end 0)
                                          'face 'match)
                       (put-text-property (match-beginning 0) (match-end 0)
                                          'font-lock-face 'match))))

                 ;; Set up keybindings
                 (local-set-key (kbd "D")
                                (lambda ()
                                  (interactive)
                                  (my/grep my/grep-search-term
                                           (read-directory-name "New search directory: ")
                                           my/grep-glob)))
                 (local-set-key (kbd "S")
                                (lambda ()
                                  (interactive)
                                  (my/grep (read-string "New search term: "
                                                        nil nil my/grep-search-term)
                                           my/grep-directory
                                           my/grep-glob)))
                 (local-set-key (kbd "o")
                                (lambda ()
                                  (interactive)
                                  (my/grep my/grep-search-term
                                           my/grep-directory
                                           (read-string "New glob: "))))
                 (local-set-key (kbd "g")
                                (lambda ()
                                  (interactive)
                                  (my/grep my/grep-search-term my/grep-directory my/grep-glob)))

                 (goto-char (point-min))
                 (message "ripgrep finished."))))
           )
          (message "ripgrep started..."))
      ;; Fallback to rgrep
      (progn
        (setq default-directory directory)
        (message (format "%s : %s : %s" search-term glob directory))
        (rgrep search-term (if (string= "" glob) "*" glob) directory)))))
```

That's it. ~100 lines. No dependencies. No packages to manage! (well except ripgrep of course)

Now that I have complete control over this function, I have added further improvements over rgrep, inspired by `deadgrep`

-   **`S`** - New search term
-   **`D`** - New directory
-   **`o`** - New glob pattern
-   **`g`** - Re-run current search

and a universal argument can be passed through to set these up on the initial grep

I have tried to make the output as similar as possible to rgrep, to be compatible with `grep-mode` and for familiarity, so it will be something like:

```nil
-*- mode: grep; default-directory: "~/project/" -*-

[o] Glob: *.el

./init.el:42:10:(defun my-function ()
./config.el:156:5:  (my-function)
./helpers.el:89:12:;; Helper for my-function

Ripgrep finished
```

and if a glob is applied it will display the glob pattern.

Its perfect for offline environments, and yes, I'm banging on about this again!, no network, no package manager, no dependencies (except ripgrep of course!)
