---
title: "Copying completion candidate to the clipboard"
author: ["James Dyer"]
lastmod: 2025-01-30T07:40:00+00:00
tags: ["embark", "emacs", "elisp", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250130074049-emacs--Copy-icomplete-candidate-to-clipboard.jpg"
---

{{< figure src="/emacs/20250130074049-emacs--Copy-icomplete-candidate-to-clipboard.jpg" width="100%" >}}

In my continuing quest to replace all my external use packages with my own elisp versions so I can still follow my current workflow on an air-gapped system, I would like to replace a single function I use often from `embark`

`embark` allows an action in a certain context, and there are a lot of actions to choose from, but I found I was generally using very few, so to remove my reliance on `embark` I think I can implement these actions myself.

<!--more-->

The main one is to copy the current completion candidate.  For example, I might be searching for a function name, so `describe-function` function for example, ha!, I just did it!, using `describe-function` to select `describe-function` and then copying to the clipboard to then paste in to this blog article!

Well that is it, so lets code it up:

```elisp
(defun my-icomplete-copy-candidate ()
  "Copy the current Icomplete candidate to the kill ring."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when candidate
      (kill-new (substring-no-properties candidate))
      (let ((copied-text candidate))
        (run-with-timer 0 nil (lambda ()
          (message "Copied: %s" copied-text)))
        (abort-recursive-edit)))))

(global-set-key (kbd "C-c ,") 'find-file-at-point)
(define-key minibuffer-local-completion-map (kbd "C-c ,") 'my-icomplete-copy-candidate)
```

Note of course that I use the built-in `fido-mode` and therefore `icomplete` so there might be possibly a different implementation if you were using something like vertico for example.

Also as a bonus, note that I have added another common `embark` action and that is to navigate to a file at point. Fortunately Emacs has this function built-in so I again bound to my former standard `embark` activation key.
