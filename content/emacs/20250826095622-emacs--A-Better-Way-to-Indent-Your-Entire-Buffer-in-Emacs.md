---
title: "A Better Way to Indent Your Entire Buffer in Emacs?"
author: ["James Dyer"]
lastmod: 2025-08-26T09:56:00+01:00
tags: ["emacs", "elisp", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250826095622-emacs--A-Better-Way-to-Indent-Your-Entire-Buffer-in-Emacs.jpg"
---

As an Emacs user, you've probably found yourself wanting to clean up the indentation of an entire file. The standard approach is to select all (`C-x h`) and then run `indent-region` (I think, correct me if I am wrong!), but this has an annoying side effect: it destroys your current mark position, which might have been carefully set for other operations.

<!--more-->

{{< figure src="/emacs/20250826095622-emacs--A-Better-Way-to-Indent-Your-Entire-Buffer-in-Emacs.jpg" width="100%" >}}

Also, and just whisper it and don't tell anyone, but I have been using VSCode a little (only for work, of course!) and an indent by default seems to indent the whole file, which initially I thought would be really annoying, but actually it feels quite natural, as always at some point I would like to indent my code.

This is Emacs however and there is a simple solution and I've added it to my Emacs configuration:

So, just a recap on some typical workflow, it goes a little something like this:

1.  You're working in a file with inconsistent indentation
2.  You want to fix the entire buffer's formatting
3.  You run `C-x h` (select all) followed by `M-x indent-region`
4.  Your mark is now at the beginning of the buffer, disrupting your workflow

This is particularly frustrating when you've set a mark for a specific editing task and want to preserve that position.

```elisp
(defun indent-whole-buffer ()
  "Indent the entire buffer without affecting point or mark."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c i") 'indent-whole-buffer)
```

The function indents from `(point-min)` to `(point-max)`, covering the entire buffer, then restores everything to exactly how it was before, except now with proper indentation.

After adding this to your configuration, simply press `C-c i` (or whatever keybinding you prefer) to indent your entire buffer. Your cursor stays put, your mark remains set, and your file gets beautifully formatted!
