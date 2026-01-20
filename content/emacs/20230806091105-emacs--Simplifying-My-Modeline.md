---
title: "Simplifying My Modeline"
author: ["James Dyer"]
lastmod: 2023-08-06T09:22:00+01:00
tags: ["emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230806091105-emacs--Simplifying-My-Modeline.jpg"
---

After watching the latest informative video from Protesilaos Stavrou (Prot):

<!--more-->

{{< figure src="/emacs/20230806091105-emacs--Simplifying-My-Modeline.jpg" class="emacs-img" >}}

[Emacs: write custom mode line](https://www.youtube.com/watch?v=Qf_DLPIA9Cs&t=4s)

I decided to try and create my own simple mode line incorporating the features I regularly glance at throughout a day.  Having `mu4e` and `magit` details on my mode line plus other mysterious characters seems a bit much.

After adapting the examples given in the video I created the following:

```elisp
(setq-default mode-line-format
              '("%e"
                " %o "
                "%* "
                my-modeline-buffer-name
                my-modeline-major-mode))

(defvar-local my-modeline-buffer-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (format " %s " (buffer-name))
                    'face '(t :background "#3355bb" :foreground "white" :inherit bold))))
  "Mode line construct to display the buffer name.")

(put 'my-modeline-buffer-name 'risky-local-variable t)

(defvar-local my-modeline-major-mode
    '(:eval
      (list
       (propertize "λ" 'face 'shadow)
       " "
       (propertize (capitalize (symbol-name major-mode)) 'face 'bold)))
  "Mode line construct to display the major mode.")

(put 'my-modeline-major-mode 'risky-local-variable t)
```

I left in **`%e`** as suggested to show an error message if memory was full.

**`%o`** for how far through the buffer I am.  I have my scroll bars disabled but always find it useful to see where I am in a file especially when coding or within a large org file (for example when applying some repetitive macros seeing 80% through the buffer is quite motivating!)

**`%*`** to indicate if a file has been modified using the asterisk convention which is common in many other programs and has the side-effect of removing all other cryptic characters that I don't understand.  In the future I can always add some back in again but at that point I will (hopefully) understand what I am reintroducing.

Finally it is just the case of adding the buffer name and major mode which comes straight from the examples given in the video.

I especially like the control over the file name colour profile in combination with the use of the `mode-line-window-selected-p` function (introduced as part of emacs 29) which helps to identify the buffer in focus.

As with all these things I will use it and adapt accordingly and hopefully not feel the need to add everything back in again!, I think I like this simplicity for now.
