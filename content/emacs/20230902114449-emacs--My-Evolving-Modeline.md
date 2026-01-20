---
title: "My Evolving Modeline"
author: ["James Dyer"]
lastmod: 2023-09-02T12:32:00+01:00
tags: ["modeline", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230902114449-emacs--My-Evolving-Modeline.jpg"
---

I am a tinkerer and of course emacs is a perfect vehicle for this mentality.  A prime example of this is my constant evolving modeline.  For many years the default modeline hasn't presented itself as being a problem, but is it optimal for me?, the answer I have now realised is no, and I think I can do better for my use cases.

<!--more-->

{{< figure src="/emacs/20230902114449-emacs--My-Evolving-Modeline.jpg" class="emacs-img" >}}

So after a period of introspection (regarding emacs! 🙂) I think I can now define a clear set of criteria for my perfect modeline:

1.  Show the full pathname

    By default, emacs displays the buffer name, but during coding sessions, I often prefer to see the full file path, especially since I sometimes work with multiple copies of files with the same name.

2.  Clearly indicate when a file has been modified

    For when I accidentally insert a 'n' or 'p' character in a buffer (I think you can guess how this happens!) it will be immediately apparent and I will be less likely to break my emacs config.

3.  Have control over the faces

    Define my own sizing and colouring for an active and inactive buffer modeline to make clearer my current selected buffer.  Otherwise I will be at the whim of my current theme which may not always be optimal.

4.  Efficient information display

    Fit as much information in a compact manner as possible, I don't want to be hunting around for buffer information, for example the column number and major modes should always be displayed.

5.  Version control menu should be available

    I frequently use git/magit for version control and prefer to keep the menu bar disabled. This choice sometimes leads me to search for built-in VC (Version Control) commands within emacs. For instance, registering a new file in magit can be tricky since untracked files seem to only display top-level directories unless a subdirectory file has been registered. To include all files in a directory, I rely on vc-register, but I don't always remember the key chord. Therefore, having a VC menu in the mode line is immensely helpful.

6.  Simplicity

    This includes keeping the modeline elisp definition as simple as possible for easier maintainability / transparency and never allowing extra items to creep in like email or miscellaneous information.

Given all this I have come up with the following:

```elisp
(setq-default mode-line-modified
              '(:eval (if (and (buffer-file-name) (buffer-modified-p))
                          (propertize " * Modified " 'face
                                      '(:background "#e20023" :foreground "#ffffff")) "")))
(set-face-attribute 'mode-line-active nil :height 125
                    :background "#ffffff" :foreground "#000000")
(set-face-attribute 'mode-line-inactive nil :height 110
                    :background "#000000" :foreground "#ffffff")
(set-face-attribute 'mode-line nil :height 120)
(set-face-attribute 'mode-line-inactive nil :height 120)

(setq-default mode-line-format
              '("%e"
                mode-line-modified
                (:eval
                 (if (buffer-file-name)
                     (if (mode-line-window-selected-p)
                         (propertize (format "%s " (abbreviate-file-name (buffer-file-name)))
                                     'face '(:background "#6b91c0" :foreground "#ffffff" :inherit bold))
                       (format "%s " (abbreviate-file-name (buffer-file-name)))
                       )
                   )
                 )
                (:eval
                 (if (mode-line-window-selected-p)
                     (propertize "%o %4l %2c %b"
                                 'face '(:background "#b8b8b8" :foreground "#000000"))
                   "%o %4l %2c %b"))
                (vc-mode vc-mode)
                mode-line-modes
                )
              )
(setq mode-line-compact t)
```

I have tried to use the built-in modeline variables where possible and use `mode-line-compact` which replaces repeating spaces with a single space.

I found the best way to indicate a modified file and for it to clearly stand out on the modeline is by colour and string length, but I still wanted to keep the asterisk convention as this is quite common.

I decided to subtly change the size of the active and inactive modeline for extra emphasis and this can be massively exaggerated to taste.
