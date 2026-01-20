---
title: "Centering the Cursor After a Scroll"
author: ["James Dyer"]
lastmod: 2022-10-11T00:00:00+01:00
tags: ["scrolling", "emacs", "elisp", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--centering-cursor-after-scroll__emacs_linux.jpg"
---

Something has been nagging at me for a while now, I seem to spend a lot of time moving the cursor to the next or previous line within a window, spamming `C-n` and `C-p` to get where I want to within a file.

<!--more-->

{{< figure src="/emacs/emacs--centering-cursor-after-scroll__emacs_linux.jpg" class="emacs-img" >}}

Why is this?

Well I am scrolling quite often using `C-v` and `M-v` and this has a side effect on the cursor by pushing it to the top or bottom of the window. After I scroll eyeball a file to the desired location I would like my cursor to be vaguely in the center of the window as this is naturally where my eye would have been to identify the file location.

Although I have upgraded the default emacs scrolling to smooth scrolling by using good-scroll I am still having this issue.

One solution is to use `M-r`

```elisp
(move-to-window-line-top-bottom &optional ARG)
Position point relative to window.
```

which moves the cursor first to the middle then the top and then to the bottom of the window. So after a scroll I can just muscle memory `M-r`?  Well I am scrolling quite a lot and the key combination is a little awkward to get to and I want to limit my key presses as much as possible.

Another possible solution is to use **recenter-top-bottom** but this will just add an extra scroll and the cursor position will be in the wrong place anyway.

There may also be packages out there that will help but I am choosing to ignore this option for the moment due to wanting to keep vanilla emacs as much as possible (yes I know I keep banging on about this!)

Lets see what I can do with some elisp.

Before I discovered the wonder of emacs smooth scrolling using good-scroll I was using some elisp to have greater control over the number of lines the basic **scroll-**-command\* would scroll, so I had something like this:

```elisp
(defun window-some-height () (max 1 / (1- (window-height
                                           (selected-window))) 4)))

(defun scroll-up-some ()
  (interactive)
  (scroll-up-command (window-some-height))
  )

(defun scroll-down-some ()
  (interactive)
  (scroll-down-command (window-some-height))
  )
```

with the scroll-some functions bound to the standard scroll bindings.

Now good-scroll is enabled it morphed into the following:

```elisp
(defun scroll-up-some ()
  (interactive)
  (good-scroll-up)
  )

(defun scroll-down-some ()
  (interactive)
  (good-scroll-down)
  )
```

as the scroll amount is defined by good-scroll.

My idea here is to modify the scroll functions to add some post scroll cursor centering commands.  Maybe I could use (move-to-window-line-top-bottom)!. Well I tried it, but it just didn't seem to make any difference, I tried some delays, e.t.c.  I suspect that the problem here is that good-scroll has a certain method to achieve its smooth scrolling which is incompatible with the function calls I am initially trying.

Well I decided to keep throwing ideas at the scroll functions to see if I could somehow accidentally force a roughly consistent cursor centering after a scroll and see if something sticks (there may be a better term for this!)

Scroll lock didn't work!, margins didn't work!

Eventually I managed to find a function call that seemed to make a difference to the cursor position after a scroll but didn't seem to position the cursor where I thought it should do given the documentation. The function is:

```nil
  (move-to-window-Line ARG)

  Position point relative to window.
  ARG nil means position point at center of window.
  Else, ARG specifies vertical position within the window;
  zero means top of window, negative means relative to bottom
  of window, -1 meaning the last fully visible display line
  of the window.

  Value is the screen line of the window point moved to, counting
  from the top of the window.
```

Passing in a 'nil parameter was close but the cursor was too skewed to either end (but at least had a consistent effect on the position of the cursor), however it had little or no effect for half height windows after a horizontal split.

However, passing in different arguments to the **move-to-window-line** function have me differing post scroll cursor locations giving me a licence to that well known tried and tested foolproof method of trial and error!

Of course for a full height window I now had the perfect setup but what about those pesky half height and maybe smaller windows?. There is a valid argument that might say that smaller windows are generally so small that even if the cursor gets pushed to the top or bottom then it won't take too much effort to reposition the cursor. I would say to an extent that is true but for a half height window I would still want to have a centering cursor option after a scroll.

So again elisp to the rescue!, I modified my scroll functions as thus:

```elisp
(defun scroll-up-some ()
  (interactive)
  (good-scroll-up)
  (if (< 50 (window-body-height))
      (move-to-window-Line -14)
    (move-to-window-Line -3)
    )
  )
(defun scroll-down-some ()
  (interactive)
  (good-scroll-down)
  (if (<50 (window-body-height))
      (move-to-window-Line 14)
    (move-to-window-Line 8)
    >
```

The value of 50 was a window line height less than a full window height with a little taken off, but not too much to make it smaller than half height!

Once I have the move-to-window-line values tweaked accordingly for my setup then in theory I have a decent functioning cursor centering solution. Of course there are some foibles, like what happens if I go full-screen with emacs, e.t.c but this seems to work well for me for the moment and I am now moving the cursor around less often within a buffer.

As with all these things and especially emacs I'm sure a better simpler, probably built-in solution will present itself, but that may be for another day...
