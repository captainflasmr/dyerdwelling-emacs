---
title: "The Smallest of Productivity Gains by Instantly Opening Dired Files when isearching"
author: ["James Dyer"]
lastmod: 2025-05-13T09:20:00+01:00
tags: ["emacs", "dired", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250513085926-emacs--Instantly-Open-Dired-Files-with-Isearch-and-Enter.jpg"
---

If you’re an Emacs user (which I know you are), especially one who lives in `dired-mode`, you’re probably familiar with the quick power of `isearch` for finding files or directories. But if you’re like me, you might have noticed a tiny speed bump in the workflow: after finding a file or directory with `isearch`, you would typically have to hit `<enter>` to exit the search, and then `<enter>` again to open the entry. That’s two steps for something that feels like it should be one and this has been a very minor annoyance for me for a very long time now.

<!--more-->

{{< figure src="/emacs/20250513085926-emacs--Instantly-Open-Dired-Files-with-Isearch-and-Enter.jpg" width="100%" >}}

I had a little time to shave some more yak hair, so lets try and address this!

The solution I came up with was to add a bit of advice to `isearch-exit`, so now, when you’re in `dired-mode` and you use `isearch` to locate a file or directory, pressing `<enter>` will both exit the search **and** immediately open the file or directory, no need for a second confirmation.

Here’s the magic!

```emacs-lisp
(defadvice isearch-exit (after dired-enter-directory-or-file activate)
  "In dired mode, enter directory or open file after isearch."
  (when (eq major-mode 'dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (when file
        (dired-find-file)))))
```

The only thing I am a little worried about are the side effects, but I guess we shall see...

And another thing, now my `dired` navigation is one step smoother; how much time generally is it going to save me? Was it worth the effort?.

Let’s say, conservatively, that I have pressed that extra Enter 100 times each day. Each press takes, say, 150 ms, accounting for human reaction times, even though it isn’t really a reaction, more of a muscle memory response.

Let’s do the calculations! I have no idea why I’m doing this, but maybe I can validate the amount of time I spent on this for future time-saving gains. Maybe it will make me feel better about the countless hours of yak shaving I have done over many years (with Emacs, of course, although I do have a yak in the garden shed that really needs some attention!)

So, the savings are as follows:

-   **Day:** 15 seconds
-   **Week:** 105 seconds
-   **Month:** 7 minutes
-   **Year:** 1 hour 24 minutes

Well, the yearly total is probably about the amount of time I actually took to implement this, so that means in a year I will be making productivity gains! Sweeeeeeet!
