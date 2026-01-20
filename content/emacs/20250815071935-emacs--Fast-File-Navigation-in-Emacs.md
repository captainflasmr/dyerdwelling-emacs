---
title: "Fast Recent File Navigation in Emacs"
author: ["James Dyer"]
lastmod: 2025-08-16T19:07:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250815071935-emacs--Fast-File-Navigation-in-Emacs.jpg"
---

As an Emacs user, you're always hunting for ways to shave milliseconds off common tasks and generally noodling around and shaving some more of that yak!. File switching is one of those operations you do hundreds of times a day, so even small improvements compound dramatically. Today, I want to share a workflow that I have been tinkering with that combines the best of Emacs's built-in recent file tracking with modern completion interfaces.

{{< figure src="/emacs/20250815071935-emacs--Fast-File-Navigation-in-Emacs.jpg" width="100%" >}}

Emacs gives us several ways out of the box to quickly access files:

-   find-file
-   switch-to-buffer for open buffers
-   Registers
-   Bookmarks
-   recentf-mode for recently accessed files

and I have a funny feeling there are probably more.

Each has its place, but they all have limitations. switch-to-buffer only shows currently open buffers, and with fido-mode, I have found that the most frequently used buffers don't always bubble to the top predictably, I may need to look a little more into this though. Registers are great but require manual setup and only remember the position when you set them, not your last edit location.

What I really wanted was instant access to my recently edited files, with each file opening exactly where I left off!

I thought I had become quite efficient at file switching through fido completion and dired but only recently I realised I was navigating through dired to find files way too often and wasting time.  I think this is probably typical of an Emacsers journey and after a while of continuous minor toil there comes a time to take action and shave off those milliseconds and improve flow.

The solution starts by enabling two built-in  Emacs features:

```elisp
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)
```

save-place-mode automatically remembers your cursor position in every file and restores it when you reopen the file, it was the first time I had heard of this when figuring all this out and couldn't believe I didn't know about it before (not an uncommon Emacs experience), this works across Emacs sessions, so you get that "pick up exactly where I left off" experience.

And recentf-mode maintains a persistent list of recently visited files, ordered chronologically. Unlike switch-to-buffer, this survives Emacs restarts and gives you a true history of your work and I thought in this case it better to limit the list size as much as possible for greater clarity.

The traditional recentf-open-files command shows a nice ordered list in a buffer but to some it could feel clunky compared to modern completion interfaces.  There is also recentf-open which does go through completing read but then reorders the recentf list for some reason, so to me the obvious solution is to push the recent files list through completing-read myself:

```elisp
(defun my/fido-recentf ()
  "Use fido to select from recently opened files."
  (interactive)
  (completing-read "Recent file: " recentf-list nil t nil 'recentf-list))
```

To get this to work the way I wanted, I had to supply recentf-list also as the last argument. By using the same symbol for both the collection and the history, we tell the completion system to respect the original ordering. Your most recently accessed files should stay at the top!

In my final solution I have decided to keep the old recentf-open-files interface as an option. Strangely I actually like the old list in a buffer interface so I have included it in my final version as an option.

```elisp
(defun my/fido-recentf (arg)
  "Use fido to select from recently opened files.
With universal argument, use the traditional recentf-open-files interface."
  (interactive "P")
  (if arg
      (recentf-open-files)
    (find-file (completing-read "Recent file: " recentf-list nil t nil 'recentf-list))))
```

Now M-x my/fido-recentf gives you completing read, while C-u M-x my/fido-recentf drops you into the classic numbered list.

Bind this to something convenient (I use M-o) and your file navigation transforms! (well that might be a bit of an exaggeration)

This might seem like a small optimization, but it exemplifies what makes Emacs special. We're not just using the tools as shipped, we're composing them in novel ways to create something perfectly tailored to our needs.

That's the kind of workflow improvement that makes every day at the keyboard just a little bit more pleasant, and those small pleasures add up to something significant over months and years of coding.
