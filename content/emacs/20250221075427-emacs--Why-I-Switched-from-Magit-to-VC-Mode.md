---
title: "Why I Switched from Magit to VC-Mode (and How It Works for Me)"
author: ["James Dyer"]
lastmod: 2025-02-21T07:54:00+00:00
tags: ["vc-mode", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250221075427-emacs--Why-I-Switched-from-Magit-to-VC-Mode.jpg"
---

I am currently using `vc-mode` for my source code configuration needs.  I wouldn’t call myself a die-hard `vc-mode` user, (at least not yet!). To earn that title, I think I would need years of experience with it and scoffing at this newfangled thing they call `magit`, while my muscle memory recoils at the thought of reading and interacting with a transient menu!

<!--more-->

{{< figure src="/emacs/20250221075427-emacs--Why-I-Switched-from-Magit-to-VC-Mode.jpg" width="100%" >}}

If that were the case, I’d fully embrace the label of a die-hard holdout. But in reality, that just isn't true.

My version control history has been, well, complicated, or to put it another way, quite simple. It largely revolves around that ancient system known as Subversion (`svn`), and leveraging it through the good old command line. It wasn’t until the past couple of years that I even stepped into the Emacs version control ecosystem. So where to start? Naturally, I had heard a lot of praise for `magit`.

First question (and I suspect the usual question): how do you even pronounce it? A soft "g" like in **magic**? That sounds better, even if it doesn’t make much sense. Then again, given that I’d likely never discuss it with colleagues at work, pronunciation hardly matters.

Anyway, I happily "magited" for a year or so (ironically, I think I learned more about Git's command line through `magit` than I would have otherwise). I would have continued using it too, except I decided to make a mental paradigm shift regarding my use in Emacs in general.

I had reached a point where I think I knew enough Emacs Lisp to start replacing many external packages with custom functions. It was a personal challenge, an opportunity to deepen my understanding of `elisp` and tackle more advanced customization. What better way to learn than by replacing something like `ace-window`? As it turns out, most of these substitutions were fairly straightforward.

But `magit`? Now that was a tough one. Should I create my own transient menu?, or a more basic menu system? It’s Git, so that could quickly become a tangled mess of options. At the same time, I couldn’t ignore my need for Subversion support at work. `magit`, as great as it is, is Git-only (though `git svn` exists).

Wouldn’t it be nice if there was a version control system that seamlessly handled multiple backends?

Turns out (as we already know), Emacs has one built-in and that is `vc-mode`.

I’ve now been using `vc-mode` for about a year. I still appreciate `magit`, but for my current workflow, balancing Git for personal projects and Subversion at work, `vc-mode` gets the job done.

I'm not generally using Git in any advanced fashion anyway and if I do need to drop into the command line then I can just open up a terminal in emacs.

Yes `vc-mode` has its limitations, for example, why is creating a tag not part of the default keymap of `vc-dir`?, actually I think it is and I vaguely remember activating it through `vc-create-branch`, was it a universal argument?, I can't remember, but how about just doing things the old-fashioned way, `M-x vc-create-tag`?, that works fine for me.  Maybe in the not too near future I can yak shave that away anyway! :)

So far with `vc-mode` I’ve made some small improvements, which I shall share in future blog posts:

-   Adding the ability to toggle tracked files on and off.
-   Displaying the current branch permanently in the `vc-dir` header line.

And actually the `vc-mode` tweaks above were the original topic of this post, but I seem to have rambled on long enough...
