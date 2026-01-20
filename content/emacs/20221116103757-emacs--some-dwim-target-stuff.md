---
title: "Revisiting Window Cut / Copy Files with DWIM"
author: ["James Dyer"]
lastmod: 2022-11-16T00:00:00+00:00
tags: ["emacs", "dwim", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20221116103757-emacs--some-dwim-target-stuff.jpg"
---

I previously wrote about wanting the ability in emacs to copy and paste files from one window to another just like a linux GUI file manager and after a little hunting around and experimentation I settled on putting together some elisp:

<!--more-->

[Cut / Copy Files from Window to Window using Dired Buffers ]({{< ref
"/emacs/emacs--cut-copy-files-using-dired-buffers__emacs_linux.md" >}})

This worked pretty well for a while although there were a couple of side effects that I hadn't bargained for!

The first was the bypassing of the trash/wastebasket, of course this can be easily remedied by for example installing trash-put as part of my copy but that is starting to add more lines to the elisp and I didn't want to complicate things too much.

Also the single rename functionality in **dired** now didn't work as I essentially remapped the basic functionality of the **R** key to set up a flag for a paste operation and I have come to realise that I use the rename often as I would do in a GUI file explorer.

All I want to do is to copy/move files from one emacs window to another, is that too much to ask?, and no, no it isn't, as there is a much simpler way to achieve this without any of the foibles listed above.

It is:

{{< figure src="/emacs/20221116103757-emacs--some-dwim-target-stuff.jpg" width="300px" >}}

`Dired tries to guess a default target directory`

Yay!

So just open a couple of dired windows, mark and operate and the directory dired suggests is the second or destination window dired directory.

Now you may think my previous efforts were wasted but I would disagree with this assumption, I have started to get to grips with elisp, managed to get some elisp working and continued reconsidering my general emacs workflow which as we know will save me time in the long run.

Oh and another thing, the power of writing a blog and trying to articulate what I think I know means that I have read further into the **Help** for **dired-dwim-target**.  This helps to clarify a few things as sometimes I get a little muddled with renaming files and which window/buffer dired is actually being used, here is a Help snippet:

```nil
You can customize it to prefer either the next window with a Dired
buffer, or the most recently used window with a Dired buffer, or to
use any other function.  When the value is a function, it will be
called with no arguments and is expected to return a list of
directories which will be used as defaults (i.e. default target and
"future history") (though, ‘dired-dwim-target-defaults’ might modify
it a bit).
The value t prefers the next windows on the same frame.
```

This is very much something I shall be looking into at a future date, but for now I have something that works and reduces the size of my **.emacs** file.
