---
title: "Shrinking Media With The Help Of Emacs"
author: ["James Dyer"]
lastmod: 2023-07-30T15:28:00+01:00
tags: ["emacs", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230730102057-emacs--Shrinking-Media-With-The-Help-Of-Emacs.jpg"
---

Often I find myself refining my collection of photos and family videos, usually with the following process:

<!--more-->

{{< figure src="/emacs/20230730102057-emacs--Shrinking-Media-With-The-Help-Of-Emacs.jpg" class="emacs-img" >}}

-   removing media clutter
-   removing any duplicates
-   tagging as necessary
-   compressing where reasonable
-   renaming to a more `denote` format

Emacs and the associated muscle memory greatly helps with this process.

Firstly though I leverage other applications, for example, duplicate removal and tagging takes place through `digikam`, media clutter through `thunar` and `gthumb`

My emacs process is then:

-   open media directory in `dired`

-   sort by size using `C-u s S` - the big S is setting the ls format by size

-   `M-<` `(beginning-of-buffer)` - so I can see the largest files

-   if I want to squish a video I will probably want to preview it first in **mpv**, so I do this asynchronously using `&` `(dired-do-async-shell-command)` and relying on my defined `dired-guess-shell-alist-user` setup and then selecting `i` (mpv shortcut) to have a quick peek at the original dimensions.  Typically if the video is 1920x1080 then I like to halve the dimensions which when running through `ffmpeg` saves about 80-90% on disk space.

    I have my own `ffmpeg` bash scripts which I call through `dwim-shell-command` so a command search for something like **dw sh** `my/dwim-video-shrink` will dired execute the file under cursor or over a list of marked files.

-   for images I again use `dwim-shell-command` with a quick command search for something like **dw cr** `my/dwim-picture-crush` which isn't always as brutal as it might suggest but a script that I regularly modify when compressing images, setting the compression parameters appropriately.

    If I need to quickly inspect the image then I could open it in emacs but I prefer again to open in gthumb using `&`

-   Possibly at the end of a directory compress I might check the directory size by `?` in `dired` which I have mapped to an elisp `my/get-file-size` function that runs the `async-shell-command` `du -`

-   and of course my files can be easily renamed through `wdired`

As I am using a tiling window manager all of this can be accomplished keyboard only and on a single screen!
