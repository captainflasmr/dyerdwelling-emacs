---
title: "Using org-copy-visible in dired"
author: ["James Dyer"]
lastmod: 2023-01-15T00:00:00+00:00
tags: ["quick", "emacs", "dired", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230115202613-emacs--Dired-Copy-Just-Directory-List-To-Kill-Ring.jpg"
---

Just a quick one.

<!--more-->

Often it seems I need a copy of a list of files / directories in plain text without any gubbins such as a path, permissions, date and all those shenanigans, basically `basenaming`; for example:

{{< figure src="/emacs/20230115202613-emacs--Dired-Copy-Just-Directory-List-To-Kill-Ring.jpg" class="emacs-img" >}}

So how can I achieve this in emacs? I would really prefer to use `dired` somehow rather than `shell / ls` (which was my first thought)

Below is my typical dired listing:

```nil
drwxr-xr-x  4 4.0K Jan 15 19:35 Backup
drwxr-xr-x  3 4.0K Jan 14 19:33 Camera
drwxr-xr-x 22 4.0K Jan 15 19:01 content
-rw-r--r--  1   65 Dec 31 16:34 .directory
```

Rectangle marking first came to mind but the paste seems to have a weird format and strangely inserts the text.

So I came up with the following process:

In `dired`, select **'('** `dired-hide-details-mode` which toggles off all the details and gives:

```nil
Backup
Camera
content
.directory
```

This looks very promising and surely a simple **M-w** (kill-ring-save) will work?.  It doesn't work. It still copies the full details :(

However if org-copy-visible is used it does the same as in org mode in that it only copies the visible parts of the region.  I had no idea that functions from one mode can be used in another!
