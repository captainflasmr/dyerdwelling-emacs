---
title: "Revert Git Permission Changes On Repository Transfer"
author: ["James Dyer"]
lastmod: 2023-06-17T20:30:00+01:00
tags: ["magit", "emacs", "bash", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230617200412-emacs--Revert-Git-Permission-Changes-On-Transfer.jpg"
---

I recently reinstalled my laptop and copied across my git repositories from an external backup drive.  However most of my repositories were flagged by `magit` as having been _updated_ mainly with the following issue on multiple files:

<!--more-->

```nil
old mode 100644
new mode 100755
```

I found a fix for a single repository on `stackoverflow` but I have quite a few repositories now, so I wrote the following bash script borrowing the core git diff command fix and then ran it in `eshell`

```bash
#!/bin/bash

LIST="DCIM/Art/Content/ArtAssets
  DCIM/Art/Content/ArtRage
  DCIM/Art/Content/ArtRagePenTool
  DCIM/Art/Content/ArtRageTabletFriend
  DCIM/Art/Content/InfinitePainter
  DCIM/Art/Content/Krita
  DCIM/content
  bin
  publish"

for item in $LIST; do
    echo $item
    cd ~/$item
    git diff -p -R --no-ext-diff --no-color \
        | grep -E "^(diff|(old|new) mode)" --color=never \
        | git apply
done
```

This is where the integrated nature of emacs can come in useful, although the `stackoverflow` example was the git diff line I wanted to perform this for each of my repositories and to achieve this I just simply grabbed the Path column output of `magit-list-repositories` using `rectangle-mark-mode` and trimmed down using some macros so I could iterate through each repository directory and run the command each time to clean up.

{{< figure src="/emacs/20230617200412-emacs--Revert-Git-Permission-Changes-On-Transfer.jpg" class="emacs-img" >}}
