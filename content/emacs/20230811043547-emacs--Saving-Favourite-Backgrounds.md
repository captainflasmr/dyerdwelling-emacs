---
title: "Saving My Favourite Wallpapers"
author: ["James Dyer"]
lastmod: 2023-08-11T05:22:00+01:00
tags: ["sway", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230811043547-emacs--Saving-Favourite-Backgrounds.jpg"
---

I am using the `styli.sh` script as my wallpaper changer in **sway** and with a little transparency and wallpapers from **unsplash** I can quickly switch randomly through my wallpaper images and get things looking pretty nice.

<!--more-->

{{< figure src="/emacs/20230811043547-emacs--Saving-Favourite-Backgrounds.jpg" class="emacs-img" >}}

I tend to switch a few times before I find one that I like, but that image tends to disappear when I restart my laptop and the wallpaper gets reset.  What I would like to do is to have the ability to send the current wallpaper to a wallpaper favourite folder to build up a chosen set of images.

The first step to achieving this would be to add a couple of lines to the `styli.sh` bash script to copy the current wallpaper image to a defined location.  I am guessing that sway stores this image somewhere but as `styli.sh` handles all the variables and logic I might as well add in the following:

```bash
swaymsg output "*" bg "$WALLPAPER" "$MODE"
rm ~/wallpaper-faves*.{jpg,jpeg,png,gif}
cp "$WALLPAPER" ~/wallpaper-faves-${WALLPAPER##*/}
```

and now for some emacs elisp to move this wallpaper image into a favourite folder :

```elisp
(defun my/copy-background-to-faves ()
  "Copy the current sway background to wallpaper faves folder"
  (interactive)
  (let* ((source-folder "/path/to/home/")
         (faves-folder "/path/to/wallpaper/faves/")
         (image-files (directory-files source-folder nil "^wallpaper-faves.*\\.\\(jpg\\|jpeg\\|png\\|gif\\)$" nil nil)))
    (dolist (image-file image-files)
      (rename-file (concat source-folder image-file) (concat faves-folder image-file) t)
      )
    )
  )
```

Now over time my `faves-folder` will build up with all my cherished wallpapers and I can begin the process again!
