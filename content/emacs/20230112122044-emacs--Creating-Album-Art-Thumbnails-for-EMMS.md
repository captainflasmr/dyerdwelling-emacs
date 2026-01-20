---
title: "Creating Album Art Thumbnails for EMMS"
author: ["James Dyer"]
lastmod: 2023-01-13T00:00:00+00:00
tags: ["emms", "emacs", "bash", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230112122044-emacs--Creating-Album-Art-Thumbnails-for-EMMS.jpg"
---

I have been looking for a music player on Linux for a while now but haven't really settled on one; I have simple requirements:

<!--more-->

-   A view showing album art
-   An easy way to play random tracks
-   A quick method to skip a track

---

To date I have only been considering players with a graphical front end but it always seems a hassle to, open the program, look for some music, figure out how the shuffle and play work; and then what if I don't like the current track playing?, my laptop isn't really set up to quickly skip the track.

Now I am delving further into the integrated environment of emacs I thought I would see what was on offer.

A little research and I settled on **EMMS**

I will go into a deeper review after I have used it for a few months but mainly for this post I wanted to share a quick bash script I created to allow album art to be shown in EMMS.

By default EMMS looks for a **cover.jpg** file in the current music directory.  My collection is neatly split into directories for each album with album art embedded into each mp3 track.  This has given me the opportunity to write a bash script to automate the creation of these cover.jpg files.

The script is as follows:

```bash
#!/bin/bash
DIRS=$(find "$HOME/MyMusicLibrary" -type d -printf '%p;')

export IFS=";"

for dir in $DIRS; do cd "$dir" files=(*) if [[ ${files[0]: -4} == ".mp3" ]]; then echo $dir ffmpeg -hide_banner -loglevel panic -stats -y \ -i "${files[0]}" -an -c:v copy "$dir/cover.jpg" convert -resize 120x120 "$dir/cover.jpg" "$dir/cover.jpg" fi done
```

Of course change the `"$HOME/MyMusicLibrary"` to your music library location and then in the emacs init file add the following:

```elisp
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
```

and this is an example of the result (my music collection is larger than this!):

{{< figure src="/emacs/20230112122044-emacs--Creating-Album-Art-Thumbnails-for-EMMS.jpg" width="300px" >}}
