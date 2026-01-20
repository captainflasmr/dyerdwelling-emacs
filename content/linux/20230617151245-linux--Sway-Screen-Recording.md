---
title: "Screen-Recording"
author: ["James Dyer"]
lastmod: 2023-06-17
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230617151245-emacs--Sway-Screen-Recording.jpg"
---

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [script](#script)
- [OBS](#obs)

</div>
<!--endtoc-->


## script {#script}

I have created the following script to command line cater for x11 and wayland:

```bash
#!/bin/bash
# ~/.config/kwinrc
# [Compositing]
# AllowTearing=true
# GLCore=false
# GLTextureFilter=1
# HiddenPreviews=4
# LatencyPolicy=ExtremelyLow
# OpenGLIsUnsafe=false
DATE=$(date +%Y%m%d%H%M%S)
DST=~/Videos
# for sway
if [[ $XDG_SESSION_TYPE == "wayland" ]]; then
    wf-recorder -f "$DST"/$DATE--screen-recording.mp4 -F fps=10
else
    # for X11 only
    ffmpeg -video_size 1920x1080 -framerate 10 -f x11grab -i :0.0+0,0 -c:v libx264rgb -crf 23 -preset ultrafast "$DST"/$DATE--screen-recording.mp4
fi
```


## OBS {#obs}

Works out of the box in X11 but need to install `wlrobs-hg` to get it to work on Wayland
