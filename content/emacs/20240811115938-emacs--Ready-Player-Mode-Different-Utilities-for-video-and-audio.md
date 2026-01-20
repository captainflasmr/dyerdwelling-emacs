---
title: "Ready Player Mode different Utilities for Video and Audio!"
author: ["James Dyer"]
lastmod: 2024-08-11T16:00:00+01:00
tags: ["ready-player", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240810072449-emacs--Ready-Player-Mode-Tweak.jpg"
---

Regarding my previous post, you can now disregard my little Frankensteinian elisp effort!:

<!--more-->

[Ready Player Mode with a little elisp Tweak]({{< ref
"/emacs/20240810072449-emacs--Ready-Player-Mode-Tweak.md" >}})

Thanks to a quick turnaround from Author: Alvaro Ramirez <https://xenodium.com> URL: <https://github.com/xenodium/ready-player> Version: 0.4.1 now directly caters for the ability to use different utilities for video and audio, as thus:

```elisp
(use-package ready-player
  :init
  (ready-player-mode 1)
  :custom
  (ready-player-thumbnail-max-pixel-height 200)
  (ready-player-autoplay nil)
  (ready-player-repeat t)
  (ready-player-shuffle t)
  (ready-player-open-playback-commands
   '((ready-player-is-audio-p "mplayer")
     (ready-player-is-video-p "mpv"))))
```

Really nice!! 😀

{{< figure src="/emacs/20240810072449-emacs--Ready-Player-Mode-Tweak.jpg" width="100%" >}}
