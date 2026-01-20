---
title: "Ready Player Mode with a little elisp Tweak"
author: ["James Dyer"]
lastmod: 2024-08-10T09:42:00+01:00
tags: ["ready-player", "emms", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240810072449-emacs--Ready-Player-Mode-Tweak.jpg"
---

I'm really enjoying **ready-player** at the moment, so much so I think it might end up replacing **emms**.

<!--more-->

{{< figure src="/emacs/20240810072449-emacs--Ready-Player-Mode-Tweak.jpg" width="100%" >}}

I listen to music in a very simple, almost old-fashioned way, album by album, and I have my music collection well organized, with each album in a separate directory and in a way, `emms` was a little too much.

`ready-player` is well integrated with dired, so opening an audio file, for example, will kickstart the listening process for me.  I like to have shuffle on (`ready-player-shuffle`), and with `ready-player-repeat` if there is more media to play in the current directory then it will enter a continuous play mode.

Here are my current settings:

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
   '(
     ("mpv" "--audio-display=no")
     ("mplayer")
     ("ffplay")
     ("vlc")
     )))
```

The only little issue I have run into so far (and I encountered something similar with `emms`) is to figure out which media player back-end works best with my set-up, I am running SwayWM on Arch.

For some reason `emms` would only run well with `vlc` (found by trial and error).  With `ready-player` the default `mpv` worked well in most cases except it wouldn't automatically move on to the next audio track when `ready-player-repeat` was set.

I am happy with `ready-player` opening videos through `mpv` by default and would only really want to ever use `mpv` for video playback.

So first things first, lets establish which back-end gives me a continuous audio playback.

With a little trial and error (simply swapping around the back-ends) I found that `mplayer` was the one that worked for me, `mplayer` can play videos too but my muscle memory is so mpv-centric that I always want to be using mpv for video playback.

With `ready-player` as far as I can tell there is just a single list of potential playback commands which applies to all media types.  `ready-player` is in its early stages of development so I wouldn't be surprised if at some stage this might get added for more flexibility/customization.  But for me it doesn't really matter and in fact gives me the opportunity to flex my elisp know-how.

Can I write something to overcome this issue?

Emacs is essentially just an elisp machine anyway, so lets write some elisp to augment the current `ready-player` functionality to suit my multiple playback-end needs, here is some elisp:

```elisp
(defun set-ready-player-commands ()
  "Set `ready-player-open-playback-commands` based on file extension."
  (let ((file-extension (file-name-extension (buffer-file-name))))
    (setq ready-player-open-playback-commands
          (cond
           ((member file-extension '("mp4" "mkv" "mov" "avi"))
            '(("mpv" "--audio-display=no")
              ("mplayer")
              ("ffplay")
              ("vlc")))
           ((equal file-extension "mp3")
            '(("mplayer")
              ("mpv" "--audio-display=no")
              ("ffplay")
              ("vlc")))
           (t
            '(("mpv" "--audio-display=no")
              ("vlc")
              ("ffplay")
              ("mplayer")))))))

(add-hook 'find-file-hook 'set-ready-player-commands)
```

Simply, I just added some extra `ready-player` setup whenever a buffer is loaded from a file, essentially when I select it from dired.

I set the playback commands based on the extension, so for a video file it can use `mpv`, for an mp3 it prefers `mplayer`, with a fallback for any other type of file.  I guess I could have been a little more flexible in defining a wider range of audio file, but I generally only listen to mp3.

With that addressed, `ready-player` now seems to give me much less friction than `emms`, with `emms` I would have to perform roughly the following steps to even begin listening to a music album:

-   first initialise - add files so emms can load in metadata
-   browse to select music album
-   push to a playlist
-   jump to playlist to perform track functions

I would have to remember `emms` commands, or build up muscle memory, but with `ready-player` I just need to navigate to the relevant music folder using `dired`, open a file and I'm ready to go!, and the front-end is like any standard media player which makes more sense to me.

If I want to modify playback then I'm quite happy to jump to the playback buffer which can be accomplished quickly through vertico by fuzzy finding "ready", or by pushing `ready-player` to its own tab.

I think I will continue to play around with this cool little package.
