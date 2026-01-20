---
title: "Cursor Blinking Rate"
author: ["James Dyer"]
lastmod: 2023-04-28T13:00:00+01:00
tags: ["emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230406200632-emacs--Cursor-Blinking-Rate.jpg"
---

Sometimes I can find a blinking cursor distracting and somewhat expectant!, so currently I am favouring a solid non blinking cursor while still being able to easily locate my cursor using `hl-line-mode`

<!--more-->

{{< figure src="/emacs/20230406200632-emacs--Cursor-Blinking-Rate.jpg" class="emacs-img" >}}

I have tried **beacon** and **pulsar** in the past but have found that a simple line highlight nicely serves my purpose.

This however led to a weird issue at work with my emacs setup and although I am not quite sure why it is happening (although I have my theories) I have found a workaround.

As at home I am always tinkering with my emacs init file and I think at some point I decided to turn off cursor blinking.  This coincided with a new virtual machine setup and hence a new emacs install.  At that point I started to notice that sometimes the emacs window wouldn't refresh until I either gave it some keyboard input or strangely I just wiggled my mouse :)

I could reproduce this issue by restarting emacs from a clean start (cleaning out buffers and the desktop file) and switching immediately to a permanent register pointing to a file.  Also I would notice that sometimes when I `dired-jump`'d I would have to tap a key to get the emacs window to refresh.

After a period of trial and error I narrowed the culprit down to :

```elisp
(blink-cursor-mode -1)
```

When I turned on a blinking cursor there would be no such emacs refreshing issue.

I came up with a theory that in a setup involving VM Machines/X11/linux window manager/compositor/emacs/host graphics driver/and so on, there may be a graphical optimization that requires a specific event to occur for an emacs window to refresh. It's possible that the reason everything works smoothly with a blinking cursor is because the cursor is constantly disappearing and reappearing, which triggers a refresh event.

So.... the question is how do I preserve a non blinking cursor and still get an emacs window to consistently refresh?, well I dug into the blinking cursor options and set the following:

```elisp
(blink-cursor-mode 1)
(setq blink-cursor-interval 0.001)
(setq blink-cursor-blinks 1)
```

A single initial blink of the cursor would trigger a reliable display refresh but would be imperceptible to the human eye thus providing a perceived non blinking cursor!

Oh and a final thing, has anyone noticed that by default an emacs cursor blinks but 10 times and then stops?, I've got to say it was only when I was digging around the blink-cursor variables and actively scrutinising the cursor blink behaviour that I noticed.  I had assumed that it just kept blinking forever!
