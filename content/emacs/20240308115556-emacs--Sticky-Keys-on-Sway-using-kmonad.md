---
title: "kmonad Sticky Keys on Sway to Help Prevent RSI"
author: ["James Dyer"]
lastmod: 2024-03-08T20:48:00+00:00
tags: ["sway", "sticky-keys", "kmonad", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240308115556-emacs--Sticky-Keys-on-Sway-using-kmonad.jpg"
---

I primarily use Emacs on a laptop, and unfortunately, Emacs isn't inherently designed for ergonomic use. Coupled with the less-than-ideal typing experience of continuous laptop use, I'm considering adopting preemptive measures to protect my hands from potential strain injuries, including the infamous emacs pinky and RSI.

<!--more-->

{{< figure src="/emacs/20240308115556-emacs--Sticky-Keys-on-Sway-using-kmonad.jpg" width="100%" >}}

My awareness of this issue heightened after watching numerous informative coding videos by Xah Lee, from which I've gleaned valuable tips on making simple yet effective adjustments to safeguard my hands.

One recommended strategy was relocating the Ctrl key to the Caps Lock position, a common suggestion that I had already implemented a few years ago. Another was to use a mechanical or ergonomic keyboard, which I have not considered feasible for my needs as I have a strong preference for using a laptop, particularly for its versatility as a digital art tablet with a 2-in-1 design.

Nonetheless, there's another solution within my reach: Sticky Keys. Although I've been predominantly aware of Sticky Keys through the inconvenient pop-up in Windows triggered by prolonged keypresses, Xah Lee's insights shed light on its potential benefits in alleviating the strain caused by emacs' key chord demands. My only concern is that my preferred Linux window manager, Sway, lacks built-in support for Sticky Keys, unlike Gnome and KDE.

Fortunately, there is a tool I am already using right under my nose that I had overlooked and that is **kmonad**.

Just for clarification, Sticky Keys allow the user to perform key combinations by pressing keys in sequence rather than simultaneously hence splitting emacs key chords into individual keypresses.

Here is a little summary of what **kmonad** can do:

>
>
> KMonad offers advanced customization features such as layers, multi-tap, tap-hold, and much more. These features are usually available at the hardware level on the QMK-firmware enabled keyboards. However, KMonad allows you to enjoy such features in virtually any keyboard by low-level system manipulations.
> <br />
> <br />
> KMonad lets you map any keyboard button to any keymap. Want to swap the useless Caps Lock key with the Escape key? Want to have your modifiers such as Shift and Control on your home row, without breaking your normal typing flow? Want a modifier that is combination of Alt + Ctrl + Super + Shift? You can do all of those and much more!

I am already using kmonad to map the caps lock key to ctrl but on reading the excellent tutorial file I realised that in the latest version, namely v0.4.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2023-10-07 Sat&gt;</span></span>, it included sticky-key functionality!

Here is a little kmonad documentation regarding sticky keys:

>
>
> KMonad also supports so called "sticky keys".  These are keys that will behave as if they were pressed after just tapping them.  This behaviour wears off after the next button is pressed, which makes them ideal for things like a quick control or shift.  For example, tapping a sticky and then pressing \`abc' will result in \`Abc'.
> <br />
> <br />
> You can create these keys with the \`sticky-key' keyword:
> <br />
> <br />
> (defalias
> slc (sticky-key 500 lctl))
> <br />
> <br />
> The number after \`sticky-key' is the timeout you want, in milliseconds.  If a key is tapped and that time has passed, it won't act like it's pressed down when we receive the next keypress.
> <br />
> <br />
> It is also possible to combine sticky keys.  For example, to get a sticky shift+control you can do
> <br />
> <br />
> (defalias
> ssc (around
> (sticky-key 500 lsft)
> (sticky-key 500 lctl)))

The Sway kmonad configuration below will activate a sticky key for all my linux modifier keys along with mapping the caps lock to the ctrl key:

```nil
(defcfg
  input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
  output (uinput-sink "My KMonad output")

  ;; Comment this if you want unhandled events not to be emitted
  fallthrough true

  ;; Set this to false to disable any command-execution in KMonad
  allow-cmd true
  )

(defsrc caps lctl lsft rsft lalt ralt lmet)

(deflayer mine
  (sticky-key 2000 lctl)
  (sticky-key 2000 lctl)
  (sticky-key 2000 lsft)
  (sticky-key 2000 rsft)
  (sticky-key 2000 lalt)
  scrlck
  (sticky-key 2000 lmet))
```

For us emacs types the configuration file also looks a bit lispy! 😀

Kmonad is also cross platform so it will run on windows too and for my next post I'm going to look to replace my current Autohotkey setup in windows with kmonad and see if it works as well as it does in linux!
