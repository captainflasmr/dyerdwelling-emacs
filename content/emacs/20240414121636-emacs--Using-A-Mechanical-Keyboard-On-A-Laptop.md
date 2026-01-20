---
title: "Using A Mechanical Keyboard, Literally On A Laptop!"
author: ["James Dyer"]
lastmod: 2024-06-08T16:55:00+01:00
tags: ["sticky-keys", "keyboard", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240414121636-emacs--Using-A-Mechanical-Keyboard-On-A-Laptop.jpg"
---

The next stage on my quest to mitigate any RSI issues when using Emacs on a laptop is to see if it is a viable option to use a mechanical keyboard on a laptop.

<!--more-->

When I mean "on a laptop" I literally mean **ON** the laptop!!

My idea is to buy a smallish portable mechanical keyboard and just plonk it (technical term) over my current laptop keyboard.

At the moment I know nothing about mechanical keyboards, I just know from an RSI perspective they are generally a good idea.  I do miss the feeling of key travel and I am aware at times I'm tapping too hard on my laptop keyboard due to their shallow depth.

So here is the keyboard in its fully installed glory - extreme plonkification!

{{< figure src="/emacs/20240414121636-emacs--Using-A-Mechanical-Keyboard-On-A-Laptop.jpg" width="100%" >}}

I however encountered notable difficulties that depended largely on the placement of the keyboard's feet in relation to the laptop's built-in keyboard.  Spamming of a single key press was a common issue and was due to my built-in laptop still being enabled when the mech-keyboard was plugged in.

Simply solved however with a bash script to disable the laptop keyboard when the mech is plugged in.

Note: this will be SwayWM specific but can easily be adapted by replacing the swaymsg command.

```bash
KEYBOARD_CONNECTED=0

while :
do
    if [[ -L "/dev/input/by-id/usb-SEMICO_USB_Gaming_Keyboard-event-kbd" ]]; then
        if [[ $KEYBOARD_CONNECTED == 0 ]]; then
            KEYBOARD_CONNECTED=1
            notify-send -t 3000 "KEYBOARD CONNECTED!"
            swaymsg input 1:1:AT_Translated_Set_2_keyboard events disabled
        fi
    else
        if [[ $KEYBOARD_CONNECTED == 1 ]]; then
            KEYBOARD_CONNECTED=0
            notify-send -t 3000 "KEYBOARD DISCONNECTED!"
            swaymsg input 1:1:AT_Translated_Set_2_keyboard events enabled
        fi
    fi

    sleep 2
done
```

Now I can use Emacs on a laptop without the drawbacks of using a shallow laptop keyboard.

The next ergo improvement might now be trying to find a better resting hand position as the new mech-keyboard is raised by an inch or so, but that will be for another ergpost...
