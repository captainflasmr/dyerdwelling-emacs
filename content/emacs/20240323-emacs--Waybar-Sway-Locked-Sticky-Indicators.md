---
title: "Waybar Sticky Key LED indicators on a Laptop"
author: ["James Dyer"]
lastmod: 2024-03-30T16:43:00+00:00
tags: ["wayland", "waybar", "sway", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2024-03-30-10-27-49-banner.jpg"
---

From my previous post regarding setting up sticky keys mainly for **Control**, **Alt** and **Shift** in Emacs to remove the dependence on key chording I just wanted to scratch another itch, and that was to provide a modifier LED indicator type experience on a laptop that you would commonly get on a full size keyboard.

<!--more-->

{{< figure src="/emacs/2024-03-30-10-27-49-banner.jpg" width="100%" >}}

For example most laptops typically have a single LED indicator on the Caps Lock key with a full size keyboard having Num Lock, Caps Lock and Scroll Lock specific LEDs and generally in that order.

How would I accomplish this more granular display on a laptop that only has a single Caps Lock indicator LED?.

My previous solution of activating the Caps Lock LED when any modifier was activated works to an extent.  However, you still don't know which specific modifier is active, so to clear a locked modifier you might still have to go hunting around the modifiers to find out which one has been activated.

This so far is the only negative I have found using Sticky Keys in that occasionally a modifier key becomes accidentally locked (I suppose stuck!) and it takes a while to figure out which one.

In a highly configurable environment like Sway where you pretty much have to build most things for yourself the flexibility offered can allow me to craft a software solution by modifying the bar to visually indicate the current locked modifiers. So when stickies get stuck a quick glance will tell you which modifiers are active and hence which ones to deactivate.

For now I will just focus on **waybar** which is a customizable bar for Wayland compositors although setting up something like Polybar for X11 would be very similar.

I think the way to achieve this is to figure out how to access the current state of LED indicators which signify which sticky modifiers are set.  Currently any modifier on my laptop activates the Caps Lock LED indicator and although specifically a hardware LED, each modifier can be stored separately, and through xkb (X Keyboard Extension) we can store each LED state in the following locations if set up correctly:

```bash
/sys/class/leds/input2::capslock
/sys/class/leds/input2::numlock
/sys/class/leds/input2::scrolllock
```

My first step is to split the modifiers, setting each internal device accordingly in **/sys/class/leds**, so the original xkb file will change from:

```nil
indicator "Caps Lock" {
    !allowExplicit;
    whichModState= locked;
    modifiers= Control+Mod1+Shift;
};
```

to

```nil
indicator "Caps Lock" {
    !allowExplicit;
    whichModState= locked;
    modifiers= Control;
};
indicator "Num Lock" {
    !allowExplicit;
    whichModState= locked;
    modifiers= Mod1;
};
indicator "Scroll Lock" {
    whichModState= locked;
    modifiers= Shift;
};
```

which is the way I would activate each LED for a full size keyboard.

Next is to access each LED value so I can pass it through to waybar for display.  I will have to access the LED states more directly, hence in linux pulling values straight from a directory called /sys/class/leds

The following bash script will pass out a JSON formatted string which waybar will be able to process in a custom module:

```bash
#!/bin/bash

# Define the paths to the LED brightness indicators
caps_lock_led="/sys/class/leds/input2::capslock/brightness"
num_lock_led="/sys/class/leds/input2::numlock/brightness"
scroll_lock_led="/sys/class/leds/input2::scrolllock/brightness"

# Function to output the JSON format for Waybar
output_json() {
    local text="$1"
    local active="$2"
    if [ "$active" = "1" ]; then
        echo "{\"text\": \"$text\", \"class\": \"active\"}"
    else
        echo "{\"text\": \"$text\", \"class\": \"inactive\"}"
    fi
}

# Check the command-line argument and output the respective LED status
case "$1" in
    --caps)
        caps_state=$(cat "$caps_lock_led")
        output_json "Ctl" "$caps_state"
        ;;
    --num)
        num_state=$(cat "$num_lock_led")
        output_json "Alt" "$num_state"
        ;;
    --scroll)
        scroll_state=$(cat "$scroll_lock_led")
        output_json "Sft" "$scroll_state"
        ;;
    *)
        echo "Usage: $0 [--caps | --num | --scroll]"
        exit 1
        ;;
esac
```

Each LED is associated with its own device, and conveniently, the brightness file is simply a text file containing either a 0 or 1 to indicate whether the LED is active or inactive. To facilitate integration with custom modules in Waybar, and to link them into its CSS I will need to output each module as JSON, assigning each key modifier a class set to either 'active' or 'inactive'.

I will have to write a some custom waybar modules so in the associated modules.json :

```nil
    "custom/caps_lock": {
        "exec": "led-monitor.sh --caps",
        "interval": 1,
        "return-type": "json"
    },
    "custom/num_lock": {
        "exec": "led-monitor.sh --num",
        "interval": 1,
        "return-type": "json"
    },
    "custom/scroll_lock": {
        "exec": "led-monitor.sh --scroll",
        "interval": 1,
        "return-type": "json"
    }
```

Each button has to be a separate module with each having its own name and LED state which enables the CSS to access each LED individually.

I have kept the styling simple for now but of course the style can be anything you like, so here is an associated CSS :

```css

/* General LED styling */
#custom-caps_lock,
#custom-num_lock,
#custom-scroll_lock {
    padding: 0em 0.2em;
}

/* Active LED styling */
#custom-caps_lock.active,
#custom-num_lock.active,
#custom-scroll_lock.active {
    background-color: @recording-color;
}
```

Here I have put a general padding on the right and left and then defined a simple highlight background colour for when the JSON script above returns active.

These modules can be positioned anywhere along waybar, I typically like to have mine on the right as thus :

{{< figure src="/emacs/20240323111726-emacs--Waybar-Sway-Locked-Sticky-Indicators/2024-03-30-10-27-10.jpg" width="100%" >}}

with modifier activation of Alt and Shift shown as:

{{< figure src="/emacs/20240323111726-emacs--Waybar-Sway-Locked-Sticky-Indicators/2024-03-30-10-27-49.jpg" width="100%" >}}

So in the case above if my keys are doing weird things I can quickly glance at waybar and see that Alt and Shift are currently Sticky activated thus I can deactivate by selecting each to remove the visual indicators and carry on my merry little way!
