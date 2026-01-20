---
title: "Waybar Toggling Sticky Key Keymaps"
author: ["James Dyer"]
lastmod: 2024-04-14T11:55:00+01:00
tags: ["sway", "sticky-keys", "krita", "emacs", "elisp", "bash", "art", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2024-04-14-10-55-03.jpg"
---

For my previous post I was talking about a software visual indicator to discern which key has been locked in a sticky key situation.  For example there are typically two modes of stickiness, being latched and locked, by default I had set up the locked variant as I thought it would be more useful for Emacs and in fact I have found this to be the case.  For example, double tapping the Control key allows nice easy single key navigation via 'n' 'p' 'f' and 'b', possible page down with 'v' and to delete lines I can use 'k' .e.t.c.

<!--more-->

{{< figure src="/emacs/2024-04-14-10-55-03.jpg" width="100%" >}}

I'm sure I will find more bonuses in Emacs with this key locking mechanism and of course I can always tweak my keybindings, for example maybe a page up could be a Ctrl-&lt;key&gt; so I can have even more power navigating with a single &lt;key&gt;.

But how does a locked key setup work in a different program? and especially the software I use for my digital art, namely Krita.  I have a USB numpad peripheral in which I map each key to a Krita shortcut, for example common actions such as colour picking, brush resize, canvas flip, undo / redo e.t.c.  I have found this to actually be a nice cheaper option than one of the more specialised digital art controllers and more flexible as I will always be able to use it on linux with keymapping software like kmonad or kanata.

It didn't take long to find out that I was running into difficulties with Krita and the locked sticky setup.  For example colour picking is quite a common activity in digital art and it just so happens that for Krita it is bound by default to the Ctrl key!  In addition Shift resizes the brush too.  I found that quite often I was tapping the Ctrl and or Shift consecutively which was locking the modifier key which would have some unwanted side effects and would start to get frustrating as I fumble around to unlock the relevant modifier thus disrupting my flow.  I quickly came to the conclusion that a locked sticky was not viable for my use within Krita.

So how could I craft a solution to this problem?

In my previous post, I might have unintentionally already laid the groundwork as I have created both a latched and a locked sticky key variant in the xkb file format. Perhaps I could develop a toggling mechanism between the locked and latched states, switching to latched state when I use Krita.

Just like before, I think Waybar would be ideal to perform the toggle with a click (which could also be a stylus click from a pen) and to then display the current status.

Initially I created the following bash script to perform the toggling of the keymaps :

```bash
#!/bin/bash

# Define the paths to your keymap files
KEYMAP_SWAY=~/.config/keymap_sway.xkb
KEYMAP_LOCKED=~/.config/keymap_with_locked_modifiers.xkb
KEYMAP_STICKY=~/.config/keymap_with_sticky_modifiers.xkb
CURRENT_KEYMAP_PATH=~/.config/keymap_current

# Check if the current keymap is set, if not use the sway keymap
if [[ ! -f "$CURRENT_KEYMAP_PATH" ]]; then
    echo "$KEYMAP_SWAY" > "$CURRENT_KEYMAP_PATH"
fi

CURRENT_KEYMAP=$(cat "$CURRENT_KEYMAP_PATH")

# Swap the keymaps
if [[ "$CURRENT_KEYMAP" = "$KEYMAP_LOCKED" ]]; then
    cp -f "$KEYMAP_STICKY" "$KEYMAP_SWAY"
    echo "$KEYMAP_STICKY" > "$CURRENT_KEYMAP_PATH"
else
    cp -f "$KEYMAP_LOCKED" "$KEYMAP_SWAY"
    echo "$KEYMAP_LOCKED" > "$CURRENT_KEYMAP_PATH"
fi

# Reload Sway configuration
swaymsg reload
```

Note that at first I tried to use the xkbcomp command but by all accounts only works on X11 so with wayland and in particular Sway I had to find a workaround to reload a toggled keymap - which is where swaymsg reload comes in.  The solution is a little hacky but will work and involves swapping around files on disk and then forcing a reload of the sway configuration file.

As always with toggling, the key is usually working out the current toggle state in order to know what to toggle to.  I decided to use the old fashioned method of a file on disk containing the path to the current keymap!

In the sway configuration file I can now attach a keybinding to the new toggle script :

```nil
bindsym $mod+y exec keymap-toggle.sh
```

'y' for sticky seems appropriate methinks.

Now for updating waybar to reflect the status.  In this case as in the modifier LEDs of my last post I will have to create some custom modules and link it to a content producing bash script.

A keymap_monitor.sh script is as follows:

```bash
#!/bin/bash

CURRENT_KEYMAP_PATH=~/.config/keymap_current

CURRENT_KEYMAP=$(cat "$CURRENT_KEYMAP_PATH" | grep "sticky")

if [[ $CURRENT_KEYMAP ]]; then
    echo "{\"text\": \"STICKY\", \"class\": \"active\"}"
else
    echo "{\"text\": \"LOCKED\", \"class\": \"inactive\"}"
fi
```

and the waybar module :

```nil
    "custom/togglesticky": {
        "exec": "keymap-monitor.sh",
        "interval": 1,
        "return-type": "json",
        "on-click": "keymap-toggle.sh",
    }
```

Note the return-type type is json and the echo statements in the keymap_monitor.sh script have formatted the return data in the json format meaning the waybar module can process the string effectively.  Note also a css class is set up which will link to the associated waybar css style sheet and potentially offer a nice flexible look for each mode if desired.

"on-click": "keymap-toggle.sh" will allow the toggle to be actioned and the monitor script will be called every second as per the interval setting to poll the current toggle state.

Well that should be about it!, when I now open up Krita I can just tap on the LOCKED indicator on waybar which will then display STICKY (I decided to go with STICKY rather than LATCHED)  This now indicates that I have changed the keymap, at which point double tapping any modifier keys as part of my arting shenanigans will now not lock the associated modifier and cause annoyance down the line.

{{< figure src="/emacs/20240414101304-emacs--Waybar-Toggling-Sticky-Key-Types/2024-04-14-10-55-19.jpg" width="100%" >}}

I might even eventually find some icons or my preferred method emojis to make things look more graphically pleasing, but with all these things I always initially look to get most of the way there and in a working state before I start polishing it.
