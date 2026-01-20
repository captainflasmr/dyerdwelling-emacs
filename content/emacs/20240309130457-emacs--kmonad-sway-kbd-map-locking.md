---
title: "Eliminating Key Chords in Emacs under Linux with Sticky Keys"
author: ["James Dyer"]
lastmod: 2024-03-22T21:39:00+00:00
tags: ["wayland", "sway", "sticky-keys", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240309130457-emacs--kmonad-sway-kbd-map-locking.jpg"
---

I spend many hours living in emacs and a large proportion of this time is using a laptop.  Recently I have been thinking about my hands and how to protect them from any future pain or RSI.

<!--more-->

{{< figure src="/emacs/20240309130457-emacs--kmonad-sway-kbd-map-locking.jpg" width="100%" >}}

Emacs out of the box can be a little, lets say ~~tortuous~~ awkward.  For example I've always found the default keybindings for switching windows (\`C-x o\`) and saving buffers (\`C-x C-s\`) to be particularly uncomfortable, a feeling exacerbated by their frequent use. Also, key presses in Emacs typically involve key chording, this means that not only do you have to contort your hand into awkward positions, but you also have to press keys simultaneously thus reducing the possibility for natural hand movement during key activation.

The concept of C-x, especially for a Control key not mapped to the Caps Lock can really take its toll over a number of years; as of yet I am fortunate not to have yet experienced any hand pain but from what I've read, it's not uncommon for discomfort to start to set in after many years of daily keyboard use, so I really want to take measures now.

In a previous post I had started to discuss this topic :

[Sticky Keys on Sway using kmonad]({{< ref
"/emacs/20240308115556-emacs--Sticky-Keys-on-Sway-using-kmonad.md" >}})

I attempted to use the cross platform tool **kmonad** to apply Sticky Key functionality thus obviating the need for multiple key chord activation in emacs.  For the most part a basic latched sticky concept works well and is easy to set up, however when using emacs I think a **locking** sticky modifier would be a much better fit, which is where a modifier will be locked on a double tap and then unlocked with a single tap.

For instance, if I could lock the Control key, navigating a buffer could be done with single key presses using 'n', 'p', 'b', and 'f'. Deleting multiple lines, a task I frequently do, would then require only individual presses of the 'k' key. To mark a selection, I could simply tap the space-bar and then use single navigation keys to expand the selection. A single tap of the Control key would then deactivate the lock.

Unfortunately **kmonad** doesn't yet support this kind of sticky key locking functionality, however my attempts at finding a workaround led me to stumble into the murky arcane world of the x keyboard extension!

An XKB (X Keyboard Extension) file format is essentially a descriptive language that allows you to define the behaviour of the keyboard on linux.  This includes the mapping of physical keys to symbols (characters or functions), modifying the action of keys depending on other keys that are held down (modifiers like Shift, Ctrl, Alt) and was primarily developed for the X11 windowing system.

I run a Wayland compositor called **Sway** as my daily driver and it just so happens that Wayland is compatible with the x keyboard extension standard!

Below are some instructions on how to set up Sway with Sticky keys to help eliminate the need for key chords in emacs.  These instructions have been applied to the Sticky Keys section in the emacs wiki : <https://www.emacswiki.org/emacs/StickyModifiers#h5o-8>

Note that the following instructions can also work for X11 desktop environments as the x keyboard extension has been the de facto standard for many years on X11 and in fact I have successfully set up Sticky Keys in the same manner for the i3 window manager.

---

**Setting up Sticky Keys through XKB**

Firstly run the bash script below to create two sticky keyboard variant files:

```bash
#!/bin/bash
DST=$HOME/.config

# Reset keyboard layout (to your preferred language)
setxkbmap gb

# Apply sticky modifiers to a file
xkbcomp $DISPLAY -xkb - | \
    sed 's|SetMods|LatchMods|g' > \
        $DST/keymap_with_sticky_modifiers.xkb

# Reset keyboard layout (to your preferred language)
setxkbmap gb

# Apply locked modifiers to a file
xkbcomp $DISPLAY -xkb - | \
    sed 's|SetMods|LatchMods|g' | \
    sed 's|,clearLocks);|,clearLocks,latchToLock);|g' > \
        $DST/keymap_with_locked_modifiers.xkb
```

-   keymap_with_sticky_modifiers.xkb - a latched sticky modifier setup in which a modifier is activated by a following key press.

-   keymap_with_locked_modifiers.xkb - a locking sticky modifier setup in which a modifier will be locked on a double tap and then unlocked with a single tap.

At this point you can activate Sticky Keys by loading the new xkb keymap in the Sway config as follows:

```nil
input "type:keyboard" {
    xkb_file $HOME/.config/keymap_with_locked_modifiers.xkb
}
```

Note that for X11 you can use the xkbcomp command as thus:

```bash
xkbcomp $HOME/.config/keymap_with_locked_modifiers.xkb $DISPLAY
```

Optionally modify the xkb file directly after generation to use the “Caps Lock” keyboard indicator LED for indicating a sticky modifier is active, like so :

```nil
indicator "Caps Lock" {
    !allowExplicit;
    whichModState= locked;
    modifiers= Control+Mod1+Shift;
};
```

This turns on the Caps Lock LED whenever a Control, Alt (Mod1) or Shift key is sticky locked giving a visual hint to any locked keys.  Although useful on a laptop if you are using a full sized keyboard each modifier can be linked to its own indicator LED, for example:

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

As part of this keyboard redefinition it might also be useful to remap the Caps Lock key to the Control modifier.  One way to achieve this in Sway is to modify the config file as follows:

```nil
input type:keyboard {
   xkb_file $HOME/.config/keymap_with_locked_modifiers.xkb
   xkb_options ctrl:nocaps
}
```

However, as we are redefining our keyboard layout we can directly modify the xkb configuration file in the following manner:

replace:

```nil
    key <CAPS> {         [       Caps_Lock ] };
```

with

```nil
    key <CAPS> {         [       Control_L ] };
```

replace:

```nil
    modifier_map Lock { <CAPS> };
```

with

```nil
    modifier_map Control { <CAPS> };
```

For bonus points, and to ensure that the right Alt modifier functions like a regular Alt thereby distributing the keys more evenly between the left and right hands, perform the following changes:

add:

```nil
    modifier_map Mod1 { <RALT> };
```

set the definition of key &lt;RALT&gt; to:

```nil
    key <RALT> {         [           Alt_L,          Meta_L ] };
```

---

With this setup, I can now effectively press single keys in succession to trigger functionalities in Emacs that would normally require key chording. Even pressing C-g has become naturally easier; I'm now pressing Ctrl, then not having to stretch my index finger across at the same time as my pinky is now released from the control key.
