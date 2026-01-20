---
title: "Transitioning RAlt to Ctrl - Enhancing Emacs Keybindings"
author: ["James Dyer"]
lastmod: 2024-04-20T10:12:00+01:00
tags: ["sticky-keys", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240417211640-emacs--Transitioning-RAlt-to-Ctrl-Enhancing-Emacs-Keybindings.jpg"
---

In the midst of adjusting my Emacs keybindings for a more ergonomic workflow, I've experimented with various configurations.  For example, I duplicated the right Alt key (RAlt) to function as an additional Alt key allowing for versatile key presses across the keyboard based on command keybinding locations.

<!--more-->

{{< figure src="/emacs/20240417211640-emacs--Transitioning-RAlt-to-Ctrl-Enhancing-Emacs-Keybindings.jpg" width="100%" >}}

The RAlt key is certainly accessible with a little curl under of the right thumb and a modifier key activator I hadn't even considered before.

For instance, I found combinations like M-n and M-p already comfortable with a left hand / right hand split, but with RAlt for activating Meta/Alt a right hand / left hand split became possible for certain commands, notably M-f and M-w which I often use.  This theoretically already felt a more comfortable setup.

However, after a few months, I realized I couldn't quite find a way to transition to the RAlt remapping; I simply wasn't using it. Yet, the initial experiment hinted at the potential of shifting modifier load to the right hand, especially leveraging the strong thumb for ergonomic advantages.

Then it struck me: what if I map RAlt to Ctrl?, I immediately questioned why I hadn't tried it earlier!

The transition so far has unveiled the following (note: I may have got carried away with the detail here, but in a way its fun to weigh up the pros and cons!):

**Benefits**

-   Splitting common prefixes like C-x and C-c between hands.
-   Enhanced comfort in isearch with split hand C-r and C-s.
-   Improved buffer window navigation, with RLR (Right Left Right) pattern for C-x o.
-   Simplified buffer window splits with C-x &lt;num&gt;.  Although there are typically two consecutive left hand key activations, sticky keys allows just two individual key taps of x then &lt;num&gt;.
-   Streamlined actions like the common C-c C-c, with the option of holding RAlt allowing easy left-hand tapping of c, c
-   Reassigned C-o (never used) to the (other-window) so I can replace my ever fluctuating window keybinding navigation to something more simple using just one hand, plus I can hold down the Ralt and tap through the windows.
-   C-g was always a bit of stretch - especially before sticky keys, but now I have a RL combination.
-   Simplified Emacs exit with C-x C-c (although why would I ever need this?!) holding RAlt for x,c tap tap.
-   A locked sticky key Ctrl double tap now doesn't activate a double whammy of left hand pinky lateral movement.
-   Saving can be easier, C-x C-s again is a right hand modifier hold with an x,s tap
-   Another common command (which in fact I had to rebind as it was too taxing on my left hand) is now much more comfortable, namely C-x C-e (eval-last-sexp) in the same way as saving mentioned above.
-   I am finding myself instinctively using C-j a lot more as naturally my right thumb is now very close to RAlt and my right index finger is always on j
-   Reverting buffers is much easier, It is bound to C-x x g (revert-buffer-quick)
-   I am finding myself starting to revert back to the vanilla Emacs keybindings rather than my adapted ones mainly I suspect that C-x and C-c is much easier now to access.

**Negatives**

-   I will have to adjust to the new position of the keybindings, and some like the undo with C-/ feed a bit strange due to the close proximity between Ralt and /
-   Keyboard configuration is required to map Ralt to Control.

I'll evaluate this setup's efficacy over the next few months and I may contemplate not just the possibility of minimizing Ctrl key usage on the left hand but potentially eliminating it altogether! My left hand pinky would then never have to attempt lateral movement again!
