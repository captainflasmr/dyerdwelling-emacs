---
title: "How To Map RAlt to Ctrl for Emacs"
author: ["James Dyer"]
lastmod: 2024-05-04T15:30:00+01:00
tags: ["xkb", "wayland", "sway", "sticky-keys", "plantuml", "kmonad", "emacs", "autohotkey", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240425213402-emacs--How-To-Map-Ralt-to-Ctrl-puml.jpg"
---

In a recent post I was talking about the benefits of mapping the RAlt key to the Ctrl key and this set up is so far still feeling very comfortable.

<!--more-->

However, there are many ways to set up the mapping, so below are instructions on how to map the right Alt key (RAlt) to Ctrl for different platforms. These are just the methods I have used in the past, I'm sure there are a multitude of other options out there.

---

<div class="ox-hugo-toc toc local">

- [Linux](#linux)
- [Windows](#windows)
- [Linux or Windows](#linux-or-windows)

</div>
<!--endtoc-->

---

Before you dive in, here is a rough plantuml diagram to give you a top level diagrammatical overview of what I am describing below (note: kmonad is not included):

<a id="code-snippet--workflow1"></a>
```plantuml
!pragma layout smetana
title: How To Map Ralt to Ctrl Activity Diagram
start
:Start;

if (OS == "Linux") then (yes)
   if (Environment == "X11 or Wayland") then (yes)
     :Generate xkb file;
     note right
       xkbcomp $DISPLAY -xkb keymap.xkb
     end note

     :Modify keymap.xkb;
     note right
       Add modifier_map and key definitions
     end note

     :Load modified file;
     note right
       xkbcomp keymap.xkb $DISPLAY
     end note

     :For persistence,\nadd to startup applications or\n.bashrc/.profile;
   endif

   if (Environment == "X11") then (yes)
     :Choose Tool;
     if (Tool == "xmodmap") then (yes)
       :Create/Edit .Xmodmap;
       :Add keycode modification;
       :Apply changes;
       note right
         xmodmap ~/.Xmodmap
       end note
     else (no)
       :Run setxkbmap command;
       note right
         setxkbmap -option ctrl:ralt_rctrl
       end note
     endif
     :For persistence,\nadd to startup applications or\n.bashrc/.profile;
   endif

   if (Environment == "Wayland TWM") then (yes)
     :Modify sway/hyprland config;
     note right
       Depending on the window manager, apply the necessary `input` configurations.
     end note
     :For persistence, ensure\nconfig is loaded on login;
   endif
 else (no)
   :Install AutoHotkey;
   :Create and Edit Script;
   :Run Script;
   note right
     Script contains key remappings like RAlt::Ctrl
   end note
   :For persistence, place in "Startup" folder;
 endif

 stop
```


## Linux {#linux}


### X11 or Wayland {#x11-or-wayland}


#### xkb (X Keyboard Extension) {#xkb--x-keyboard-extension}

First generate an xkb file which defines all keybindings for the current layout by running the follwing command:

```text
xkbcomp $DISPLAY -xkb keymap.xkb
```

Add to the keymap.xkb file along with the other defined modifier_maps:

```text
modifier_map Mod1 { <RALT> };
```

and set the definition of key to:

```text
key <RALT> { [ Alt_L, Meta_L ] };
```

Now load the modified file back in again:

```text
xkbcomp keymap.xkb $DISPLAY
```

If you want it to be applied every time you log in, you can add this command to your startup applications or add it to your .bashrc or .profile file.


### X11 only {#x11-only}


#### xmodmap {#xmodmap}

Xmodmap is a little archaic these days and is seen as being a little deprecated and soon to become obsolete (especially with the incoming Wayland protocol) but especially on X11 it can still be used if you don't want to fiddle around with xkb files.

To map the right Alt key (RAlt) to Ctrl using xmodmap in Linux, you can follow these steps:

Create a file named .Xmodmap in your home directory if it doesn't exist already.

Add the following line to the file:

```text
keycode <keycode_of_RAlt> = Control_L
```

Replace with the keycode of your right Alt key.

To find the keycode, you can use the xev command. Open a terminal and run:

xev

This will open a small window. Move your cursor into that window and press the right Alt key. Look for the keycode in the terminal output. It should look something like keycode 108 (maybe exactly like!)

To apply the changes, run:

```text
xmodmap ~/.Xmodmap
```

Now your right Alt key should behave like the Ctrl key. Keep in mind that this mapping will only persist for your current session. If you want it to be applied every time you log in, you can add the xmodmap ~/.Xmodmap command to your startup applications or add it to your .bashrc or .profile file.


#### setxkbmap {#setxkbmap}

Simply run the following command:

```text
setxkbmap -option ctrl:ralt_rctrl
```

If you want it to be applied every time you log in, you can add this command to your startup applications or add it to your .bashrc or .profile file.


### Wayland Tiling Window Managers {#wayland-tiling-window-managers}


#### Sway {#sway}

Perform the xkb steps defined above and then modify the sway config file as follows:

```text
input type:keyboard {
   xkb_file keymap.xkb
}
```

alternatively modify the sway config file as follow:

```text
input type:keyboard {
    xkb_options ctrl:ralt_rctrl
}
```

As this is modifying the basic config file it will be guaranteed to be loaded every time you log in.


#### Hyprland {#hyprland}

As the xkb steps above and then modify the hyprland config file as follows:

```text
input {
   kb_file = keymap.xkb
}
```

alternatively modify the hyprland config file as follow:

```text
input {
   kb_options = caps:ctrl_modifier
}
```

As this is modifying the basic config file it will be guaranteed to be loaded every time you log in.

---


## Windows {#windows}


### AutoHotKey {#autohotkey}

Install AutoHotkey if you haven't already. You can download it from the official website: <https://www.autohotkey.com/>

Once AutoHotkey is installed, right-click on your desktop or in a folder, hover over "New," and select "AutoHotkey Script." Name the script whatever you like, for example, RAlt_to_Ctrl.ahk.

Right-click the newly created script file and select "Edit Script" to open it in your default text editor.

Add the following line to the script:

```text
RAlt::Ctrl
```

Save the script and close the text editor.

Double-click the script file to run it. You should see a green "H" icon in the system tray indicating that the script is running.

Now, whenever you press the right Alt key, it will function as the Ctrl key. To stop the remapping, right-click the AutoHotkey icon in the system tray and select "Exit."

This remapping will persist until you close the script or disable AutoHotkey. If you want the remapping to be applied automatically every time you start your computer, you can place a shortcut to the script in the "Startup" folder of your Start Menu.

---


## Linux or Windows {#linux-or-windows}


### kmonad {#kmonad}

Install kmonad if you haven't already. You can find installation
instructions on the GitHub repository: <https://github.com/kmonad/kmonad>

Once kmonad is installed, create a configuration file called kmonad.hs


#### linux {#linux}

Add the following lines :

```text
(defcfg
  input  (device-file "/dev/input/by-path/<device>")
  output (uinput-sink "My KMonad output")

  ;; Comment this if you want unhandled events not to be emitted
  fallthrough true

  ;; Set this to false to disable any command-execution in KMonad
  allow-cmd true
  )

(defsrc ralt)

(deflayer default lctrl)
```

Now on the command line run up:

```text
kmonad kmonad.hs
```

To ensure that the mapping persists place the command to your startup
applications or add it to your .bashrc or .profile file.


#### windows {#windows}

```text
(defcfg
  input (low-level-hook)
  output (send-event-sink)
  fallthrough true)

(defsrc ralt)

(deflayer default lctrl)
```

Now create a batch script called kmonad.bat and add:

```text
kmonad.exe kmonad.hs
```

This remapping will persist until you close kmonad.exe. If you want the
remapping to be applied automatically every time you start your
computer, you can place a shortcut to the script in the "Startup" folder
of your Start Menu.

---

This post may end up turning into some kind of living document when I find out other ways to perform the mapping, for example I might start to look at Kanata and also see if PowerToys can perform this mapping on Windows.
