---
title: "ahk-mode and Updating To AutoHotKey Version 2"
author: ["James Dyer"]
lastmod: 2023-08-25T14:10:00+01:00
tags: ["kmonad", "emacs", "autohotkey", "artrage", "art", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230825115829-emacs--Updating-To-AutoHotKey-V2.jpg"
---

I have now managed to set up my external USB numeric keypad on Linux using `kmonad` which lets me use any regular plugged-in number keypad as a shortcut tool for my digital art.

<!--more-->

{{< figure src="/emacs/20230825115829-emacs--Updating-To-AutoHotKey-V2.jpg" class="emacs-img" >}}

This comes in very handy when painting in Krita and ArtRage (through Wine) and maybe one day I will expand this functionality to GIMP when version 3.0 comes out!

I would like the same keymapping functionality to be reflected on Windows (as ArtRage is fundamentally a Windows application).  Technically `kmonad` should work but I'm going to use `AutoHotKey` as I'm much more familiar with it.

I have a problem though, my old `autohotkey` scripts don't work with the latest version of `AutoHotKey` (version 2).  On further investigation it seems that _version 2_ is a significant update and includes the simplification and shift of the syntax.

Well I guess I had better update my scripts then!.

On Windows by default an `ahk` extension offers to open **notepad** which initially I did as I thought the changes would be simple enough and actually notepad isn't too bad these days what with the multiple tabs, dark mode and even multiple undos!.  However I quickly became frustrated with the lack of syntax highlighting / indenting e.t.c.  I have used a specific `ahk` editor in the past, something like `SciTE4AutoHotkey` but this time I thought I would break out `emacs` and see what it had to offer.

Initially loading the `ahk` script started fundamental mode and of course had no syntax highlighting.  Running `list-packages` and then a quick search for _AutoHotkey_ brought up the only package available, which was **`ahk-mode`**.  This package seems pretty straightforward and although it was last updated in 2016 (so won't cater too well with the AutoHotkey v2 shift in syntax) the syntax highlighting, commenting and indenting were just fine enough for me.  It also seems to be well integrated with `company-mode` and although I'm not a great fan of `company-mode` in general in this case it might actually be useful.

I was now able to comfortably convert my scripts to AutoHotKey v2, for example:

**version 1**

```nil
#IfWinActive, ahk_class ArtRage 3
NumpadDiv:: ^z
NumpadMult:: ^y
NumpadHome::Shift
NumpadUp:: Alt
NumpadLeft:: Space
NumpadRight::
GetKeyState, state, h
if state = D
    Send {h up}
else
    Send {h down}
return
NumpadEnd:: Control
NumpadPgDn:: RButton
```

**version 2**

```nil
#HotIf WinActive("ahk_class ArtRage 3")
NumpadDiv:: ^z
NumpadMult:: ^y
NumpadHome::Shift
NumpadUp:: Alt
NumpadLeft:: Space
NumpadRight::
{
if GetKeyState("h")
    Send "{h up}"
else
    Send "{h down}"
}
NumpadEnd:: Control
NumpadPgDn:: RButton
```

While I was in amongst the scripts I decided to add in something extra and that is to map the **Capslock** key to the **Control** key; a not uncommon mapping for a regular emacs user.

So I added in the following line and plonked the script into the Windows startup folder:

```nil
Capslock::Control
```
