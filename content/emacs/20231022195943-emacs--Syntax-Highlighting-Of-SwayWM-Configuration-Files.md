---
title: "Better Syntax Highlighting Sway Configuration Files"
author: ["James Dyer"]
lastmod: 2023-10-22T21:20:00+01:00
tags: ["sway", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2023-10-22-20-32-51_t.jpg"
---

I have been delving into the nuts and bolts of the **Sway window manager** lately and especially its fork **SwayFX** which adds a little eye candy in the style of **Hyprland**.

<!--more-->

This has led to lots of sway config file editing and hence trying to work out the best way in emacs to work with them.

By default a typical sway/config file has its own configuration format and emacs tries its best to figure out which best mode to use and settles on `conf-space-mode`.  This is a basic starter mode for a config that is space separated and gives me a rudimentary level of highlighting, for example :

{{< figure src="/emacs/20231022195943-emacs--Syntax-Highlighting-Of-SwayWM-Configuration-Files/2023-10-22-20-14-13.jpg" width="100%" >}}

I would like indentation to be a little more intelligent and currently this mode applies an ever increasing indent but I shall figure that out at a later date.

I was generally not unhappy with `conf-space-mode` as it gave me a rudimentary keyword form of highlighting and typically as a sway configuration file doesn't need to be particularly structured I wasn't ever really indenting anyway, but lets see if I can improve the syntax highlighting.

The first port of call is `list-packages` and searching for `sway`:

```nil
  sway - Communication with the Sway window manager
  sway-lang-mode - Major mode for sway
```

Unfortunately both don't suit my use case and in fact the second is for the sway programming language of which this is not!.

Well how about `treesitter`?, a big hard nope for this as well.

Next up is a little ace up my sleeve and one that continues to prove advantageous when working with the Sway tiling manager.  It is the fact that SwayWM is a drop in replacement for i3wm.  Well if the sway config files are of the same format as i3 config files then surely I can use some kind of i3 emacs mode? and so it proved to be the case!

```nil
  i3wm-config-mode - Better syntax highlighting for i3wm's config file
```

Explicitly running this mode gives me:

{{< figure src="/emacs/20231022195943-emacs--Syntax-Highlighting-Of-SwayWM-Configuration-Files/2023-10-22-20-32-51.jpg" width="100%" >}}

Much nicer! and comes with the following `auto-mode-alist` definition:

```elisp
("i3/config\\'" . i3wm-config-mode)
```

which of course requires a little adjustment to auto associate this new i3 mode with my sway files.  I came up with the following:

```elisp
(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))
```

It took me a little while to figure out the `regex` as there are a few subtleties, for example it is recommended to always use **\\\\'** rather than **$** to eliminate the confusion around new lines.

Note that I put an extra directory slash at the regex start to stop the mode being applied to any other potential directories that might end in **sway** which I feel might be a more common future occurrence than a config directory containing **i3**.
