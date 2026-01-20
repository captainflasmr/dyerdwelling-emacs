---
title: "Run-Shotcut-In-Sway"
author: ["James Dyer"]
lastmod: 2023-08-05
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230805081618-emacs--Run-Shotcut-In-Sway.jpg"
---

When Shotcut is run up on sway it will raise the following:

```nil
This application failed to start because no Qt platform plugin could be initialized. Reinstalling the application may fix this problem.

Available platform plugins are: eglfs, linuxfb, minimal, minimalegl, offscreen, vkkhrdisplay, vnc, xcb.
```

this is because of the setting export QT_QPA_PLATFORM=wayland and should be set to QT_QPA_PLATFORM=xcb

To achieve this copy the org.shotcut.Shotcut.desktop file from _usr/share/applications to /home/jdyer_.local/share/applications and replace:

shotcut %F

with

Exec=env QT_QPA_PLATFORM=xcb shotcut %F
