---
title: "btrfs-not-running-fix"
author: ["James Dyer"]
lastmod: 2024-05-22T15:29:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240522152904-emacs--btrfs-not-running-fix.jpg"
---

btrfs-assistant does not run up and generates the following:

<!--more-->

```nil
Failed to create wl_display (No such file or directory)
qt.qpa.plugin: Could not load the Qt platform plugin "wayland" in "" even though it was found.
This application failed to start because no Qt platform plugin could be initialized. Reinstalling the application may fix this problem.

Available platform plugins are: wayland-egl, wayland, eglfs, linuxfb, minimal, minimalegl, offscreen, vkkhrdisplay, vnc, xcb.

/usr/bin/btrfs-assistant: line 46: 94391 Aborted                 (core dumped) btrfs-assistant-bin ${params}
```

It can't connect to the Wayland display socket file. This is found with the WAYLAND_DISPLAY and XDG_RUNTIME_DIR environment variables.

For some reason, their btrfs-assistant script is doing some weird things with XDG_RUNTIME_DIR. Try running the binary directly with sudo -E btrfs-assistant-bin.
