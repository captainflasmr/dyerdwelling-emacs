---
title: "Waybar-Auto-Update"
author: ["James Dyer"]
lastmod: 2023-06-11
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230611161331-emacs--Waybar-Auto-Update.jpg"
---

```bash
#!/bin/bash

CONFIG_FILES="$HOME/.config/waybar/config $HOME/.config/waybar/style.css"

trap "killall waybar" EXIT

while true; do
    waybar &
    inotifywait -e create,modify $CONFIG_FILES
    killall waybar
done
```
