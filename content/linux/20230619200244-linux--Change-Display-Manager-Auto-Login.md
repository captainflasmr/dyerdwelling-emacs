---
title: "Change-Display-Manager-Auto-Login"
author: ["James Dyer"]
lastmod: 2023-06-19
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230619200244-emacs--Change-Display-Manager-Auto-Login.jpg"
---

Modify /etc/sddm.conf.d/kde_settings.conf changing:

```nil
[Autologin]
Relogin=false
Session=plasma
User=jdyer
```

Session to:

-   plasma
-   plasmawayland
-   i3
-   sway
-   hyprland
