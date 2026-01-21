---
title: "grub-repair-on-linux-garuda-boot-repair"
author: ["James Dyer"]
lastmod: 2024-10-22T23:58:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20241022235848-emacs--grub-repair-on-linux-garuda-boot-repair.jpg"
---

When the following error message is presented on boot:

```nil
error: no such partition.
Entering rescue mode...
grub rescue>
```

Option should be off the main screen when on a live disk, called boot-repair

Install garuda-boot-repair and follow the instructions.

Choose the first partition on the main disk /boot/efi e.t.c

Install by selecting ESP

This will reset the grub but only for linux

To reset and pick up windows, install grub-customizer, uninstall the garuda boot packages and simply just run up.  It will pick up the windows partition and then just select Save.

Now reboot!
