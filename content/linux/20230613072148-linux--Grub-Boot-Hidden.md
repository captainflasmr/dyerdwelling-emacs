---
title: "Grub-Boot-Hidden"
author: ["James Dyer"]
lastmod: 2023-06-13
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230613072148-emacs--Grub-Boot-Hidden.jpg"
---

/su::/etc/default/grub

GRUB_TIMEOUT_STYLE=hidden
GRUB_TIMEOUT="2"

sudo grub-mkconfig -o /boot/grub/grub.cfg

[sudo] password for jdyer:
Generating grub configuration file ...
Found theme: /boot/grub/themes/XeroKDE/theme.txt
Found linux image: /boot/vmlinuz-linux
Found initrd image: /boot/intel-ucode.img /boot/amd-ucode.img /boot/initramfs-linux.img
Found fallback initrd image(s) in /boot:  intel-ucode.img amd-ucode.img initramfs-linux-fallback.img
Warning: os-prober will be executed to detect other bootable partitions.
Its output will be used to detect bootable binaries on them and create new boot entries.
Found Windows Boot Manager on /dev/nvme0n1p1@/EFI/Microsoft/Boot/bootmgfw.efi
Adding boot menu entry for UEFI Firmware Settings ...
Found memtest86+ image: /boot/memtest86+/memtest.bin
/usr/bin/grub-probe: warning: unknown device type nvme0n1.
done
