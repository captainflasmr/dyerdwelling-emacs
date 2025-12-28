---
title: "Xero-Linux-Install"
author: ["James Dyer"]
lastmod: 2023-06-18
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230618122426-emacs--Xero-Linux-Install.jpg"
---

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [enable brightness with grub / kernel](#enable-brightness-with-grub-kernel)
- [make sure cronie is enabled](#make-sure-cronie-is-enabled)
- [enable nas](#enable-nas)
- [enable kmonad keys](#enable-kmonad-keys)
- [run ~/bin/permissions-\*](#run-bin-permissions)
- [init mu4e for emacs](#init-mu4e-for-emacs)
- [copy over MyMusicLibrary to ~](#copy-over-mymusiclibrary-to)
- [setup thunderbird](#setup-thunderbird)
- [setup syncthing](#setup-syncthing)
- [test other logins](#test-other-logins)

</div>
<!--endtoc-->

Do the usual and then:


## enable brightness with grub / kernel {#enable-brightness-with-grub-kernel}

Modify the GRUB config file sudo nano /etc/default/grub in the GRUB_CMDLINE_LINUX_DEFAULT append "i915.enable_dpcd_backlight=3" then save the file

Rebuild GRUB for it can take the change of your config file, by doing (on Arch) sudo grub-mkconfig -o /boot/grub/grub.cfg

reboot and test with the key if it works


## make sure cronie is enabled {#make-sure-cronie-is-enabled}

sudo systemctl enable cronie
sudo crontab -e / @reboot /home/jdyer/bin/startup_root.sh


## enable nas {#enable-nas}

mkdir -p ~/nas
open file manager and login to nas


## enable kmonad keys {#enable-kmonad-keys}

sudo usermod -aG input jdyer

this gives permission to access the keyboard input devices

reboot


## run ~/bin/permissions-\* {#run-bin-permissions}


## init mu4e for emacs {#init-mu4e-for-emacs}

mu init --maildir=/home/jdyer/Maildir --my-address='james@dyerdwelling.family'


## copy over MyMusicLibrary to ~ {#copy-over-mymusiclibrary-to}


## setup thunderbird {#setup-thunderbird}


## setup syncthing {#setup-syncthing}

sync to ~/DCIM


## test other logins {#test-other-logins}

in theory they should work
