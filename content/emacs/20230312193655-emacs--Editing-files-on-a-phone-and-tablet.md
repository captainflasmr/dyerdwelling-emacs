---
title: "Editing org files on an Android device - Part 1"
author: ["James Dyer"]
lastmod: 2023-03-12T21:08:00+00:00
tags: ["emacs", "android", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230312193655-emacs--Editing-files-on-a-phone-and-tablet-pt1.jpg"
---

Now that there is a build for emacs on Android I thought I would try and develop an emacs workflow between my Galaxy Note 8 / Galaxy Tab S7+ and my linux laptop.

<!--more-->

At the moment I am using Markor on my phone and this serves my needs adequately but of course I am missing that org file support.

My current process involves **syncthing** to push my org files around my laptop / phone and tablet.  I also push other things that are regularly backed up and this does include my **.emacs** config file.

So the relevant files are already present on my device the next thing to do is to install emacs from F-Droid and grant emacs file permissions.

My phone is running Android version 9 and my tablet version 13 so the method to get permissions set up for Emacs was slightly different in each case.

On my phone from the settings I selected Apps -&gt; top right menu (App permissions) -&gt; Storage -&gt; Emacs to On

On my tablet from the settings I selected Apps -&gt; top right menu (Special access) -&gt; All files access -&gt; Emacs to On

Next up is to set up my configuration file, in this case I am going to create a symbolic link using `dired`.  First of course is to run up emacs and then navigate to my syncthing'd `.emacs` file which in my case is in the following location:

```bash
/storage/emulated/0/DCIM/Linux
```

The android emacs menu bar is quite responsive and the menu appears in a nice large touch friendly font so I can just **File -&gt; Open Directory...** at which point an on screen keyboard pops up and I can input the path.

Note : Something I discovered early on was that because I have KDE Connect installed across all my devices and Clipboard sync is turned on it means that the clipboard is shared between devices.  This means that anything that is copied from within emacs on my laptop appears on my phone or tablet, so I could just type out the path on my laptop, copy it to the kill ring and it appears on the Samsung Keyboard clipboard field ready to paste into emacs!

Now the path has been input `dired` shows the directory containing all my files including my `.emacs` file.  Now I select the line containing the file **(yes, using my finger!!)** then from the top menu : **Operate -&gt; Simlink to...** and input the following:

```bash
/data/data/org.gnu.emacs/files/.emacs
```

Now restart emacs or maybe **Emacs-Lisp -&gt; Evaluate Buffer**

When I first attempted this my default `.emacs` gave a prodigious number of errors and I realised that for the moment android emacs doesn't have the ability to download packages from melpa or elpa. Not too much of a problem for me though as my default config is tending towards vanilla anyway so for the moment I decided to create a purely android version of my .emacs config by stripping out everything I didn't think I needed with an eye on creating a generic version across not only android but windows too.  Currently I have a few `(when (string-equal system-type "windows-nt")` and for android I just need something similar but with **android** as the defining string.

Technically just setting the storage permissions for emacs on my device would have been enough to open an org file and edit it, but now I have a symbolic link to my .emacs config and it is synched to all my devices it means I now have the ability to modify it anywhere and although I currently have an android specific version it won't be too long before I have a single generic config for all my devices / operating systems.

The next step will be to find an on-screen keyboard to fit a good emacs editing workflow - a sneak peek below of Keyboard Designer but that will be for part 2!

{{< figure src="/emacs/20230312193655-emacs--Editing-files-on-a-phone-and-tablet-pt1.jpg" width="300px" >}}
