---
title: "Porting Dolphin Context Sensitive Scripts To Thunar"
author: ["James Dyer"]
lastmod: 2023-07-10T20:03:00+01:00
tags: ["thunar", "org", "macros", "linux", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230709144332-emacs--Porting-Dolphin-Scripts-To-Thunar.jpg"
---

I have decided to switch my linux file manager from Dolphin to Thunar and this means transferring the context sensitive menus I have set up through KDE Plasma to using Thunar's custom actions.

<!--more-->

{{< figure src="/emacs/20230709144332-emacs--Porting-Dolphin-Scripts-To-Thunar.jpg" class="emacs-img" >}}

I quickly figured out that I can just modify `$HOME/.config/Thunar/uca.xml` to add in new Thunar actions, so all I need to do is to firstly work out how to perform the translation from Dolphin to Thunar actions and then how to set up emacs in such a way that would get me to a quick solution.

Dolphin relies on files within `$HOME/.local/share/kservices5/ServiceMenus` to define the context menus and I have separate files for each MIME type:

-   AudioMenu.desktop
-   PictureMenu.desktop
-   VideoMenu.desktop

Each one has an actions list near the top defining a link to multiple script command definitions throughout the file.  So I think all I need to do is to extract out the actions, for example :

```nil
Actions=PictureConvert;PictureInfo;PictureRotateLeft;PictureRotateRight;PictureScale;PictureAutoColour;PictureCrush;PictureMontage;PictureRotateFlip;PictureUpscale;PictureGetText;PictureOrientation;PictureCorrect;Picture2pdf
```

and apply them to the `uca.xml` file using an example I had already created as a prototype :

```nil
<action>
   <icon></icon>
   <name>PictureConvert</name>
   <submenu></submenu>
   <unique-id>1688724015024216-1</unique-id>
   <command>ServiceConsole PictureConvert %F</command>
   <description>PictureConvert</description>
   <range>*</range>
   <patterns>*</patterns>
   <image-files/>
</action>
```

The `ServiceConsole` script is just a wrapper around each bash script and lets me choose the terminal, shell and anything else I would like to wrap around:

```bash
#! /bin/bash
konsole -e "/bin/bash -c '$*; echo; echo "Finished"; read input'"
```

Now I know what needs to be done I think I will create a window in emacs on the left containing the Thunar custom action file (`uca.xml`) and `*scratch*` on the right and then define a macro to step through each action and apply the script names to the `uca.xml` file.

I can now extract the `Actions` list from each file, paste it into the scratch buffer and then `forward-word` copy to the kill ring in turn and for each script name copy and paste to a new copy of the xml example template.

The only other aspect I need to pay attention to is the transfer of the MIME types to `<image-files/> <audio-files/> <video-files/>`

There is a `<unique-id>` set for each action but I think that is more of an internal Thunar Id and I could just copy and paste the existing Id as part of my macro and it still works.

So for a task that I thought would take quite a while and would be convoluted in the end turned out to be pretty trivial and this was mainly due to at first spending some time working out how to translate the configuration files (with a little prototyping) and then applying the transformation using emacs.

These context sensitive menu bash scripts transforming image, video and audio files can also be run off the command line and I can run them in emacs using the **dwim-shell-command** package. This means that I now have a consistent media transformation set of tools across all my file managers!

I will go into more detail on my use of **dwim-shell-command** in a future blog post ;)
