---
title: "Guide to Creating an Advent Calendar"
author: ["James Dyer"]
lastmod: 2023-10-18T15:50:00+01:00
tags: ["advent", 2023]
categories: ["linux", "open-source"]
draft: false
thumbnail: "/linux/20231001155909-emacs--Advent-Calendar-Creation/advent-calendar--2023-windows__work_front.jpg"
---

Here are the steps to create a new advent calendar.

<!--more-->


## template {#template}

Get the latest template from:

```nil
nas/Art/Projects/AdventCalendar/advent-calendar--template.kra
```

Copy into a new directory of the named convention:

```nil
advent-calendar--2023-windows__work.tar.gz
```


## get images {#get-images}

Get the calendar door images using a ratio of screenshot dimensions 600:420, for example in wayland `Win-S-s`:

```nil
slurp -a 600:420 -d | grim -g - ~/DCIM/Screenshots/$(date +'%Y-%m-%d-%H-%M-%S.jpg')
```

Put each image into the `img` subfolder.


## resize images {#resize-images}

The images will be of the correct ratio but not necessary the correct `600:420` size so scale up using `Upscayl` in batch mode.

Now set all image thumbnails to the following size.

`PictureScale` performs a straightforward resize

If for some reason the image is not of the correct ratio then `PictureCrop` can be used to get the middle portion to the correct ratio and then resize.

For all images in `img` run `PictureCorrect` to convert to jpg and to truncate the names.


## import images {#import-images}

Open the template `kra` file in Krita and drag and drop all image files into new layers.

Using the move tool with moving content move all images under the transparent doors.

Import the background image and tweak the calendar to taste.


## export {#export}

In Krita create a `front` and `back` layer and export to jpg images.


## print {#print}

To print open up the `front` and `back` images into GIMP as Krita currently doesn't support printing and increase the contrast and brightness using:
**Filter-&gt;Adjust-&gt;Colour Adjustment Curves**

Print using the following settings:

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_a.jpg" width="100%" >}}

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_b.jpg" width="100%" >}}

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_c.jpg" width="100%" >}}

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_d.jpg" width="100%" >}}

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_e.jpg" width="100%" >}}

{{< figure src="/linux/20231001155909-emacs--Advent-Calendar-Creation/20231001155909-emacs--Advent-Calendar-Creation_f.jpg" width="100%" >}}


## create {#create}

Now the A3+ images typically on photo paper have been printed out then first cut out the windows with a crafting knife.

Now apply a prit stick glue to the back of the cut out front windows and apply it to the back image

Trim the edges using a guillotine and stick the edges together using cello-tape.
