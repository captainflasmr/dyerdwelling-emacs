---
title: "Using-GIMP-Filters-On-Command-Line"
author: ["James Dyer"]
lastmod: 2024-10-18T08:28:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20241018082803-emacs--Using-GIMP-Filters-On-Command-Line.jpg"
---

When using filters on the command line in GIMP, for example in my auto colour script PictureAutoColour firstly a script needs to be defined in:

~/.config/GIMP/2.10/scripts/batch-gimp.scm

```elisp
(define (batch-gimp pattern
                radius
                amount
                threshold)
  (let* ((filelist (cadr (file-glob pattern 1))))
   (while (not (null? filelist))
      (let* ((filename (car filelist))
          (image (car (gimp-file-load RUN-NONINTERACTIVE
                        filename filename)))
          (drawable (car (gimp-image-get-active-layer image))))
       (gimp-levels-stretch drawable)
       (gimp-file-save RUN-NONINTERACTIVE
               image drawable filename filename)
       (gimp-image-delete image))
      (set! filelist (cdr filelist)))))
```

and then called from the script as follows:

```nil
GIMPCMD="gimp -i -b '(batch-gimp \"$A\" 5.0 1.0 0)' -b '(gimp-quit 0)'"
```

This calls the GIMP internal filters.

There is also the option of using gmic and gegl filters:

and even more interestingly there is DarkTable!, just create a new style with edits and it will be in :

```nil
/home/jdyer/.config/darktable/styles
```
