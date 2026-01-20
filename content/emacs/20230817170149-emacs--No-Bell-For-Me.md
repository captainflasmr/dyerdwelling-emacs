---
title: "No Bell For Me"
author: ["James Dyer"]
lastmod: 2023-08-17T17:01:00+01:00
tags: ["scrolling", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230817170149-emacs--No-Bell-For-Me.jpg"
---

Now I have set `(pixel-scroll-precision-mode 1)` which comes with emacs 29 this has led to an unexpected issue in that I now often get the following messages which cause a bell sound to be generated each time:

<!--more-->

{{< figure src="/emacs/20230817170149-emacs--No-Bell-For-Me.jpg" class="emacs-img" >}}

```nil
pixel-scroll-precision-scroll-up-page: Beginning of buffer [22 times]
pixel-scroll-precision-scroll-down-page: End of buffer [54 times]
```

I had turned to `(setq visible-bell t)` in the past but this generates an annoying flash each time rather than an annoying bell sound.

There is a solution to this of course and that is `(setq ring-bell-function 'ignore)` in that we are silent when a visual bell is rung, ah pure bliss 🙂🔔
