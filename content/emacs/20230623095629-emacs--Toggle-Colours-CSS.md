---
title: "Cycling Colours in CSS and Other Files"
author: ["James Dyer"]
lastmod: 2023-06-23T11:04:00+01:00
tags: ["emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230623095629-emacs--Toggle-Colours-CSS.jpg"
---

I try and keep a simple colour scheme definition for my web site using some CSS variables:

<!--more-->

{{< figure src="/emacs/20230623095629-emacs--Toggle-Colours-CSS.jpg" class="emacs-img" >}}

```nil
--theme-fg: #2b2d3a;
--theme-bg: #fffbef;
--theme-alt: #ffa500;
```

`rainbow-mode` of course helps to show the colours in situ, but sometimes if I am changing the colour scheme I use `list-colors-display` and then copy and paste the hex value.  But should I use the name instead?, it is tempting as the colour would be very apparent and readable in the CSS file but then it is difficult to just incrementally tweak a hex value to get the colours to look just right.  I also like to use orange, well I always seem to revert back to it but I can never remember the hex value.

There is an in-built function in emacs which can help me out with all this called `css-cycle-color-format` and is bound to C-c C-f

With the point over the colour, potentially three different CSS color formats are cycled, by name (if possible), hexadecimal, and rgb()/rgba().

For example I could cycle through orange as thus:

```nil
--theme-alt: orange;
--theme-alt: #ffa500;
--theme-alt: rgb(255, 165, 0);
--theme-alt: orange;
```

What about a colour / hex value in a non CSS file I hear you say, well although `css-cycle-color-format` is bound to `css-mode-map` it can be called for other files and in other modes.  For example in `init.el` calling `css-cycle-color-format` seems to work so I guess it could just be rebound to any `mode-map` as desired.

Also if you have a CSS file with a profusion of hex colour values the `css-cycle-color-format` command could find the name value for you, for example as a test, putting in a colour that is obviously red :

```nil
--theme-alt: #ff0000;
```

cycles to:

```nil
--theme-alt: red;
```
