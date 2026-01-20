---
title: "Emacs 29.1 Transparency Alpha On Sway"
author: ["James Dyer"]
lastmod: 2023-08-12T15:36:00+01:00
tags: ["sway", "emacs-29", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230812141900-emacs--Emacs-29-1-Transparency-Alpha-On-Sway.jpg"
---

I have just been been going through the new features added to emacs 29.1 and have been trying out the transparency or `alpha-background` on `sway`.

<!--more-->

> "`alpha-background` controls the opacity of the text background when running on a
> composited display.".

I was keenly looking for an emacs improvement to the `swaywm` built in compositing method which in the case of emacs would make transparent the whole window including text which of course isn't ideal.

I had defined the following in my `sway` config file:

```nil
for_window [class="Emacs"] opacity 0.92
```

`0.92` seemed to be the perfect balance between wallpaper visibility and text legibility.

I am now going to compare the emacs `alpha-background` method with an equivalent `sway` emacs compositing opacity.

So for emacs I am going to set something like:

```nil
(set-frame-parameter nil 'alpha-background 70)
(add-to-list 'default-frame-alist '(alpha-background . 70))
```

See the following images for a comparison (can you guess which is which?) - I would suggest opening each image in separate browser tabs and comparing that way.

Note/Disclaimer: I chose opacity values that displayed an equivalent wallpaper visibility through emacs and as it turned out the opacity values were different which I suspect relates to the way the `sway` compositor is processing its opacity value rather than the way emacs does it but the demonstration fundamentally should still be a valid one.

{{< figure src="/emacs/20230812141900-emacs--Emacs-29-1-Transparency-Alpha-On-Sway-Old.jpg" width="100%" >}}

{{< figure src="/emacs/20230812141900-emacs--Emacs-29-1-Transparency-Alpha-On-Sway.jpg" width="100%" >}}

and here is a more detailed comparison on a selected screen region:

{{< figure src="/emacs/20230812141900-emacs--Emacs-29-1-Transparency-Alpha-On-Sway-Old-s.jpg" width="100%" >}}

{{< figure src="/emacs/20230812141900-emacs--Emacs-29-1-Transparency-Alpha-On-Sway-s.jpg" width="100%" >}}

Suffice it to say that this is a very nice improvement and certainly something I shall be using from now on.
