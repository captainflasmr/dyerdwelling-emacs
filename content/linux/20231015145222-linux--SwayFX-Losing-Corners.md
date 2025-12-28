---
title: "SwayFX Losing Corners"
author: ["James Dyer"]
lastmod: 2023-11-19T11:08:00+00:00
tags: ["swayfx", "sway", 2023]
categories: ["linux", "open-source"]
draft: false
thumbnail: "/linux/20231015145222-emacs--SwayFX-Losing-Corners.jpg"
---

I seemed to temporarily lose the rounded corners in **SwayFX** and I think the reason for this is that the gaps inner need to be a certain width, for example the following doesn't seem to work:

<!--more-->

{{< figure src="/ox-hugo/20231015145222-emacs--SwayFX-Losing-Corners.jpg" width="100%" >}}

`gaps inner 8px`

but the following does!:

`gaps inner 10px`
