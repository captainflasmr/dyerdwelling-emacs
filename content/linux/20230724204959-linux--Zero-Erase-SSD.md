---
title: "Zero-Erase-SSD"
author: ["James Dyer"]
lastmod: 2023-07-24
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230724204959-emacs--Zero-Erase-SSD.jpg"
---

I use the following for an SSD:

```bash
shred -vf /dev/nvme0n1p5
blkdiscard /dev/nvme0n1p5
```
