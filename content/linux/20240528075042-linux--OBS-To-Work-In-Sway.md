---
title: "OBS-To-Work-In-Sway"
author: ["James Dyer"]
lastmod: 2024-05-28T07:50:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240528075042-emacs--OBS-To-Work-In-Sway.jpg"
---

If the following error is shown in the OBS log file after an error is shown:

<!--more-->

```nil
07:46:50.341: Failed to initialize MFX
07:46:50.341: [qsv encoder: 'msdk_impl'] Specified object/item/sync point not found. (MFX_ERR_NOT_FOUND)
07:46:50.341: [qsv encoder: 'simple_video_recording'] qsv failed to load
```

This is because hardware video encoding using QSV is enabled, to fix in OBS :

Settings -&gt; Output -&gt; Video Encoder :

and change:

Hardware (QSV)

to a software version
