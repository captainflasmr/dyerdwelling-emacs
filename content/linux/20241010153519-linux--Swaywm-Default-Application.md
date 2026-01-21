---
title: "Swaywm-Default-Application"
author: ["James Dyer"]
lastmod: 2024-10-10T15:35:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20241010153519-emacs--Swaywm-Default-Application.jpg"
---

Modify _home/jdyer_.config/mimeapps.list to add in the following for example, opening links in a browser:

```nil
x-scheme-handler/http=chromium.desktop
x-scheme-handler/https=chromium.desktop
```

and for other too:

```nil
application/pdf=com.github.xournalpp.xournalpp.desktop;firefox.desktop;okularApplication_pdf.desktop;
```
