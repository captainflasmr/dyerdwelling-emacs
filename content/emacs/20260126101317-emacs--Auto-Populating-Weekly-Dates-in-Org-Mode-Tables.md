---
title: "Auto-Populating Weekly Dates in Org-Mode Tables"
author: ["James Dyer"]
lastmod: 2026-01-26T10:13:00+00:00
tags: ["emacs", 2026, "org"]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20260126101317-emacs--Auto-Populating-Weekly-Dates-in-Org-Mode-Tables.jpg"
---

Here is just a quick one, I was working with an org-mode table for tracking work weeks and needed to auto-populate a Date column where each row increments by exactly one week. The table structure looked like this:

<!--more-->

{{< figure src="/emacs/20260126101317-emacs--Auto-Populating-Weekly-Dates-in-Org-Mode-Tables.jpg" width="100%" >}}

The first row has a base date (2026-01-05), and I wanted subsequent rows to automatically calculate as weekly increments: 2026-01-12, 2026-01-19, and so on.

Initially, I tried several approaches that seemed logical but encountered `#ERROR` results and eventually settled on a working solution which is to hardcode the base date directly in the formula:

```org
#+TBLFM: $3='(format-time-string "%Y-%m-%d" (time-add (date-to-time "2026-01-05") (* (- @# 2) 7 24 3600)))
```

which gave:

{{< figure src="/emacs/20260126101317-emacs--Auto-Populating-Weekly-Dates-in-Org-Mode-Tables2.jpg" width="100%" >}}

and I can now of course extend the table for all the weeks in the year and I don't have to fill in manually any more!

Here's how it works:

-   `(date-to-time "2026-01-05")` - Convert the hardcoded base date to Emacs time format
-   `(- @# 2)` - Calculate the offset from the base row
-   `(* (- @# 2) 7 24 3600)` - Convert the offset to seconds (weeks × days × hours × seconds)
-   `time-add` - Add the offset to the base date
-   `format-time-string "%Y-%m-%d"` - Format back to ISO date string
