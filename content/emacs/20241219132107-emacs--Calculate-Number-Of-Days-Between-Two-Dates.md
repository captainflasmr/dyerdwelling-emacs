---
title: "Calculate Number Of Days Between Two Dates"
author: ["James Dyer"]
lastmod: 2024-12-19T13:21:00+00:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241219132107-emacs--Calculate-Number-Of-Days-Between-Two-Dates.jpg"
---

Just a very quick one today.

I recently needed to find the number of days between two dates. I thought this would be easy in Emacs, and indeed it was, but as with most things in Emacs, you need to know exactly what you're doing. Here is the method I used:

<!--more-->

```nil
  M-x =calendar=

  Navigate to the start date

  Set mark

  Navigate to the end date

  M-x =calendar-count-days-region=

  OR

  M-=
```

Note that the count is inclusive of the mark, the documentation says:

> It is bound to M-=.
>
> Count the number of days (inclusive) between point and the mark.

{{< figure src="/emacs/20241219132107-emacs--Calculate-Number-Of-Days-Between-Two-Dates.jpg" width="100%" >}}

That is all. 🙂
