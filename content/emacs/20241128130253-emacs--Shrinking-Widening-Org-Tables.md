---
title: "Shrinking and Widening Org Tables"
author: ["James Dyer"]
lastmod: 2024-11-28T13:02:00+00:00
tags: ["orgtable", "org", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241128130253-emacs--Shrinking-Widening-Org-Tables.jpg"
---

This post is more of a note to myself, something I can store in my single emacs blog org file, so if I forget again, I can quickly search.

I keep forgetting the keybinding to shrink and expand an org table.

I often define tables to have a narrower width than content using the &lt;num&gt; concept in the top line of the table, but when I want to expand, I just can't remember the keybinding.

A search in `*help*` for `org-table-shrink` and `org-table-expand` reveals nothing!

<!--more-->

{{< figure src="/emacs/20241128130253-emacs--Shrinking-Widening-Org-Tables.jpg" width="100%" >}}

Anyway, here is a note to myself:

```nil
  C-c TAB (translated from C-c <tab>) runs the command org-ctrl-c-tab
  (found in org-mode-map), which is an interactive native-compiled
  Lisp function in ‘org.el’.

  It is bound to C-c TAB.

  (org-ctrl-c-tab &optional ARG)

  Toggle columns width in a table, or show children.  Call
  ‘org-table-toggle-column-width’ if point is in a table.  Otherwise
  provide a compact view of the children.  ARG is the level to hide.
```

For clarity, within an Org table, pressing `C-c <TAB>` will toggle shrink/expand the state of the column that the point is currently in.

`C-u C-c <TAB>` will shrink all columns AND

`C-u C-u C-c <TAB>` will expand all columns

Right, that is a note, just for me 😀
