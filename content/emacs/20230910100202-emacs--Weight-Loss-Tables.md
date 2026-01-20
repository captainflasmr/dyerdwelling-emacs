---
title: "Org Table to Calculate Weight Loss"
author: ["James Dyer"]
lastmod: 2023-09-12T21:11:00+01:00
tags: ["orgtable", "org", "emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230910100202-emacs--Weight-Loss-Tables.jpg"
---

For a while now I have been using org tables to represent and calculate pieces of data, obviating the need to open up a normal spreadsheet.

<!--more-->

{{< figure src="/emacs/20230910100202-emacs--Weight-Loss-Tables.jpg" class="emacs-img" >}}

Recently I have been wanting to create a table to keep track of weight loss and this is what I came up with (with example weights):

```nil
| date             | weight | pounds | loss | total |
|------------------+--------+--------+------+-------|
| <2023-08-18 Fri> |   15:5 |    215 |      |       |
| <2023-08-25 Fri> |   14:9 |    205 |  -10 |   -10 |
| <2023-09-01 Fri> |   14:2 |    198 |   -7 |   -17 |
| <2023-09-08 Fri> |  13:11 |    193 |   -5 |   -22 |
#+TBLFM: $3='(convert-weight $2)::@3$4..@>$4=$3-@-1$3::@3$5..@>$5=vsum(@$4..@3$4)
```

The initial difficulty was my British attachment to the imperial system which means always defining a human weight in stone and pounds.  I had to initially define the following function to parse the weight string value into a numeric total pounds representation.

```elisp
(defun convert-weight (weight)
  (let* ((parts (split-string weight ":"))
         (stone (string-to-number (car parts)))
         (pounds (string-to-number (cadr parts))))
    (+ (* stone 14) pounds)))
```

After the date and weight is inserted the total pounds can be calculated as thus :

```nil
$3='(convert-weight $2)
```

which is calculated for every data row.

I can now more easily calculate the loss each week with a simple org table equation and hence a total cumulative loss.

I think I can use `calc` in some way in an org table but for me this method seems easier and I have complete control over my weight input string.

The next issue is one I have run across many times with org tables and that is catering for a first line calculation.  This is where I have to start getting my head around cell referencing.

For this I found the `C-} (org-table-toggle-coordinate-overlays)` very useful which explicitly shows the cell reference positions.  The row column identifier often catches me out as the row specification only counts data lines and not things like headline fillers.

Org seems to prefer the @ROWS$COLUMN convention and although this doesn't feel intuitive to me (I prefer it the other way) I thought I would stick with it and just adjust my thinking.

The loss calculation although a simple one needs to only take place on those data rows that have a previous weekly weight value, namely all those data rows not in the first data line.

Therefore I specifiy a range:

```nil
@3$4..@>$4=$3-@-1$3
```

Note the `@>` which references the last row.

Note: I think I could make this more flexible by specifying an offset from the hline, namely `@I+1`

Finally I need to add in a running cumulative total and again I had to consider not calculating on the first data line:

```nil
@3$5..@>$5=vsum(@$4..@3$4)
```

So that's just about it for ongoing measurements.  I just need to insert a new row at the bottom of the table, insert a date and current weight and on recalculation all the work is done for me!
