---
title: "Plotting Other Org Tables"
author: ["James Dyer"]
lastmod: 2023-09-26T21:30:00+01:00
tags: ["orgtable", "org", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230923092356-emacs--Adding-Row-Number-Column-In-Org-Table-thumb.jpg"
---

I'm currently in the process of learning how to create graphical plots from org tables using `gnuplot`. I've noticed that it's generally more straightforward to extract x-axis data from an org table column with incrementing numbers, as opposed to relying on `gnuplot` to potentially sort out data from an existing column (which may not be plot-friendly)

<!--more-->

Generally my existing org tables do not have such an incrementing integer column, so how do I quickly create and populate such a column?, well actually it is super easy, barely an inconvenience!

For example take the following table that represents a little flutter on the horses, how would I go about plotting the _Total_ column and therefore clearly visualise the downward sloping line 😀

```nil
| Date   | Horse          | Stk |   Ret |  Prof |  Total |
|--------+----------------+-----+-------+-------+--------|
|        |                |     |       |     0 |  306.9 |
| 27 May | aggagio        |  15 |  7.68 | -7.32 | 299.58 |
|        | red derek      |  15 |     0 |   -15 | 284.58 |
| 03 Jun | dear my friend |  10 |     0 |   -10 | 274.58 |
|        | the foxes      |  10 |    32 |    22 | 296.58 |
|        | sprewell       |  10 |    16 |     6 | 302.58 |
|        | dancing poet   |  15 | 16.73 |  1.73 | 304.31 |
```

Firstly simply `M-S-<right>` to insert a table column:

```nil
|   | Date   | Horse          | Stk |   Ret |  Prof |  Total |
|---+--------+----------------+-----+-------+-------+--------|
|   |        |                |     |       |     0 |  306.9 |
|   | 27 May | aggagio        |  15 |  7.68 | -7.32 | 299.58 |
|   |        | red derek      |  15 |     0 |   -15 | 284.58 |
|   | 03 Jun | dear my friend |  10 |     0 |   -10 | 274.58 |
|   |        | the foxes      |  10 |    32 |    22 | 296.58 |
|   |        | sprewell       |  10 |    16 |     6 | 302.58 |
|   |        | dancing poet   |  15 | 16.73 |  1.73 | 304.31 |
```

Now move the cursor to the starting point and put in an initial integer:

```nil
|   | Date   | Horse          | Stk |   Ret |  Prof |  Total |
|---+--------+----------------+-----+-------+-------+--------|
| 1 |        |                |     |       |     0 |  306.9 |
|   | 27 May | aggagio        |  15 |  7.68 | -7.32 | 299.58 |
|   |        | red derek      |  15 |     0 |   -15 | 284.58 |
|   | 03 Jun | dear my friend |  10 |     0 |   -10 | 274.58 |
|   |        | the foxes      |  10 |    32 |    22 | 296.58 |
|   |        | sprewell       |  10 |    16 |     6 | 302.58 |
|   |        | dancing poet   |  15 | 16.73 |  1.73 | 304.31 |
```

Now just simply `S-Enter (org-table-copy-down)` which will fill an incremented number downwards as far as you want:

```nil
|   | Date   | Horse          | Stk |   Ret |  Prof |  Total |
|---+--------+----------------+-----+-------+-------+--------|
| 1 |        |                |     |       |     0 |  306.9 |
| 2 | 27 May | aggagio        |  15 |  7.68 | -7.32 | 299.58 |
| 3 |        | red derek      |  15 |     0 |   -15 | 284.58 |
| 4 | 03 Jun | dear my friend |  10 |     0 |   -10 | 274.58 |
| 5 |        | the foxes      |  10 |    32 |    22 | 296.58 |
| 6 |        | sprewell       |  10 |    16 |     6 | 302.58 |
| 7 |        | dancing poet   |  15 | 16.73 |  1.73 | 304.31 |
```

Now I can use a #+PLOT header to reference the first column for the x axis:

```nil
#+PLOT: title:"Betting" ind:1 deps:(7) type:2d with:lines set:"yrange [250:350]"
|   | Date   | Horse          | Stk |   Ret |  Prof |  Total |
|---+--------+----------------+-----+-------+-------+--------|
| 1 |        |                |     |       |     0 |  306.9 |
| 2 | 27 May | aggagio        |  15 |  7.68 | -7.32 | 299.58 |
| 3 |        | red derek      |  15 |     0 |   -15 | 284.58 |
| 4 | 03 Jun | dear my friend |  10 |     0 |   -10 | 274.58 |
| 5 |        | the foxes      |  10 |    32 |    22 | 296.58 |
| 6 |        | sprewell       |  10 |    16 |     6 | 302.58 |
| 7 |        | dancing poet   |  15 | 16.73 |  1.73 | 304.31 |
```

which will produce the following plot:

{{< figure src="/emacs/20230923092356-emacs--Adding-Row-Number-Column-In-Org-Table.jpg" width="100%" >}}

Not such a downward spiral as I first thought!
