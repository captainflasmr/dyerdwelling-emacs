---
title: "More Improvements To My Weight Loss Org Table"
author: ["James Dyer"]
lastmod: 2023-10-06T18:59:00+01:00
tags: ["orgtable", "org", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20231006172548-emacs--Improving-Weight-Loss-Org-Table.jpg"
---

More improvements to my weight loss table, this time I have added the following:

<!--more-->

{{< figure src="/emacs/20231006172548-emacs--Improving-Weight-Loss-Org-Table.jpg" class="emacs-img" >}}

1.  The first column now auto populates an incremental integer
2.  Extra column to display the average loss
    ```org
       #+PLOT: title:"Weight Loss" ind:1 deps:(4) type:2d with:lines set:"yrange [150:220]"
       |   | date             |   stn | pnd | lss | tot | bar            | av-loss |
       |---+------------------+-------+-----+-----+-----+----------------+---------|
       | 0 | <2023-08-18 Fri> |  15:5 | 215 |     |     | WWWWWWWWWWWWWH |         |
       | 1 | <2023-08-25 Fri> |  14:9 | 205 | -10 | -10 | WWWWWWWWWWWV   |    10.0 |
       | 2 | <2023-09-01 Fri> |  14:2 | 198 |  -7 | -17 | WWWWWWWWWW;    |     8.5 |
       | 3 | <2023-09-08 Fri> | 13:11 | 193 |  -5 | -22 | WWWWWWWWW:     |     7.3 |
       | 4 | <2023-09-15 Fri> | 13:10 | 192 |  -1 | -23 | WWWWWWWWW      |     5.8 |
       | 5 | <2023-09-22 Fri> |  13:9 | 191 |  -1 | -24 | WWWWWWWWV      |     4.8 |
       |   |                  |       |     |     |     |                |         |
       #+TBLFM: $1=@#-2::$4='(convert-weight $3)::@3$5..@>$5=$4-@-1$4::@3$6..@>$6=vsum(@$5..@3$5)::$7='(orgtbl-ascii-draw $4 150 220 15)::@3$8..@>$8=abs(vmean(@3$5..@$5));%0.1f
    ```

    1.  On my last org weight table post a comment pointed out that there was a better way to populate the first integer column, so rather than a key-press `S-RET (org-table-copy-down)` rows can be auto populated using the `$1=@#-2` equation in the `#+TBLFM` line.

    2.  I thought it might be interesting to plot he average weight loss to gain a more general perspective and to see the average weekly value settle down over a period of time.  For this I added

        `@3$8..@>$8=abs(vmean(@3$5..@$5));%0.1f`

        which leverages the calc functions of `vmean` to average out the previous loss values and then apply an absolute value using the function `abs`.

        I also found that I needed to find a way to display the table value to 1.d.p which was achieved by `%0.1f`
