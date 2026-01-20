---
title: "Ediff Comparing Regions"
author: ["James Dyer"]
lastmod: 2025-01-08T15:30:00+00:00
tags: ["emacs", "ediff", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250108140933-emacs--Ediff-Comparing-Regions_001.jpg"
---

`ediff` is one of my favourite tools in Emacs and I have recently found myself not only using `ediff-buffers` and `vc-ediff` often but also now having the need to compare two regions.

For example, when developing new Elisp functions, I generally modify the bottom of my tangled init file and then evaluate/edit as necessary, or sometimes through the _scratch_ buffer.

However, for something more complex, for example, at the moment, I am trying to generate an RSS feed from my Hugo-based blog's Org file through `org-publish`. In this case, I prefer creating a separate directory where I can experiment with example files, generated files, and temporarily manage everything under version control.

At the end of development, I could simply paste my function(s) back into my main configuration, however, as a software engineer, I am accustomed to using merging tools, so what better way to handle this than with `ediff`

Given my development functions and my init file, I can't compare entire files to copy/merge the changes across, and VC in this case is not helpful. So, how do I compare two regions? In fact, is it even possible in Emacs?

Well, yes, yes it is! (of course)

> ediff-regions-linewise
>
> Run Ediff on a pair of regions in specified buffers.
> BUFFER-A and BUFFER-B are the buffers to be compared.
> Regions (i.e., point and mark) can be set in advance or marked interactively.
> Each region is enlarged to contain full lines.

and my keybinding is:

```elisp
(global-set-key (kbd "M-s +") #'ediff-regions-linewise)
```

Note: at the moment I only ever run `ediff-regions-linewise` but the documentation recommends using `ediff-regions-wordwise` for smaller regions.  For this example I shall be using `ediff-regions-linewise` only.

Documentation for `ediff-regions-wordwise`

> The ediff-regions-linewise function is effective for large regions, over 100-200 lines.
>
> For small regions, use ‘ediff-regions-wordwise’.

Initially, I prefer to have a horizontal split with the two files containing the regions I would like to compare, and then place the point in the left-hand window.

When `ediff-regions-linewise` is then activated, that is the order the regions are set up, from left to right.

Note: According to the documentation, it seems possible to set up the two regions using marks before the call, after which you will go directly into an ediff comparison rather than the interactive region described below.  However, I have not yet been able to get the region comparison to work this way.

Ok, lets do a region comparison!

---

1.  **Run**
    -   Open both buffers side-by-side, Buffer A : Buffer B
    -   Run `M-x ediff-regions-linewise`
    -   Select each side-by-side buffer, by &lt;RET&gt;, &lt;RET&gt;

2.  **Mark Buffer A's Region**
    -   When prompted for "Region A:", Buffer A will be automatically switched to
    -   Now mark the region for comparison
    -   Once the region is marked, hit C-M-c

3.  **Mark Buffer B's Region**

    -   Similarly, when prompted for "Region B:", Buffer B will be automatically switched to
    -   Now mark the region for comparison
    -   Once the region is marked, hit C-M-c

    Now the usual familiar ediff comparison begins.

    ---

    Here is a little visual example set up with a simple paragraph difference:

    {{< figure src="/emacs/20250108140933-emacs--Ediff-Comparing-Regions_001.jpg" width="100%" >}}

    In this case Buffer A and Buffer B are set up side-by-side and with the process for a region comparison as described above on **2. Mark Buffer A's Region** that I have deliberately made slightly different for Buffer B.

    Here is the comparison result:

    {{< figure src="/emacs/20250108140933-emacs--Ediff-Comparing-Regions_002.jpg" width="100%" >}}

---

I have something that seems to work for me at the moment, but I would really like to fully understand the difference between a linewise and a wordwise comparison. I understand that wordwise comparison could be computationally expensive on large blocks, but what benefits do I gain from using a smaller wordwise comparison compared to a linewise comparison?

Also, although the interactive region selection works well, I really want to figure out how to set up the regions initially.

If anyone knows answers to these questions, just drop in a comment!
