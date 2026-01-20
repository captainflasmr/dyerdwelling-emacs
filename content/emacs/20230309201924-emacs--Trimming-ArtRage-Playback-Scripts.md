---
title: "Trimming ArtRage Playback Scripts using Emacs"
author: ["James Dyer"]
lastmod: 2023-03-21T14:30:00+00:00
tags: ["emacs", "artrage", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230309201924-emacs--Trimming-ArtRage-Playback-Scripts.jpg"
---

Emacs isn't my only obsession, I like to create digital art and for that I use ArtRage.  The interesting thing about ArtRage is that each brush stroke or editing action can be recorded in a text file or script for later playback.  I use this facility for creating time-lapses.

<!--more-->

The ArtRage manual describes the following:

> **Script Files**<br />
> ArtRage scripts are simple Unicode UTF16 text files which can be edited using Notepad, Notepad+++, TextEdit, or any similar program

"similar program"?! now let me think..... what do I have available :)

For a nice clean time-lapse ArtRage now has the ability to filter out the rest of the UI elements and leave the canvas fixed in the center of the screen.  For the most part this works very smoothly however sometimes the recording hasn't quite registered some strokes properly or some reference files have moved, in this case I may get an error such as:

{{< figure src="/emacs/Screenshot_20230309_203530.jpg" width="100%" >}}

At which point it is time to break out emacs and delve into the recorded text script file.  For a typical piece of art the script can get quite large, for my last portrait it was 174M with 605012 lines.  How does emacs handle this?, well it had no problem and opened it in a split second!

I think to resolve this current issue around `ReferenceImageXForm` I am just going to remove all occurrences as for a time-lapse playback I don't usually want to show the references anyway.

First of all lets `isearch` for ReferenceImageXForm, now that I have `isearch-lazy-count` turned on it gives me 1/36 in the minibuffer.  Lets just step through each one and remove the whole line.

Also I think I would like to remove the initial reference image load which starts off as:

```nil
Wait: 33.293s EvType: Command CommandID:
 LoadReferenceImage	Idx: 0	Reference Image: {
```

and then has many lines of image data and then finishes with an enclosing brace.  Of course this is easy to remove in emacs by leveraging:

```nil
(kill-sexp &optional ARG INTERACTIVE)
Kill the sexp (balanced expression) following point.
```

now a quick save and playback and the timelapse runs perfectly.

I remember a while ago when I was not quite aware of the capabilities of emacs and I was more Windows bound I had to look for specific text editors that could handle large files and then save them in the requisite Unicode UTF16 format.  They were slow, it was a hassle, I couldn't define any macros e.t.c, but emacs just does this out of the box!

Maybe I should petition the ArtRage team to reference emacs in their documentation!

{{< figure src="/emacs/20230309201924-emacs--Trimming-ArtRage-Playback-Scripts.jpg" class="emacs-img" >}}
