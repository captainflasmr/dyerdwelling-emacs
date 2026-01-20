---
title: "Trimming Text With Macros"
author: ["James Dyer"]
lastmod: 2022-09-25T00:00:00+01:00
tags: ["macros", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--trimming-text-with-macros__emacs_linux.jpg"
---

I am having a bet!, it is one of my few vices, and it shall be on the horses.  I don't often have a bet and in fact I generally only ever have a bet on a special occasion, like the grand national or a random parallel bet with my dad.

<!--more-->

Today is a parallel bet, yesterday my dad won over £650 and I have decided to leverage his expert equine knowledge to have a little flutter.  But wait, I'm not going to do that, I am going to leverage my old javascript horse racing prediction program, the algorithm of which was based on something I wrote on my Amstrad CPC in the mid 80s.

Now what has all this twaddle got to do with emacs I hear you say?, well my javascript program requires a very specific format of horse data and it is in fact a very simple one: `number form horsename`

My general aim in the past was to pretty much find a website displaying horse data for the chosen race and copy and paste into a web text input field which my javascript would parse and perform its magic.

SportingLife looks like a good one (top of my startpage search).  The data though of course is in the wrong order and there is a lot of it for each horse.

My first thought with all these things is how can emacs help me out with this?  Well a paste into the **scratch** buffer gives me something like this:

```nil
   1
  (5)
  Horse silk
  Saratoga Goldb27
  Age: 4|  Weight: 10-2| J: Owen Lewis(5)| T: C Hills| OR:  85| CD
  15/8
  Has taken his form up a notch
  Form: 556811|Timeform
  2
  (6)
  Horse silk
  Sword Beachb24
  Age: 5|  Weight: 9-13| J: George Bass| T: A King| OR:  82
  28/1
  Dual Windsor scorer in 2021
  Form: 71165-0|Timeform
```

I think the best way to approach this is to open a new window along side the scratch buffer and use this as my formatted output window, I will just create a random blank new file for now called _tmp.txt_

Then using a macro I can search around extracting each piece of relevant data for each horse and then just copy to the right hand pane.

Once I have a procedure that works for a single horse then I can just end the macro and then `C-x e` repeat the last keyboard macro and then spam `e` until all the horses are parsed.

{{< figure src="/emacs/emacs--trimming-text-with-macros__emacs_linux.jpg" width="300px" >}}

At which point I can copy the text from **tmp.txt**, paste into the html text input and press **Find Winner**, lets see if my old algorithm still holds up!
