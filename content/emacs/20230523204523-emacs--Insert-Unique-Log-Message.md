---
title: "Insert Unique Log Message"
author: ["James Dyer"]
lastmod: 2023-05-29T12:00:00+01:00
tags: ["emacs", "ada", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230523204523-emacs--Insert-Unique-Log-Message.jpg"
---

I had tried to implement a debugging logging/print method myself using macros but hadn't really achieved the level of elegance outlined in <https://xenodium.com/sprinkle-me-logs/>

<!--more-->

{{< figure src="/emacs/20230523204523-emacs--Insert-Unique-Log-Message.jpg" class="emacs-img" >}}

I added a couple of programming modes to the function defined in the post above and have now incorporated it into my workflow:

```elisp
((equal major-mode 'ada-mode)
 (cons (format "Ada.Text_Io.Put_Line (\"%s: \\([0-9]+\\)\");" word)
       (format "Ada.Text_Io.Put_Line (\"%s: %%s\");" word)))
((equal major-mode 'c++-mode)
 (cons (format "std::cout << \"%s: \\([0-9]+\\)\" << std::endl;" word)
       (format "std::cout << \"%s: %%s\" << std::endl;" word)))
```

For some reason I always seem to tend to ribald statements within my code, something like **poop** or some other unsavoury variant, I just need to remember to tidy these up later on!

Oh and I added an old fashioned emacs badge to the top of this blog just for fun! as technically it is kinda true as in this web page and of course as in me as a human 😀
