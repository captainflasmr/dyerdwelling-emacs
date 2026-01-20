---
title: "Do You Remember?"
author: ["James Dyer"]
lastmod: 2022-10-10T00:00:00+01:00
tags: ["remember", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--do-you-remember__emacs_linux.jpg"
---

I am currently keeping all my textual notes in a separate org file called appropriately, **notes.org**.  This is pretty much just a random rambling set of text containing bits and bobs that I don't want to forget.

<!--more-->

{{< figure src="/emacs/emacs--do-you-remember__emacs_linux.jpg" class="emacs-img" >}}

If I am mobile and want to jot something down I use an android app called **Markor**, just open up the notes.org as a text file and add something in. **Syncthing** means that this file is present on my laptop when I open it up in emacs.

In emacs, I can open the file, edit it and save it, but it all seems a little too clunky to me. If I want to just register a thought or idea isn't there a better way to accomplish this in emacs?, the answer of course as we all know is... YES!

I will add a note right now that am vaguely aware of org-capture but I get a sense that this might be too much in terms of functionality.  I really just want a quick method to record some keystrokes in whatever form I see fit.

I will again double down on my main requirement for introducing a new workflow in emacs and that is to use vanilla emacs where I can and that would involve either built-in functionality or maybe a little elisp.

Lets list some packages and search through the **built-in** emacs packages... The following seems to present itself! :

```nil
  remember 2.0 built-in a mode for quickly jotting down things to remember
```

This seems to be nice and simple and the Help file is quite informative. It is based around the concept of quickly recording something to remember and then organise at a later date. As it says:

> the initial "just remember this" impulse should be as close to simply throwing the data at Emacs as possible.

and as we know, emacs is very good at handling data.

Out of the box, running `M-x remember` brings up a blank **Remember** window buffer encouraging you to `C-c C-c to remember the data`, You can enter anything you want and with `C-c C-c` the window disappears with your input appended to a text file. Perfect!

By default a **notes** file is stored in .emacs.d/ and I think I might just want to change this to point to my own **notes.org**.  By default the following is a typical entry appended to the bottom of a notes file:

```nil
  **Mon Oct 10 15:25:29 2022 (test)

  test
```

This initial form can be tweaked to conform to whatever notes format text file you have, in my case I added the following to my .emacs:

```elisp
'(remember-annotation-functions nil)
'(remember-data-file "/home/usr/content/notes.org")
'(remember-leader-text "**")
'(remember-notes-initial-major-mode 'org-mode)
'(remember-time-format "")
(global-set-key (kbd "C-c n") 'remember)
```

Now if a thought occurs and I don't trust my aging brain to remember, a quick `C-c n, text input, C-c C-c` flick of the fingers will record that information. The window is transitory and doesn't disrupt my emacs session and the text org file will be updated and then synced to my phone for a later potential recall.

There are other aspects of remember that I could explore, like what is a diary file?, but I don't care too much about that, this works perfectly and is another example of the simple out-of-the-box amazingness of emacs!
