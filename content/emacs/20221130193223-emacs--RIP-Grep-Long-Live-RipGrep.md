---
title: "RIP grep, long live ripgrep!"
author: ["James Dyer"]
lastmod: 2022-11-30T00:00:00+00:00
tags: ["ripgrep", "grep", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20221113093223-emacs--RIP-Grep-Long-Live-RipGrep.jpg"
---

I have come to the realisation that I can be a little more efficient when it comes to searching for text within files, or as it is known in software engineering circles, **grepping**!

<!--more-->

{{< figure src="/emacs/20221113093223-emacs--RIP-Grep-Long-Live-RipGrep.jpg" class="emacs-img" >}}

I am often looking for a string within a collection of files and mainly for a **Find All References** type of functionality.  Typically I would want to accept a string (could be regex) and a directory and the search to descend through all sub directories. Of course this could be achieved in many different ways :

1.  Open up an external terminal and grep -iR
2.  dired grepping using find-grep-dired
3.  Built in in grep-find function
4.  Is there a project.el way to recursively grep?
5.  Projectile grepping?

and probably many more that I don't even know about.

Initially I ran grep from an external terminal, examined the scrolling output and then hopped back into emacs.

Not the greatest workflow.

I guess running a shell or terminal emulator from within emacs may help but I still currently prefer to open an external console to interact with the command line.

At times I have grepped within emacs using the built in **grep** function which worked well but unfortunately didn't allow recursive grepping although often I would only want to search in a local directory anyway.

However the calling of this emacs function opened my eyes a little to the possibilities of emacs integrating with a search output, namely the use of **next-error** to step through the file list.

For me F8 is already bound to **next-error** for debugging purposes and it seems that this can also be used to step through the results from an emacs grep call, meaning that I can quickly step through each file search result and open a new window buffer with the cursor resting on the grepped line.

A quick look at the out of the box experience for **find-grep-dired** was subdirectory recursive but just listed the files it found containing the text and not the context so pretty much a no go for me.

The next thing to experiment with is **grep-find** which seems basically to leverage a find command, typically in the following format:

```shell
find . -type f -exec grep -ni <string> /dev/null \{\} +
```

An F8 takes me to the grepped line as it also seems to running in a grep output mode, so this is pretty good. There are however a couple of downsides:

1.  the command is non native emacs
2.  filtering

Working in a software version control system means that there can be quite a few directories I don't really want to consider in my search query and generally I am just concerned to grep source code. It would be nice if there was a way to filter out files/directories, which not only would clean up the grep output but also speed up the search.

As we know **find** can be very annoying and fiddly when it comes to filtering and generally involves a syntactical nightmare of prunes, logic and parentheses, plus you can't define all these exclusions in a single file like _rsync_ or _tar_ for example.

Ok, so lets carry on, what is next?, it is projects... Now I am going to cheat a little here, I currently don't use any concept of a project or a collection of related files in emacs, I seem to have no need for this functionality at the moment and to be completely honest I have in the past tried to incorporate a project concept into my workflow but it just doesn't seen to work for me, maybe a later date.

Which leaves the elephant in the room (and the one that I have so far deliberately not mentioned to irk those of you who are currently rage typing with great vigour in the comment section), the large awesome skulking grep monster! and that of course is **ripgrep**

I was reluctant at first as it didn't seem to come pre installed in any of my distros and when I ran it I got a whole slew of lines, why do I need lines of context around the search finds?. I don't like this one bit, but hang on!, its fast, really fast, maybe I should open up the man page and spend a bit of time with it.

A little time playing with **ripgrep** I came to the realisation that this tool can be tailored to just show the single line of a grep search result and more than that it can use an exclusion file!, called **.ignore**

Ok so this tool looks pretty promising, so on to the next step, and of course that is to find an emacs package that can give me an integrated emacs ripgrep experience.  So firing up **list-packages** I settled on a package called (surprisingly), **ripgrep** which is just a simple front end to "rg\* (the installed ripgrep executable) and comes with a command "ripgrep-regexp\* and leverages compilation-mode to display the results. As the output is compilation mode it means that the F8 next-error binding will just work out the box.

After a little playing around I can start to tailor the **.ignore** filter file to trim down the results and make the already lightning fast **ripgrep** to be extra super duper turbo charged, and the results can be navigated by using F8 which currently sits nicely into my workflow.

Now I have bound **ripgrep-regexp** to F8 in emacs the workflow is much improved and I can now stay in emacs for longer when I want to run a **Find All References** type of functionality
