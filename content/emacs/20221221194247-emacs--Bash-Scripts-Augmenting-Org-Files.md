---
title: "Quick Bash Scripts Augmenting Org Files"
author: ["James Dyer"]
lastmod: 2022-12-21T00:00:00+00:00
tags: ["org", "emacs", "bash", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2022-12-21_20-01_t.jpg"
---

This post isn't strictly about what can be achieved within the emacs ecosystem but what can be achieved outside it while still binding to the workflow principles of org mode.

<!--more-->

I have yet to convert to any of the internal shells/terminal emulators within emacs, I have continued to Control-Alt-T (C-M-t) term my way around linux; although this is becoming less frequent due to my greater proficiency with **dired**

As my org journey evolves I have now realised that I have two files I rely on for my quick capturing needs.  When an idea pops into my head I am usually emacs bound anyway so **org-capture** is always there for me.  But what if I am alfresco?, and maybe, just maybe I am in a terminal!  Well I could quickly switch to emacs as it is 99% likely to be already running, but what if it is not?!

I could open emacs and then do my thing but emacs now takes a few seconds to load up and then for me just to close it down again afterwards?!  (as if I would) Isn't there another way?, a better way?  Could I idea push to an org file from the terminal with a quick flick of the fingers, could **bash** help me out here?, so many questions...

Well the answer is that yes, bash can help me out.

For me, any new items using org-capture are always put under the headline, hence using `file+headline` in my capture template.  How will this help?, well using the power of bash and the venerable **sed** command I can search and replace the headline but replace with an extra piece of data, namely a string of my choosing, effectively placing my TODO or note at the top of the org file list!

I thus came up with the following `bash` scripts:


## todo {#todo}

```bash
#!/bin/bash
if [[ ! -z $@ ]]; then sed --in-place 's/* Tasks/* Tasks\n** TODO '"$(echo ${@})"'/g' \ "~/DCIM/content/aa--todo.org" fi
```


## note {#note}

```bash
#!/bin/bash
if [[ ! -z $@ ]]; then sed --in-place 's/* Notes/* Notes\n** '"$(echo ${@})"'/g' \ "~/DCIM/content/aa--notes.org" fi
```

Here is my original todo org file:

{{< figure src="/emacs/20221221194247-emacs--Bash-Scripts-Augmenting-Org-Files/2022-12-21_20-01.jpg" width="300px" >}}

On the command line I can now enter:

```nil
todo generate art slideshows
```

and in a flash my org file would now be

```text

```

{{< figure src="/emacs/20221221194247-emacs--Bash-Scripts-Augmenting-Org-Files/2022-12-21_20-01_1.jpg" width="300px" >}}

all nice and ready for the next time I open emacs!, and of course now I can also add notes, or anything else of course.

My aim was for the scripts to be very simple, if you pass in no text / arguments no files will be augmented, generally the idea is just to pass in a single string, maybe multiple lines could be passed using `\n` but that isn't the way I wanted to use them.
