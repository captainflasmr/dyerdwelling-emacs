---
title: "Images to Blog Posts"
author: ["James Dyer"]
lastmod: 2022-08-28T00:00:00+01:00
tags: ["hugo", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--images-to-blog-posts-cover__emacs_linux.jpg"
---

I am just playing around with writing some sort of "techy" blog, trying to focus on a couple of my favourite things in the world, namely linux and emacs, so I thought I would do the best productive thing in the world and just start to type.

<!--more-->

I have set up this blog in a certain way using a static web site generator called Hugo, but more on that later down the road.

For now I want to just focus on describing what I am tinkering around with at the moment regarding emacs, as yes, I am always tinkering, like a good 'ol emacs user.  Yes I am old, but more on that later down the road...

I am writing this post in org mode and am trying to figure out how to insert an image, of course the first point of call would be **org-download**, I have tried this package a few times in the past but never quite got off the ground.  I understand the concept and would love to be able to drag and drop an image straight into an org file.

This of course can be accomplished using org-download but for publishing my blog it doesn't quite cut the mustard.  Hugo uses org files natively and rather than defining a **[[file:** type org link it omits the file reference and forward slash starts a reference to the _org-hugo-base-dir_ which would be where my static image files reside.

I had a bit of a hunt around and although I could define which directory the image would be dumped when draggging the file from the file explorer I would probably need to change this for each new post and even then I would need to remove the **file:** from the link reference.

So I found a simple method:

-   In emacs dired the directory I want to drag the image
-   Drag the image, the file is dropped nicely
-   Use my favourite dired command at the moment **0 w** to get the full path of the
    file name
-   yank the filename that is on the clipboard into the blog org, "M-d" the start of the path up to the Hugo base directory
-   Wrap the link in double brackets

Although I said simple, the list of steps looks less simple but as part of the emacs workflow and my emacs muscle memory kicking in I think this provides at present a decent solution.

Here is an image of me using the technique so there!:

{{< figure src="/emacs/emacs--images-to-blog-posts-cover__emacs_linux/james.jpg" width="100%" >}}

It took me about 10 seconds!, not too shabby

Next step, I might look to a good spellchecker, flycheck, aspell, hunspell and all that!
