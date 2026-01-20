---
title: "RIP ripgrep-regexp, long live deadgrep!"
author: ["James Dyer"]
lastmod: 2022-12-07T00:00:00+00:00
tags: ["ripgrep", "grep", "emacs", "deadgrep", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20221207210603-emacs--RIP-ripgrep-regexp-Long-Live-DeadGrep.jpg"
---

Well this is quite a turn up for the books, I seem to have already quickly moved on from emacs **ripgrep-regexp**!  to something better and that is a package called **deadgrep**

<!--more-->

{{< figure src="/emacs/20221207210603-emacs--RIP-ripgrep-regexp-Long-Live-DeadGrep.jpg" class="emacs-img" >}}

Now why is this?  I hear you all ask, your new grepping workflow seemed perfect, a process of file searching that could last the ages. Well as it turns out that age was more of a collection of weeks.

I started to struggle with the compilation-mode output of **ripgrep-regexp** in that a next-error call would only take me to the requisite file if I entered a directory name.  I just wanted to spam F8/next-error my way through all resulted grepped files so I could get a general sense of which files contained my search as I don't always know what I am looking for.  The constant calling up of the directory to input was seriously disrupting my workflow and after some investigation I was struggling to find a way to resolve this.

The main issue seemed to be **compilation-mode**

**compilation-mode** seems to depend on **compilation-search-path** to locate a file automatically in a buffer.  This I suspect is generally ok when compilation-mode is tied into a build project as this path is built up automatically, for example building Ada files typically will parse a gpr file to construct search directories.  Maybe projectile or project.el could help me resolve this, but as mentioned in previous posts currently a project concept doesn't yet fit into my workflow.

Well I guess I could create a list of search directories and add it to the **compilation-search-path**, something like :

```elisp
(setq my-search-directories (append '("-/bin" "-/test") '("-/content" "~/content/hugo")))

(setq compilation-search-path my-search-directories)
```

but this isn't really very flexible, I would have to keep modifying this search directory list as my disk directory hierarchy evolves.

What I need then is an emacs package that doesn't rely on compilation-mode for its output and will just leverage the ripgrep output to directly locate files.

This is where I found **deadgrep** and in fact the package text immediately applied half a pot of emollient to the skin of my current rash of irritations :

> Perform text searches with the speed of ripgrep and the comfort of Emacs. This is a bespoke mode that does not rely on compilation-mode, but tries to be a perfect fit for ripgrep.

The out of the box behavior is to search for an input string entered in the minibuffer for all occurrences from the current directory. The output is a ripgrep output grouping grep finds together file by file.

Some options appear on the top of the output buffer to provide some quick configuration ripgrep options, such as:

```nil
Search term: <search string input> change
Search type: string words regexp
Case: smart sensitive ignore

Directory: <search directory>
Files: all type glob
```

and all the occurrences of the search string are highlighted.

So the next step (pun intended) is to step through these results and see if they open a window/buffer for each search result.

F8/next-error does the trick!, so a continual F8 opens the relevant grepped file result in a window/buffer and without the need to confirm a directory.

So out of the box deadgrep seems to work perfectly for me, it is lightning fast (well I guess it would be). I can step through the results quickly, it respects my **.ignore** file and as a bonus allows a quick changing of ripgrep configuration options!, what more could an emacs user want?!  (don't answer that!)
