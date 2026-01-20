---
title: "Merging org files for Hugo static site"
author: ["James Dyer"]
lastmod: 2023-01-09T00:00:00+00:00
tags: ["org", "macros", "hugo", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230109113207-emacs--Tidying-Hugo-org-files-using-emacs.jpg"
---

I have just started the process of reducing the number of org files I maintain for my web site.  I now have a greater understanding of how **Hugo** handles these files and I think can both simplify and make them more flexible.

<!--more-->

Hugo is my static site generator of choice and although it supports org files directly I prefer to generate to a markdown file as an intermediate step using a file by file evaluation of `org-hugo-auto-export-mode`.  This means that each org subtree I modify automatically generates an **md** file to a defined folder dictated by `#+hugo_section` in the org header.

For example, I have a few org files that generate my art blog; one for videos, finished art, galleries and now AI.  Each org file has a single header `#+hugo_section` defining the destination export folder.  I think now I would like a bit more flexibility with the ability to push a subtree to a folder of my choosing so any extra sections can more easily be added.  Plus, if I merge I would only have to maintain a single org file (although a larger one!).

To achieve this I will have to add an extra property to the existing org drawer for each subtree using `:EXPORT_HUGO_SECTION:` and then to adapt my `org-capture` templates.

Now of course to add the subtree property emacs can certainly help me out.

I would like to insert typically a drawer property line for each subtree such as:

```elisp
:EXPORT_HUGO_SECTION: art--all
```

However of course things are not quite so simple.  It seems that some subtrees already have such property lines even though the destination folder is ultimately the same as `#+hugo_section` in the header (another good reason to tidy up)

So rather than a simple `isearch` to an ever present subtree anchor point (for example `:EXPORT_FILE_NAME:`) and then the insertion of the new line I will need to take into account those subtrees that already have `:EXPORT_HUGO_SECTION:` and not insert a line.

I considered edit mode in `multi-occur` but as far as I can tell it doesn't work too well when new lines are inserted.

`deadgrep` (ripgrep) also has similar functionality and will also show context around the search point which can be also edited in a similar manner to `multi-occur` but all this is starting to feel too fiddly and I am starting to spend too much time investigating this!

The grep/occur to edit buffer approach can work well in certain situations but this is a little more complex involving a small amount of logic.  At this stage the same thought always occurs to me and I really wish it would occur to me earlier.

> what about macros?

The `isearch` technique won't quite work (as explained above), but what about `isearch-regexp`?

As the properties are always in the same order I could just insert the `:EXPORT_HUGO_SECTION:` line if I find the following regex:

```elisp
:EXPORT_FILE_NAME:.*
:EXPORT_HUGO_LASTMOD:
```

_Note that the carriage return is achieved by `C-q C-j`_

Incorporate this into a macro and the cursor will only move to a point that doesn't have a `:EXPORT_HUGO_SECTION:` line and hence I can then insert and then repeat until my org file is suitable amended!

{{< figure src="/emacs/20230109113207-emacs--Tidying-Hugo-org-files-using-emacs.jpg" class="emacs-img" >}}
