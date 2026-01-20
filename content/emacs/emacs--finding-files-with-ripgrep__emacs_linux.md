---
title: "Finding Files With RipGrep"
author: ["James Dyer"]
lastmod: 2022-09-21T00:00:00+01:00
tags: ["ripgrep", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--finding-files-with-ripgrep__emacs_linux.jpg"
---

Now I am appreciating the power and simplicity of ripgrep and have it available on all my systems I was hunting around emacs to see the best way to leverage this tool for quick file searching.

<!--more-->

Working on a code base that is familiar to me is fine when locating files within emacs as of course I know where to go and when I am not I would typically open up a terminal and use `find`

As we know, `find` can be a little picky and fiddly to use and it is not a straightforward process to define the files and folders I am not interested in when returning an efficient search.

Now I have defined an **.ignore** file to define which files and directories I am not interested in for a regular ripgrep I was wondering if I could also use this somehow to search for files within a project hierarchy and from within emacs.

I have heard good things about projectile and in combination with a completion system it can bring up a searchable file list. This seems like a good solution but unfortunately I have never really found it fits in with my simple workflow, I have always failed to set it up properly and generally I don't require the ability to switch between projects.

After running `list-packages` and literally searching for ripgrep, I came across a package called **find-file-rg**, which leverages the `rg --files` command to list all files in the directory defined but excluding from the ripgrep defined .ignore file.

Fed into Ivy it means a list of potential files can be displayed and located using its completion system and all I need to define is the following:

```elisp
(global-set-key [f4] 'find-file-rg)
```

and hey presto!

{{< figure src="/emacs/emacs--finding-files-with-ripgrep__emacs_linux/2022-09-21_21-11.jpg" class="emacs-img" >}}
