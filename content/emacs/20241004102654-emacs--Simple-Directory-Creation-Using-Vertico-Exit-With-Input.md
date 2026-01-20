---
title: "Simple Directory and File Creation Using Vertico Completion Exit With Input"
author: ["James Dyer"]
lastmod: 2024-10-09T20:30:00+01:00
tags: ["vertico", "emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241004102654-emacs--Simple-Directory-Creation-Using-Vertico-Exit-With-Input.jpg"
---

In my previous post about creating simple functions to create a new directory and file in `dired`, I received an interesting comment suggesting another way to bypass the potential messiness of completion and pass through the literal text input.

<!--more-->

{{< figure src="/emacs/20241004102654-emacs--Simple-Directory-Creation-Using-Vertico-Exit-With-Input.jpg" width="100%" >}}

My last post :

[Simple Directory and File Creation in Dired]({{< ref
"/emacs/20240922201246-emacs--Efficient-Directory-and-File-Management-with-Dired-in-Emacs.md" >}})

It is a subtlety of most if not all completion systems, and that is to exit with any input, hence exiting without having to necessarily match and complete.

Given this, and given that I'm using the completion framework of `vertico`, I thought I would revisit the vertico github manual : <https://github.com/minad/vertico>

and I find :

**M-RET -&gt; vertico-exit-input**

> vertico-exit-input exits with the minibuffer input instead. Exiting with the current input is needed when you want to create a new buffer or a new file with find-file or switch-to-buffer. As an alternative to pressing M-RET, move the selection up to the input prompt by pressing the up arrow key and then press RET.

Well this is exactly the issue I was having and why I wrote my two little simple functions.  Now if I build in M-RET into my muscle memory (which already seems pretty natural), I can refine my Emacs init, while adhering more to vanilla Emacs concepts.  Also as I now am being more idiomatic, I suspect the M-RET keybinding may just come in handy at some point in the future.

So, now rebinding `dired-mode-map` as follows:

```elisp
("_" . dired-create-empty-file)
```

This is all that is required; note that `dired-create-directory` is already bound to my preferred keybinding.

So continues the evolution of my Emacs understanding!

As a side-ish note, I have found that writing this blog provides not only an opportunity to explore a topic further and thoroughly understand it, thereby enabling me to articulate the subject fully, but also to receive comments and discover subtle nuances that I hadn't realized existed or hadn't thought to explore!
