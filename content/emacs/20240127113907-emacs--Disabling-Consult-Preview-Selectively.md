---
title: "Opening Frequently Used Files More Efficiently using consult"
author: ["James Dyer"]
lastmod: 2024-01-27T21:14:00+00:00
tags: ["emacs", "consult", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240127113907-emacs--Disabling-Consult-Preview-Selectively.jpg"
---

Periodically, I find myself in situations where I restart Emacs frequently, such as when tweaking my configuration or simply experimenting. During these phases, to achieve a swift start-up, I often resorted to clearing my **`ibuffer`** as I tended to have more of an automatic **`desktop-save`** type workflow for restoring Emacs sessions.  However this would leave me with the task of manually reconstructing my previous session by reopening files.

<!--more-->

{{< figure src="/emacs/20240127113907-emacs--Disabling-Consult-Preview-Selectively.jpg" width="100%" >}}

There are of course **`dired`** functions, like **`find-name-dired`** to more quickly locate files and although I have a good grasp of where my files are, this process became somewhat repetitive and time-consuming.

I realized that my sessions tend to involve the same set of files, sparking a need for a more streamlined approach.

This lead me to transition to the built-in **`recentf`** and then **`consult-recent-file`** with `(setq recentf-max-saved-items 200)` covering most of the files I would typically want to open.

Emacs starts up now in under 2 seconds and wont have a dependency on the number of buffers I had open on a previous session.  If I want to edit a recent file I can open **`consult-recent-file`** bringing up a `completing-read` list of my recently opened files and because I have `(savehist-mode 1)` my very recently opened files will always appear at the top - quite handy!

The only aspect of **`consult-recent-file`** that I am not too keen on as with most consult functions is the file preview when traversing through the recent file list as it seems to be slowing down the opening process due to a series of perceptible delays.  Is it possible to tweak this behaviour in Emacs? Absolutely! So far, I've incorporated the following consult-related packages:

-   consult-recent-file
-   consult-theme
-   consult-outline
-   consult-imenu

and I think the only preview that I require is **`consult-theme`** leading to the following configuration:

```elisp
(consult-customize
 consult-theme :preview-key '(:debounce 0.2 any)
 consult-recent-file consult-outline consult-imenu consult-history :preview-key nil)
```
