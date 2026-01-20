---
title: "Profiling and Accidental Learning"
author: ["James Dyer"]
lastmod: 2022-08-20T00:00:00+01:00
tags: ["profiling", "esup", "emacs", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/emacs--profiling-and-accidental-learning__emacs_linux.jpg"
---

My sacred emacs is taking 13 seconds to start up!

<!--more-->

{{< figure src="/emacs/emacs--profiling-and-accidental-learning__emacs_linux.jpg" class="emacs-img" >}}

This is not good, I have read somewhere about a built in profiler, and indeed when I **ivy** complete for a function **profiler** I find a plethora of options.  **profiler-start** looks good, and with a little intersearching it all seems simple enough, except it looks as though it probably won't solve my start up problems but will profile any commands I execute in emacs, so would solve for example an issue I had in the past with **deft** starting up.

I found a good option in **esup**, added the following to my .emacs :

```elisp
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)
```

initially it didn't work and gave me the following:

```nil
  error in process sentinel: Wrong type argument: (or eieio-object class), nil, obj
```

but with a little research I managed to fix it with adding the following to my .emacs :

```elisp
;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
(setq esup-depth 0)
```

and on running **esup** I get a nice report generated with the top offenders in my _.emacs_ **org-download** was at the top:

```elisp
.emacs:36  0.264sec   22%
(use-package org-download)
```

I like the way it runs up a separate child emacs session to test out the timing on the init file and then generated a report.

But overall the total time was _1.166sec_ there must be something else going on here.  At which point I twigged and remembered that as I have **(desktop-save-mode 1)** set it means that all my buffers are reloaded on a new emacs startup, once I cleaned all these out, which I do from time to time it only took a couple of second to run up!

I think my lesson from this exercise is the amazing possibilities of learning through investigation and trying to fix issues and emacs is so configurable and so much fun to play around with I find myself learning more and more!
