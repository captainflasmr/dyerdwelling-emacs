---
title: "Dired going Up Directories"
author: ["James Dyer"]
lastmod: 2022-09-28T00:00:00+01:00
tags: ["macros", "emacs", "dired", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2022-09-28_20-35.jpg"
---

Now `dired` is becoming more ingrained into my muscle memory, navigating efficiently through the folder structure is becoming more prominent in my mind and it still doesn't feel natural enough.

<!--more-->

{{< figure src="/emacs/2022-09-28_20-35.jpg" class="emacs-img" >}}

I now don't even think about using `C-x d` and the _Enter_ key is fine for either opening a file or traversing into a directory.  But what about moving up a directory!, also a very common action.

The default defined key is `^` and actually isn't too bad and almost feels quite natural, _almost_...

Currently to traverse windows and buffers I am using the `M` key with comfortable key navigation hand positions as I am using these all the time.  I feel like traversing up a directory should have the same feeling, and now I think about it, getting a feel for a hand position and hence an instinctual interaction with the keyboard is almost what emacs is all about (apart from the idea of extension by macros).  Anyways, I digress...

What I would like to achieve is a quick command, no more than I am currently using for my window/buffer navigation, so a quick `M` and another key, one that makes sense and is sensible.  So that is why I think I will have to discount the default `^` as it involves the shift key and actually trying to reach the tilda/6 key doesn't feel that comfortable.

Hence I present the following:

```elisp
(define-key dired-mode-map (kbd "M-u") 'dired-up-directory)
```

The hand position is very comfortable, it fits in with the rest of my window navigation keys and `u` could very well mean **up!**

But there is something left, something that doesn't feel right, or natural... and that is switching to `dired` from a file.  Now I have this new defined key to traverse up a directory I have a general feeling that I should be using the same defined key to show `dired` from when I am visiting a file in a buffer.  It is very odd... I can't explain it, it just feels natural.

If it feels right then I am just going to go ahead and do it, I think the best method is probably to define a macro as thus:

```elisp
(global-set-key (kbd "M-u") 'file-up-dir)

(fset 'file-up-dir
      (kmacro-lambda-form [?\C-x ?d return ] 0 "%d"))
```

The macro is just calling `dired` and then **return** to action the current directory.  That is all I really want to do and for some reason in my own mind I seem to have extrapolated a file to be floating above / or below a directory so it almost makes sense to "go up" a directory to show the directory and hence `dired`
