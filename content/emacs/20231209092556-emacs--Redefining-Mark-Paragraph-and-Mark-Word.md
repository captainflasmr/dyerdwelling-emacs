---
title: "Redefining mark-paragraph and mark-word"
author: ["James Dyer"]
lastmod: 2023-12-09T11:50:00+00:00
tags: ["emacs", "elisp", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/2023-12-09-10-10-57_t.jpg"
---

I can't say that I'm completely happy with the way emacs marks some elements, namely :

<!--more-->

---

<div class="ox-hugo-toc toc local">

- [mark-paragraph](#mark-paragraph)
- [mark-word](#mark-word)

</div>
<!--endtoc-->

---


## mark-paragraph {#mark-paragraph}

The default mark-paragraph visually selects a paragraph as follows:

{{< figure src="/emacs/20231209092556-emacs--Redefining-Mark-Paragraph-and-Mark-Word/2023-12-09-09-45-35.jpg" width="100%" >}}

but I think I would rather have the cursor show at the bottom of the selected paragraph, it just feels more natural to me, so as thus:

{{< figure src="/emacs/20231209092556-emacs--Redefining-Mark-Paragraph-and-Mark-Word/2023-12-09-09-46-26.jpg" width="100%" >}}

To achieve this I wrote the following with a rebind to the default `mark-paragraph` keybinding or in `orgs` case the `mark-element`

```elisp

(defun my/mark-paragraph ()
  "redefinition of mark-paragraph"
  (interactive)
  (forward-char)
  (backward-paragraph)
  (push-mark)
  (forward-paragraph)
  (setq mark-active t))

(global-set-key (kbd "M-H") 'my/mark-paragraph)
```

I had to `forward-char` initially to step on to the paragraph if my cursor was just before the paragraph as this felt natural to me so that the `backward-paragraph` won't jump to the previous paragraph.

So just a simple function with a few tweaks here and there to make the paragraph marking feel a little more natural, probably subconsciously taking cues from my time working with many different text editors and as always with emacs it is pretty awesome that this kind of low level finely grained functionality can be modified to such and extent.


## mark-word {#mark-word}

Next up is `mark-word`.  I'm finding myself wanting to naturally just mark the current word my cursor is within and currently I feel I'm having to work quite hard to achieve this by a combination of backward/forward-word and marking.

The default `mark-word` `(M-@)` seems to only mark to the end of a word, as thus:

{{< figure src="/emacs/20231209092556-emacs--Redefining-Mark-Paragraph-and-Mark-Word/2023-12-09-10-10-40.jpg" width="100%" >}}

I would want something more like:

{{< figure src="/emacs/20231209092556-emacs--Redefining-Mark-Paragraph-and-Mark-Word/2023-12-09-10-10-57.jpg" width="100%" >}}

So I created the following function and rebound to the default `mark-word` :

```elisp
(defun my/mark-word ()
  "redefinition of mark-word"
  (interactive)
  (backward-to-word 1)
  (forward-to-word 1)
  (push-mark)
  (forward-word)
  (setq mark-active t))

(global-set-key (kbd "M-@") 'my/mark-word)
```

This was trickier than I first thought as when I manually mark a word through a combination of key-presses I actually apply a little logic in that if the cursor is on the first character I don't backward-word as this would take me to the previous word!

I might be able to work out if the cursor is on the first character of a word?, but the function starts to become unnecessarily complicated.

To solve this I used `backward-to-word` which moves backward until the end of a word.  This will work irrespective of where my cursor is within the current word and then forwarding back on to the current word so my cursor is on the first character.   At this point I can now mark and `forward-word` to select the word.  Yes I have used word many times here, word, word, word, word, WORD!!!! 😀

Now typing all this out I wonder if there is a simple function to just move the cursor to the start of a word, if there is I couldn't seem to find it, but as always I have learnt a lot along the way!

**[Edited : <span class="timestamp-wrapper"><span class="timestamp">&lt;2023-12-10 Sun&gt;</span></span>]**

Actually it is possible (of course!) to work out if the cursor is on the first character of a word (see comment section), so the amended function would look like:

```elisp
(defun my/mark-word ()
  "redefinition of mark-word"
  (interactive)
  (if (not (looking-at "\\<"))
      (backward-word))
  (push-mark)
  (forward-word)
  (setq mark-active t))
```

I feel like this is a better solution.
