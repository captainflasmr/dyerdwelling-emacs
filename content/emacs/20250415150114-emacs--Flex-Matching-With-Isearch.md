---
title: "Flex Matching with isearch"
author: ["James Dyer"]
lastmod: 2025-04-17T09:00:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250415150114-emacs--Flex-Matching-With-Isearch.jpg"
---

I'm having issues with `isearch`!

I've been using `fido-mode` for a while now (I'm one of those weirdos who transitioned from the Vertico/Marginalia/Orderless stack back to an Emacs built-in) and have become accustomed to the flexible matching that `fido` offers. For example, I often shorten commands to find them, such as:

<!--more-->

{{< figure src="/emacs/20250415150114-emacs--Flex-Matching-With-Isearch.jpg" width="100%" >}}

It is very common for me to search in a buffer for strings that contain a dash (especially now I am doing a lot more elisp programming), and I keep instinctively fido-shortening the search (so searching without the dash). Each time, `isearch` says no (or fails), so I have to awkwardly reach out for the "-" and go back and perform the search again.

What if there was a more flexible way to perform an in-buffer search? Well, of course, there is, and that is the `isearch-regexp` variants. I have seen some people just binding this to their main `isearch` because, why not? Most of the time, the search will be identical, and there is always the opportunity for a more flexible search using ".\*".

Let's run an example. I would like to search for the first occurrence of `use-package` in my config. Let's see how many keys we can get down to for an efficient search and how easy ergonomically they are to get to (excluding the possibility of some annoyingly similar matching lines in the config)

Firstly, let's try `isearch` - you would have to type up to at least "use-pa" (probably).

Now for `isearch-regex` - you would type "us.\*p" (probably) or of course just the same as `isearch`, so "use-pa". This is better but a bit awkward to access the "\*" and you would have to be quite aware of what you were searching for and the rough split of words.

If this were translated into a form of `fido` search, you would type at least "usep" (probably), a similar number of characters, but no awkward punctuation (so faster to type) and the find is more flexible and for me now, more familiar.

As far as I'm aware, by default `fido` uses some form of flex matching (but not quite flex), which has been good enough for me for a while now. So, how do I get this form of matching using `isearch` and it being in-buffer?

After a little investigation, there are packages out there in Emacs-land for some fuzzy searching, and the most relevant seemed to be `flex-isearch`. I'm not sure I quite got it working for me, but looking through the code, I thought I could distil some concepts into simple defuns. I learned that you can actually slot any search function at the backend of isearch, so let's transform a normal search into something on steroids that would give us a flexible search, `fido` style!

```elisp
(defvar flex-isearch-group-size 3
  "Number of initial characters to group together for more accurate flex searching.")

(defun flex-isearch-regexp-compile (string)
  "Convert a search string to a more intelligent flex-matching regexp.
The first `flex-isearch-group-size` characters are grouped together for more accurate matching."
  (let* ((parts (split-string string " " t))
         (compile-part
          (lambda (part)
            (let ((grouped (substring part 0 (min flex-isearch-group-size (length part))))
                  (rest (substring part (min flex-isearch-group-size (length part)))))
              (concat
               (regexp-quote grouped)
               (mapconcat
                (lambda (char)
                  (let ((c (char-to-string char)))
                    (cond
                     ((and (>= char ?A) (<= char ?Z))
                      (concat "[^" c "\n]*" c))
                     ((and (>= char ?a) (<= char ?z))
                      (concat "[^" c (upcase c) "\n]*[" c (upcase c) "]"))
                     (t
                      (concat "[^" (regexp-quote c) "\n]*" (regexp-quote c))))))
                rest
                "")
               "[^-_[:alnum:]\n]*")))))
    (concat
     "\\b"
     (mapconcat compile-part parts "[^-_[:alnum:]\n]+"))))

(defun flex-isearch-search-fun ()
  "Return the appropriate search function for flex searching."
  (if isearch-forward 'flex-isearch-forward 'flex-isearch-backward))

(defun flex-isearch-forward (string &optional bound noerror count)
  "Flex search forward for STRING."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-forward regexp bound t)))

(defun flex-isearch-backward (string &optional bound noerror count)
  "Flex search backward for STRING."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-backward regexp bound t)))

(setq isearch-search-fun-function 'flex-isearch-search-fun)
```

With my first attempt at this, the search at times seemed to settle on more candidates than I would have liked, but I thought, "I generally always know at least the first 2 or 3 characters that I'm searching for, at which point things get a little fuzzy" (pun intended). So, can I group the first few letters as part of the search, thus narrowing down the candidate list?

The answer is yes, yes I can.

The code above is a first attempt and with all these things, I shall play around with it and see if it works for me. At the moment, this seems to give me a more `fido-iish` feel when searching in-buffer with `isearch`, and no awkward search punctuation in sight!. So, using the examples above I can now find "use-package" in my config with a simple "usep".

I shall report back in a later post to see if it has settled into my workflow, or whether there is an annoyance that I just cannot put up with so it will have to be discarded, along with my other myriad of Emacs experiments!
