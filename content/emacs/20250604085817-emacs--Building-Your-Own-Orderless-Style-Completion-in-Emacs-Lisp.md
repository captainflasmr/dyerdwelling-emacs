---
title: "Building Your Own Orderless Style Completion in Emacs Lisp"
author: ["James Dyer"]
lastmod: 2025-06-04T09:40:00+01:00
tags: ["elisp", "emacs", 2025, "completion"]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250604085817-emacs--Building-Your-Own-Orderless-Style-Completion-in-Emacs-Lisp.jpg"
---

While packages like `orderless` provide flexible “any word, any order” completion, sometimes you want something lightweight and easy to tweak (well I do anyway). In this post, I’ll show you how to implement a simple orderless-like completion style using only Emacs Lisp, and how to integrate it smoothly into your workflow.

<!--more-->

{{< figure src="/emacs/20250604085817-emacs--Building-Your-Own-Orderless-Style-Completion-in-Emacs-Lisp.jpg" width="100%" >}}

Traditional completion in Emacs often matches prefixes or substrings, but sometimes you want to type a few key parts of a word, in any order, and jump straight to your target. That’s what `orderless` and similar completion styles allow. But what if you want to write your own, or experiment with the logic?, well I will show you how...

Let’s walk through the logic:

```elisp
(defun simple-orderless-completion (string table pred point)
  "Enhanced orderless completion with better partial matching."
  (let* ((words (split-string string "[-, ]+"))
         (patterns (mapcar (lambda (word)
                             (concat "\\b.*" (regexp-quote word) ".*"))
                           words))
         (full-regexp (mapconcat 'identity patterns "")))
    (if (string-empty-p string)
        (all-completions "" table pred)
      (cl-remove-if-not
       (lambda (candidate)
         (let ((case-fold-search completion-ignore-case))
           (and (cl-every (lambda (word)
                            (string-match-p
                             (concat "\\b.*" (regexp-quote word))
                             candidate))
                          words)
                t)))
       (all-completions "" table pred)))))
```


## **What’s Happening Here?** {#what-s-happening-here}

-   **Word Splitting:**

    The user's input (a string) is split into words on spaces, dashes, or commas. This produces a list of "search terms." This means that, in the minibuffer, the word separator can be any of these characters. I was initially really faffing around and struggling to work out how to insert a space between words in the minibuffer, as it seems to perform some form of completion. However, I eventually figured out that `M-SPC` actually inserts a space, allowing you to separate words. I use `fido-mode`, so I'm not sure if this is the same for other minibuffer completion systems.

    After initially adding in the comma separator however I found that I actually prefer it, it is easier to access and I don't think any keywords, functions e.t.c will typically contain a comma?

-   **Pattern Construction:**

    For each word, a regex pattern is constructed: `\\b.*WORD.*`.
    This means: “find a word boundary, followed by any characters, then the word, then anything else.” This is a bit looser than strict word matching, and you can tune it.

-   **Candidate Filtering:**

    We generate all possible completions with `all-completions` and then filter them down. For a candidate to match, all the search terms (words) must appear somewhere, in any order.

-   **Case Sensitivity:**

    Matching respects `completion-ignore-case`, so your results will be case-insensitive if you want of course.


## **Registering and Using the Style** {#registering-and-using-the-style}

To make Emacs aware of your new completion style, add it to `completion-styles-alist`:

```elisp
(add-to-list 'completion-styles-alist
             '(simple-orderless simple-orderless-completion
                                simple-orderless-completion))
```


## **Contextual Use: Minibuffer Only** {#contextual-use-minibuffer-only}

You might not want this style everywhere (which I suspect is likely). For example, in file completion you might prefer strict prefix matching. So, let’s activate it only in the minibuffer:

```elisp
(defun setup-minibuffer-completion-styles ()
  "Use orderless completion in minibuffer, regular completion elsewhere."
  ;; For minibuffer: use orderless first, then fallback to flex and basic
  (setq-local completion-styles '(basic simple-orderless flex substring)))

;; Hook into minibuffer setup
(add-hook 'minibuffer-setup-hook #'setup-minibuffer-completion-styles)
```


## **Tweaking and Extending** {#tweaking-and-extending}

-   **Pattern Tuning:**

    The regexes can be made stricter or looser (e.g., remove `\\b` for more “fuzzy” matching).

-   **Word Separators:**

    You can split on other characters if your workflow uses different delimiters.

-   **Order of Styles:**

    Adjust the order in `completion-styles` to prefer your custom style over others. I found that if the `simple-orderless` style was listed first, pressing Tab to bring up the completions buffer doesn't work, which I like to use sometimes, so that is why `basic` is first.


### **Conclusion** {#conclusion}

With just a handful of lines, you can build your own orderless-like completion style, giving you full control and transparency. This is a great starting point for experimenting with more advanced completion logic, and a good illustration of the power of Emacs’ built-in completion framework!
