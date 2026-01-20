---
title: "Customizing Emacs Completion: From Fido's Fuzzy Matching to Literal Substring"
author: ["James Dyer"]
lastmod: 2025-09-05T10:06:00+01:00
tags: ["fido", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250905100614-emacs--Customizing-Emacs-Completion:-From-Fido's-Fuzzy-Matching-to-Literal-Substring-Search.jpg"
---

For my completion framework, I'm currently using `fido-mode`, and more recently, `fido-vertical-mode`. However, I'm scratching yet another itch in my ongoing quest to be more efficient in Emacs, specifically to jump to files more quickly. I explored this in a previous post where I enhanced the `recentf` functionality to work through completing-read in a predictable order, but what about the completing-read interface itself?

<!--more-->

{{< figure src="/emacs/20250905100614-emacs--Customizing-Emacs-Completion:-From-Fido's-Fuzzy-Matching-to-Literal-Substring-Search.jpg" width="100%" >}}

This happens to me often in Emacs, there is a subconscious functional annoyance which eventually bubbles to the surface and this case the surface bubble revolves around fido's fuzzy matching behaviour.  Simply put, I don't like it!

While it can be helpful for discovering files and commands you partially remember, sometimes you know exactly what you're looking for and want a more literal, predictable search experience, in fact for me now, I would say it is not just sometimes, but always!. The fuzzy matching is finding too many candidates when I type in a few characters and really I want a contiguous input string to be literally matched.

This post chronicles my journey from fido's flex matching to a custom setup that provides literal substring matching, perfect for when you know what you want and just want to type it directly.

Hang on a sec, can't I just change the completion style?, this should be easy!

```elisp
(setq completion-styles '(substring basic))
```

But that has no effect!, boooo!

Anyways, that was a quick attempt at a fix, in the meantime lets explore flex a little bit more and `icomplete` (which is the underpinning completion technology of fido) and see if we cam come up with a robust solution.

Fido-mode use what's called "flex" completion by default. This means that when you type `abc`, it will match files like `a_long_b_filename_c.txt` because it finds the letters a, b, and c in that order, even with other characters between them.

While this flexibility is powerful, it can be frustrating when you want to search for a specific substring. If you're looking for a file named `project-abc-config.txt`, you might expect typing `abc` to prioritize that match, but flex matching might show you `a_big_collection.txt` first instead.

So back to my initial attempt at a fix by setting the `completion-styles` variable. The `substring` style matches your input as a contiguous block anywhere within candidates, while `basic` does prefix matching. This seemed like exactly what I wanted, I just need to find a way to set it and to make it stick.

After some digging into the source code, I found the culprit in `icomplete.el`. The `icomplete--fido-mode-setup` function contains the following:

```elisp
(defun icomplete--fido-mode-setup ()
  "Setup `fido-mode''s minibuffer."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    ;; ... other settings ...
    (setq-local completion-styles '(flex)  ; This line forces flex!
                completion-flex-nospace nil
                ;; ... more settings ...
                )))
```

This function runs every time you enter the minibuffer, forcibly overriding any `completion-styles` setting you might have configured. This explains why my `setq` had no effect, fido was resetting it on every use!

Rather than fight fido's opinionated behaviour, I could instead switch to `icomplete-vertical-mode`, which provides a similar interface but respects the standard completion configuration.

```elisp
(icomplete-vertical-mode 1)

;; scroll list rather than rotating
(setq icomplete-scroll t)

;; Make completion case-insensitive
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(with-eval-after-load 'icomplete
  (setq completion-styles '(substring basic partial-completion emacs22)))
```

This gave me the literal substring matching I wanted and I think I have managed to set up everything else to the way fido comes out of the box.

However, there was one more hurdle.

By default, `icomplete-vertical-mode` requires you to explicitly select a completion before submitting with `C-m` (Enter) which is a keybinding I had grown accustomed to using in fido. This adds an extra confirmation step that fido-mode doesn't have.  There is a way around this however and that is to adapt to the keybinding `C-j` which typically is more of a do literal action then exit type of thing, where C-m is more of just a simple Enter/action.  I am willing to adapt to this keybinding.

So this works pretty well for me really, but can I not just get `completion-styles` to stick for fido?, even though I have a solution I really want to see if I can adjust fido's default functionality.

Well simply I used an advice function to wrap around the original fido setup function and set up the `completion-styles` local variable after fido has done its thing:

```elisp
(defun my-fido-completion-styles-advice (&rest _args)
  "Override completion styles after fido setup."
  (when (and fido-mode (icomplete-simple-completing-p))
    (setq-local completion-styles '(substring basic partial-completion))))

(advice-add 'icomplete--fido-mode-setup :after #'my-fido-completion-styles-advice)
```

Now I have two options for using completion in Emacs the way I want it and now I can find files, or anything else for that matter much more quickly.

This journey taught me several important lessons about Emacs customization:

1.  **Read the source**: When configuration variables don't seem to work as expected, the source code often reveals why.

2.  **Local vs. global settings**: Fido uses `setq-local` to override settings per-buffer, which is why global `setq` calls don't work.

3.  **There's always another way**: Emacs' flexibility means there are usually multiple approaches to achieving the same goal.

While fido-mode's fuzzy matching is excellent for discovery and exploration, I just wanted the predictability of literal substring matching and with a small advice function, you can have the best of both worlds!
