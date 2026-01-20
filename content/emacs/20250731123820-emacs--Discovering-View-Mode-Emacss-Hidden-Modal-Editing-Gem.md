---
title: "View-Mode - Emacs's Hidden Modal Editing Gem?"
author: ["James Dyer"]
lastmod: 2025-08-01T08:30:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250731123820-emacs--Discovering-View-Mode-Emacss-Hidden-Modal-Editing-Gem.jpg"
---

---

<div class="ox-hugo-toc toc local">

- [The Rabbit Hole](#the-rabbit-hole)
- [View Mode](#view-mode)
- [The Setup](#the-setup)
- [Baby Proofing](#baby-proofing)
- [Cursor](#cursor)
- [Subtleties](#subtleties)
- [Foibles](#foibles)
- [Improvements?](#improvements)
- [Conclusion](#conclusion)

</div>
<!--endtoc-->

---


## The Rabbit Hole {#the-rabbit-hole}

I've fallen down another rabbit hole, but then again, this is Emacs we're talking about!

In this case it relates to a fundamental editing principle.

I was trying to understand some code, so of course that involved plenty of point/cursor navigation, scrolling, etc. After an hour or so of doing this, I started to wonder if there was a more efficient navigation method involving single keypresses. If I know I only want to view the file, then why not get rid of this modifier key nonsense? 😁 I'm aware that the basic functionality I'm talking about here is modal editing.

There are many different packages out there from **boon** to **meow**. So does Emacs have something built-in? Well, throughout my series of blog posts, I often ask this question, and the answer always comes back emphatically the same: **"Why yes, of course it does!"** This might be a little misleading, as though there isn't a specific built-in that gives modal editing right out of the box, there is a mode that can be tweaked a little to achieve something similar.


## View Mode {#view-mode}

So what is the thing that's been hiding in plain sight? Well, it is **view-mode**. Although strictly not a modal editing option, I think with a little tweaking I could possibly turn it into one. After a bit of internet stumbling, I came across a Reddit post from someone who has tried something similar. I think I shall add my own spin. The link is <https://www.reddit.com/r/emacs/comments/fojc1y/using_viewmode_for_modal_navigation/>

If you have been reading my posts over the years, you probably know my general design ethos on such a solution, and that is to use built-in Emacs-centric philosophies and to make sure it can easily be set up on an air-gapped system. Well, for this solution, I'm going to deviate slightly from this idiom and incorporate not just more Emacs-ish keybindings but also some Vim ones! My main reasoning for this is that evil-mode is very popular, and from time to time I have recourse to using Vim over ssh, and the `jkhl` setup does feel pretty natural.

View-mode has been part of Emacs forever, but its default keybindings feel a bit... antiquated. **z** and **w** for scroll up and down? Space for page scroll, Return for forward one line, etc. (Maybe it works in the latter case, as it is more of a legacy Unixy vibe, but really I think we should be using more Emacs-ish keybindings, and as an option, why not Vim ones, as they tend to be mutually exclusive)


## The Setup {#the-setup}

Of course, view-mode is completely customizable, so here's my setup that combines the best of both worlds, familiar Emacs patterns and efficient Vim-style navigation:

```elisp
;; Enable view-mode when entering read-only
(setq view-read-only t)

;; Enhanced keybindings
(with-eval-after-load 'view
  ;; More Emacs-ish keys

  ;; Navigation
  (define-key view-mode-map (kbd "n") 'next-line)
  (define-key view-mode-map (kbd "p") 'previous-line)
  (define-key view-mode-map (kbd "f") 'forward-char)
  (define-key view-mode-map (kbd "b") 'backward-char)

  ;; Beginning/end of line
  (define-key view-mode-map (kbd "a") 'beginning-of-line)
  (define-key view-mode-map (kbd "e") 'end-of-line)

  ;; Vim-ish keys

  ;; Quick exit to edit mode
  (define-key view-mode-map (kbd "i") 'View-exit)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "l") 'forward-char)

  ;; Page movement
  (define-key view-mode-map (kbd "u") '(lambda()
                                         (interactive)
                                         (View-scroll-page-backward 3)))
  (define-key view-mode-map (kbd "d") '(lambda()
                                         (interactive)
                                         (View-scroll-page-forward 3)))

  ;; Beginning/end of line (Vim style)
  (define-key view-mode-map (kbd "0") 'beginning-of-line)
  (define-key view-mode-map (kbd "$") 'end-of-line)

  ;; Beginning/end of buffers
  (define-key view-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key view-mode-map (kbd "G") 'end-of-buffer)

  ;; Other bespoke bindings
  (define-key view-mode-map (kbd ";") 'other-window)

  (define-key view-mode-map (kbd "SPC") 'nil))

;; Quick toggle keys
(global-set-key (kbd "C-<escape>") 'view-mode)
(global-set-key (kbd "C-<tab>") 'view-mode)

;; Optional: return to view-mode after saving
(add-hook 'after-save-hook
          (lambda ()
            (when (and buffer-file-name (not view-mode))
              (view-mode 1))))

;; Visual feedback - box cursor in view mode, bar when editing
(add-hook 'view-mode-hook
          (defun view-mode-hookee+ ()
            (setq cursor-type (if view-mode 'box 'bar))))
```


## Baby Proofing {#baby-proofing}

{{< figure src="/emacs/20250731123820-emacs--Discovering-View-Mode-Emacss-Hidden-Modal-Editing-Gem.jpg" width="100%" >}}

Here's where it gets interesting (possibly). I have a baby, and babies have this amazing ability to slam their tiny hands down on keyboards at the most inopportune moments. Nothing quite like a random key smash right in the middle of an important file!. Wouldn't it be great to have a protective read-only shield over a file? So I decided, why not put all files into view-mode by default?, this way, accidental keypresses won't modify anything, and I can intentionally switch to edit mode only when I actually need to make changes.

```elisp
;; Enable view-mode for files by default
(add-hook 'find-file-hook
          (lambda ()
            (unless (or (derived-mode-p 'dired-mode))
              (view-mode 1))))
```

I'm currently using the automatic activation to see how it goes, but I have to say I'm not completely comfortable with setting files to read-only mode. Although more baby-proof, it does mean I have to activate edit mode (or deactivate view-mode) each time I want to edit a file and that doesn't feel very Emacsy to me.


## Cursor {#cursor}

You will notice that in the setup there is a modification of the cursor depending on the mode state. Although the modeline will indicate if view-mode is on or off, I'm actually quite liking the differentiation in editing modes to be reflected in the cursor. It is simple, and my brain is starting to recognize instinctively which mode I am in with no extra visual baggage.


## Subtleties {#subtleties}

It is also worth pointing out some more subtleties:

-   I have added the enabling of view-mode every time a file is saved. I am a habitual saver, I have no idea why (oh wait, habit!), so that means having to enter "edit mode" quite often. We shall see how this goes, but currently it means that I am not having to often flit back to view-mode after some characters are inserted as I would have saved and returned anyway!

-   In addition, there is a sneaky `(setq view-read-only t)` which means that toggling a file to and from read-only via `C-x C-q` will activate `view-mode` accordingly. I'm not really using this at the moment, but it could come in useful if I start thinking more in a `dired` manner, where I activate the edit mode in dired quite often to change filenames, so this could be muscle memory transferable. In the main, though, I don't think setting this is going to hurt anything.

-   Another subtlety is the lambdas set for the page scroll. `view-mode` has quite a neat implementation for scrolling, in that, with a prefix or a defun argument, the number of lines for a scroll can be set. I don't often like to scroll full pages or even half pages, so I typically have set only a few lines for faster scrolling, this of course is configurable to your own tastes.

-   You can see that I have added in all the basic Emacs and Vim typical navigational keys, and in the case of Emacs, stripped of the modifier. This, for me, feels much more natural, and strangely, I find myself naturally jumping between the Vim and Emacs keys, for shame!

-   I have already disabled a key within the view-mode mode-map and that is SPC, I found myself often (so far) pressing space without toggling off view-mode and that would scroll one page down - annoying!

-   If I am using the vim keys often and it might be likely I am viewing files, maybe code files and maybe I have a single horizontal split, I might just like to have a single key to jump back and forward between windows.  I chose ";" for a few reasons but the main one is that the key is adjacent to the other vim navigation keys.

It is worth noting, of course, that all normal Emacs navigational keybindings are still available, so you also have that option too within view-mode.


## Foibles {#foibles}

So far I have noticed some, lets say, foibles

-   It is not uncommon that I would want changes that I have just made and saved to be undone, but when undoing I am in view-mode, and the reversal is rejected due to the file being read only!, I might have to think about this one.

-   In org-mode, with the speed keys enabled which gives you a range of org commands from a single character when the point is at the heading start obviously can cause a conflict.  For example, "s" is mapped in speed keys to narrow section, but in view-mode mode it is an incremental search.


## Improvements? {#improvements}

-   One Idea is maybe I can auto-deactivate view-mode when a typical Emacs edit keybinding is detected, but this is fraught with edge cases, but maybe.

-   Do I go further with a vim paradigm? I'm not going to lie here I like the "x" to delete a character and "dd" to delete a whole line (C-k (kill-line) is something I use all the time).  Could I add these in without having to break out of view-mode and hence preserving a flow for fast edits?, not sure.


## Conclusion {#conclusion}

Anyways, the result is a surprisingly pleasant modal editing experience so far that already feels natural in Emacs while giving me the ergonomic benefits of Vim-style navigation and comfortable single-key navigation options.

Plus, my codebase is now baby-resistant!
