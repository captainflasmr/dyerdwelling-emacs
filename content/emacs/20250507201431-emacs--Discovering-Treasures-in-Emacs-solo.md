---
title: "Discovering Treasures in Emacs-solo"
author: ["James Dyer"]
lastmod: 2025-05-07T21:45:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250507201431-emacs--Discovering-Treasures-in-Emacs-solo.jpg"
---

While exploring [Emacs-solo](https://github.com/LionyxML/emacs-solo), I was struck by how closely its philosophy aligned with my own [Emacs-DIYer](https://github.com/captainflasmr/Emacs-DIYer) project. The parallels were sometimes uncanny, for example, we had independently chosen identical keybindings like "M-s g" for grep functionality. I had been also considering "M-s f" for my find variant and was looking for a home for `recentf`, potentially at "M-g r". There were also some ideas around using other finding tools such as `ripgrep` and `fd` as a callout to some external tools and there were many other similarities.

<!--more-->

I was particularly intrigued to see `newsticker` included, as I had recently adopted it as an `elfeed` replacement. The `ace-window` alternative implementation also closely mirrored my own single-defun approach.

So, with my first pass through, I have picked out the following goodies to be purloined and incorporated into `Emacs-DIYer`

-   dired icon replacements
-   Git gutter status overlay
-   Hide completion buffer when icomplete in buffer
-   ollama alternative directly from ansi-term

{{< figure src="/emacs/20250507201431-emacs--Discovering-Treasures-in-Emacs-solo.jpg" width="100%" >}}

---

Firstly, the dired icon replacement provides visual clarity without requiring the `all-the-icons` packages. While perhaps not essential, it gives me a comfy feel and I think these icons significantly improve the Emacs experience, particularly for frequent dired users like myself.

I pretty much just copied the entire `emacs-solo-dired-icons` section but stripped out the `use-package` as I'm trying to keep `Emacs-DIYer` compatible with older versions of Emacs.  This worked straight out the box (like all the other integrations) and I just needed to make sure I have the requisite font collection installed.

---

The git status indicator in the dired gutter was a welcome addition I hadn't previously implemented or even considered. It provides immediate visual feedback that feels natural and intuitive, especially coming from a background of extensive version control experience.

Having used `subversion` for over 20 years (before transitioning to `TortoiseSVN` and then `magit` and `vc`), I was already accustomed to status characters like "M" indicating modified files and other single key status key definitions seem consistent to my previous experience, and isn't it great to see them visible in dired!

In summary, this integration complements my existing modeline status indicator, along with `vc` and gives me both file-specific and directory-level version control visibility, noice!

---

I had been working on replacing `corfu/company` with in-buffer completion, but hadn't solved how to hide the completion buffer when using `icomplete`. My settings already closely matched those in Emacs-solo, but the critical missing piece was this elegant advice:

```elisp
(if icomplete-in-buffer
    (advice-add 'completion-at-point
                :after #'minibuffer-hide-completions))
```

Now it feels much cleaner and I might start using it rather than `corfu` from now on.

---

The final act of pilfering is to incorporate a simple `ollama` integration.

Before creating my [Ollama Buddy](https://github.com/captainflasmr/ollama-buddy) package, I had explored integrating `ollama` through a shell and I had also come to the conclusion that using `ansi-term` was really the only quick solution, but I just couldn't figure out the implementation details.  However, the experience ultimately led me to take an alternative approach, writing a small implementation to interface with `ollama` from an Emacs buffer, which eventually gave rise to `Ollama Buddy`.

The `Emacs-solo` `ansi-term` solution feels surprisingly well-integrated once you become comfortable switching between line and character modes. I've now incorporated this as an alternative to my API-based implementation, providing two no-dependency options for local LLM access!

I plan to further explore `Emacs-solo`, particularly for additional `vc` enhancements. This first pass has already yielded valuable improvements to `Emacs-DIYer`, and this has been a valuable exercise to learn from another project with such similar design principles.
