---
title: "Streamlining Navigation in Org-Mode using an adapted org-goto"
author: ["James Dyer"]
lastmod: 2025-01-18T11:08:00+00:00
tags: ["org-goto", "org", "imenu", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241228131958-emacs--Streamlining-Navigation-in-Org-Mode.jpg"
---

Navigating through lengthy org files can sometimes feel cumbersome. To address this I have tweaked `org-goto` to be a little more usable through the minibuffer completion system to jump to any org heading.

I have been aware of the `org-goto` command for a while now, which technically, allows for the efficient navigation through an org file.  Every now and then I give it a go, but it just feels clunky and some of the keybindings don't quite feel intuitively Emacs for some reason.

Now my org files are getting more elaborate, I would like a better way to navigate and especially through the hierarchy of headings.

What about `org-imenu-depth` I hear you say!  Well as far as I can tell, this allows stepping through to a defined subheading level but while going through multiple menu/hierarchical selection steps.  For efficiency, I would like a way to flatten the subheadings for a one-shot completion selection method.  This is where `org-goto` can actually be adapted to satisfy this need, and I think leverages the org refile menu system.

<!--more-->

To achieve this, some variables will require changing, firstly:

```emacs-lisp
(setq org-goto-interface 'outline-path-completionp)
```

The `org-goto-interface` variable defines the interface you use to navigate when calling `org-goto`. By default, org goto uses a mixed interface with direct completion and an alternative outline-based navigation using buffers. I prefer the **outline-path-completionp** setting, which provides a breadcrumb-like structure for easier navigation through the minibuffer completion system.

By default, Org mode’s outline-path interface works in steps, which means you select one heading level at a time. I find this behaviour a bit clunky, especially for larger files. Fortunately, you can disable this step-by-step behaviour by setting `org-outline-path-complete-in-steps` to `nil` which flattens the presented org goto heading list when activated so you can see the full list of headings/subheadings in a single step and therefore be able to navigate directly to your desired location.

```emacs-lisp
(setq org-outline-path-complete-in-steps nil)
```

---

Lets demonstrate how this now works with an example.

Given the following org:

```org
* Heading 1
...
* Heading 2
...
** Subheading 2.1
...
* Subheading 3
...
** Subheading 3.1
...
*** Subheading 3.1.1
```

imenu would just allow navigation to leaf nodes and not the higher subdirectories.

(Note : I use `fido-mode=/=icomplete`):

Here is the minibuffer menu presented when `imenu` is activated by default:

```org
(Heading.1 | Heading.2 | *Rescan* | Subheading.3)
```

For example, I can navigate to Heading 1 but what about Heading 2 or Heading 3?, when selecting these headings that have subheadings it will take me into further leaf node menus for selection.

This is where the setup above for `org-goto` comes in handy as it now presents the following:

```org
{Heading 2/ | Heading 2/Subheading 2.1/ | Heading 1/ | Subheading 3/ | Subheading 3/Subheading 3.1/ | Subheading 3/Subheading 3.1/Subheading 3.1.1/}
```

Where of course the minibuffer completion system can complete to not just the leaf nodes but also the higher level subheadings, allowing navigation to any org heading!

{{< figure src="/emacs/20241228131958-emacs--Streamlining-Navigation-in-Org-Mode.jpg" width="100%" >}}
