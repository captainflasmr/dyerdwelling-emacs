---
title: "Debugging Software Breakage with Git Stash and Emacs"
author: ["James Dyer"]
lastmod: 2025-09-10T08:00:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250826103741-emacs--Debugging-Software-Breakage-with-Git-Stash-and-Emacs.jpg"
---

We've all been there, your code was working perfectly from a clean checkout, but after making a bunch of changes across multiple files, something has broken. The dreaded question arises! which change caused the break? This is the story of how a debugging session led me to discover gaps in Emacs' VC mode and ultimately create a custom solution.

<!--more-->

{{< figure src="/emacs/20250826103741-emacs--Debugging-Software-Breakage-with-Git-Stash-and-Emacs.jpg" width="100%" >}}

I started with a clean, working codebase. After implementing several features across different files, my software suddenly stopped working. The classic debugging nightmare, multiple changes, one (or more) breaking changes, and no clear path to the culprit.

My debugging strategy as always is methodical. Over many years of software engineering I have learnt that you just need to figure out a systematic approach and then just get on with it!

1.  Start from the known-good base version
2.  Gradually reintroduce changes from my working set
3.  Test after each addition to identify the breaking point

Git stash turned out to be perfect for this workflow. Firstly I stashed all my changes, giving me a clean working directory to start from. My plan was to selectively apply portions of the stash, testing after each addition.

Using Emacs' built-in VC mode, I could use `vc-git-stash-show` to display my stashed changes in a diff buffer. From there, I could navigate through the files and selectively apply hunks using Emacs' diff mode commands. This gave me fine-grained control over which changes to reintroduce.

As I progressed through applying changes, I realised that I would really like to keep an eye on what changes remained in my stash compared to my current working directory, basically like a dynamic diff to be regenerated after each application (like typically on an individual file using ediff). This would allow me to keep an eye on likely culprits as I move through the hunking process.

In pure Git, this is straightforward:

```bash
git diff stash@{0}
```

But Emacs' VC mode doesn't provide a command for this specific operation (I have found this not to be uncommon for Emacs vc-mode, but I still like it anyways!)

Generally I think, Emacs' VC interface is designed to be VCS agnostic, which is both a strength and a limitation. While it provides excellent abstractions for common operations like `vc-diff`, it doesn't expose Git specific features like comparing against stash references.

The available VC commands were:

-   `vc-diff` - compares working directory with HEAD or between revisions
-   `vc-git-stash-show` - shows the diff of a stash

But no "diff working directory against stash" command

Now, it's worth noting that Magit, does apparently provide this functionality, but I prefer to run on air-gapped systems (yes, that again!) where installing external packages isn't always practical or desired. In such environments, I lean heavily on Emacs' built-in functionality and augment it with custom elisp when needed which is probably something I suspect I am likely to do in this case.

I had an initial eshell idea on how to accomplish this!, for example you can redirect command line output to Emacs buffers using the `#<buffer name>` syntax, so lets try that!

I tried:

```bash
git diff stash@{0} > #<buffer *git-diff*> && diff-mode
```

This almost worked, but I encountered a timer error related to eshell's command chaining.

and then I tried:

```bash
git diff stash@{0} > #<buffer *git-stash-diff*> ; diff-mode
```

After some experimentation, I still couldn't quite get eshell to generate a buffer from a command and then initiate a mode.  Of course I could just jump to the buffer and run it myself, but generally I wanted a solution to be easily repeatable.

Right, lets scrap the eshell idea and lets fall back on my tried and tested method of writing a defun in elisp!:

```elisp
(defun my-git-diff-stash (stash-ref)
  "Diff working directory against specified stash"
  (interactive "sStash reference (e.g., 0, 1, 2): ")
  (let ((buffer (get-buffer-create "*git-stash-diff*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process "git" nil buffer t "diff" (format "stash@{%s}" stash-ref))
      (diff-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))
```

This function:

-   Prompts for a stash reference (defaulting to numeric input like 0, 1, 2)
-   Creates a dedicated buffer for the diff
-   Runs `git diff` against the specified stash
-   Automatically applies `diff-mode` for syntax highlighting
-   Opens the buffer and positions the cursor at the beginning

The final step was to bind this command to the VC prefix map:

```elisp
(define-key vc-prefix-map (kbd "S") 'my-git-diff-stash)
```

"S" is currently used for a regex search of some kind which I currently don't understand and hence am not using.

Now I can use `C-x v S` to quickly diff my working directory against any stash (although who knows when I will need this again!)

With this in place, my debugging workflow became smoother

1.  Stash all changes
2.  Apply changes incrementally using `vc-git-stash-show`
3.  Test the software after each addition
4.  When it still works, check what remains `C-x v S`
5.  Continue applying changes from the remaining diff
6.  When it breaks, I have a good idea of the breaking issue

This experience taught me several valuable lessons:

1.  **VC mode's limitations**: While Emacs' VC interface is excellent for common operations, specialized Git workflows sometimes require custom solutions.

2.  **The value of built-in solutions**: Working in air-gapped environments has taught me to maximize Emacs' built-in capabilities before reaching for external packages. While Magit would have solved this problem out of the box, building the solution myself using VC mode and custom elisp keeps dependencies minimal and increases my understanding of both Git and Emacs internals.

3.  **Eshell's power**: The ability to redirect command output directly to Emacs buffers is incredibly useful, even if it has some quirks with command chaining and in the end I never really got it to work, but it is in my brain more concretely now as this blog post now exists!

4.  **Integration matters**: Binding custom functions to standard keymaps makes them feel like native features.
