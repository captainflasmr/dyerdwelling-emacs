---
title: "Ollama Buddy - In-Buffer LLM Streaming"
author: ["James Dyer"]
lastmod: 2026-03-12T11:09:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/ollama-buddy-logo.jpg"
---

There is now an in-buffer replace feature in [ollama-buddy](https://github.com/captainflasmr/ollama-buddy), so now an ollama response can work directly on your text, streaming the replacement in real-time, and giving you a simple accept/reject choice!, I have also added an smerge diff inline if desired to show the differences and give the user the ability to accept or reject

<!--more-->

{{< figure src="/emacs/ollama-buddy-logo.jpg" width="100%" >}}

Here is how it works : <https://www.youtube.com/watch?v=Po7Wqpk0sqY>

The feature is tucked away in the transient menu. Here's the workflow:

1.  Toggle it on via `C-c O` → Actions → `W`, or run `M-x ollama-buddy-toggle-in-buffer-replace`
2.  A small `✎` indicator appears in your header line, confirming the mode is active
3.  Select a region of text you want rewritten
4.  Invoke a command from the role menu; for example, `C-c o` then pick your rewrite command
5.  Watch as the AI streams the replacement directly into your buffer

It is also worth noting that each custom menu can be defined with a `:destination` option, for either `in-buffer` or `chat`, so the global in-buffer replace doesn't need to be selected, and the custom transient menu can be tailored for each command. For example, in the default custom menu, refactor code and proofread have the destination of `in-buffer` set, but actions like git commit message or describe code will fall back to the global option, which by default is `chat`, which is probably what you would want for these options.

During streaming, you'll see a dimmed, italic `[Rewriting...]` placeholder where your selection was. The new text then flows in with a highlight overlay.

Once the stream finishes, you're dropped into a minor mode with two options:

```text
C-c C-c → accept the changes (keep new text, clear highlighting)
C-c C-k → reject and restore your original text
```

The mode line helpfully shows `[Rewrite?]` while you're deciding. It's a bit like `ediff` but for AI-generated changes and uses smerge-mode.

What if the AI starts going off the rails halfway through? No problem. Press `C-c C-k` at any point during streaming and the network process stops immediately, restoring your original selection. No waiting for it to finish rambling!

Press `C-c d` and the original text gets inserted below the new text in the same buffer. Word-level differences are highlighted using `smerge-refine-regions`, so you can see exactly what changed at a glance.

Green overlays mark added or modified words. Red strikethrough overlays show what was removed. It's proper granular diffing, not just a before-and-after comparison.

This workflow is particularly useful for:

-   Refactoring code blocks you've already written
-   Rephrasing documentation or prose that feels clunky
-   Adjusting tone without losing your core message
-   Quick grammar and style improvements

It's less about "write this for me from scratch" and more "help me iterate on what I've already got."

For best results, I've found that smaller, focused selections work better than trying to rewrite entire files in one go. The AI has more context to work with, and you can make more granular decisions about what to keep.

Next up is probably some more polish on the diff highlighting, or perhaps exploring how this could work with multi-file projects. But for now, again I think this implementation is good enough.
