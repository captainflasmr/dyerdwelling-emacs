---
title: "Ollama-Buddy 0.9.17: Claude Support, Asynchronous Operations and Responses to Registers"
author: ["James Dyer"]
lastmod: 2025-04-02T17:15:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg"
---

I’ve added experimental support for Claude AI, aligning its implementation with my support for ChatGPT, to pave the way for external AI templating which should make it easier to integrate new models in the future. Like the ChatGPT implementation, it doesn’t stream tokens in real time but instead outputs the final result all at once. However, after testing it further, I’m actually starting to prefer it. The response speed is so fast that not seeing a real-time “Typing” indicator isn’t a big deal. I think that streaming feels more relevant for local LLMs running through `ollama`, where token generation is slower, making real-time output more useful.

<!--more-->

{{< figure src="/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg" width="100%" >}}

Secondly, the Texinfo manual for this package now magically installs itself when pulling from MELPA. I fluked this!, I just thought it was sensible to create a `docs` directory and then plonked an info file there. After looking into it, I found that MELPA performs some extra processing when handling Emacs documentation. It automatically scans common documentation directories like `docs` and grabs the `.info` file, pretty neat! This means you can now browse `ollama-buddy`'s functionality directly through `info`.

Next up, I realized I wasn’t efficiently handling `ollama` model operations like delete, pull, and copy. I was currently process-calling `ollama` and passing through arguments as if I was on the command line.  This was functional, but wasn’t ideal and wasn't stimulating the `ollama` API for these operations correctly, or even at all.  After reassessing my design, I came to the realization that I was in fact using four different methods to communicate with `ollama`:

1.  `curl`
2.  Direct process calls
3.  `url.el`
4.  `make-network-process`

At first, I leaned on `curl` since it was straightforward and matched the official `ollama` examples. My approach with a project such as this is generally to get things working quickly and then refine/iterate later. However, once I had a solid design (and design principles!), I wanted to eliminate external dependencies like `curl`. This lead me to explore `url.el`, but initially I couldn't seem to get my head round it / get it working - and I decided to go for the nuclear option of `make-network-process` for network-level flexibility.  Later, I revisited `url.el` for Claude and ChatGPT support, rewriting the implementation to use `url-retrieve`, but decided generally to keep `make-network-process` for `ollama` interactions as it was still Emacs built-in and actually I'm more familiar with the lower level network concepts as having wrestled with them over many years at work.

Anyway, back to the stimulating of the `ollama` API for model operations.

I considered leveraging my existing `url.el` based `ollama-buddy--make-request` function, but quickly realized it used `url-request-data`, which blocks execution. This wasn't an issue for quick requests like model info or tags (although could be), but for long-running tasks like model pulls and general model operations it risked freezing Emacs!

Switching to `url-retrieve` solved this, as it runs asynchronously, however, `url-retrieve` only triggers its callback at the end of the request, making real-time progress tracking difficult. To address this, I implemented `run-with-timer`, ensuring persistent status updates in the header-line which now allows for multiple operations, including pull, copy, delete, even simultaneously.

Now that I’m using `ollama-buddy` as my primary Emacs AI assistant, I’m refining my AI workflow. Since my design is centered around the chat buffer, all interactions and outputs end up there. But what about quick tasks like proofreading text or refactoring code? Ideally, I want a workflow that aligns with `ollama-buddy`'s principles, meaning no direct in-buffer edits (though future me might change their mind!).

For example, if I want to tighten a rambling paragraph (is this one? :), I currently send it via the custom menu to the chat buffer with a proofreading tag. However, retrieving the output requires jumping to the chat buffer, copying it, switching back, deleting the original, and then pasting the revision - too many steps.

To streamline this, I’ve now implemented a feature that writes the latest response to a customizable register, this way, I can simply delete the original text and insert the improved version without extra navigation.

Note: I have remapped the `insert-register` default keybinding of `C-x r i` to `M-a`, as by default I am writing to register ?a and `M-a a` seems more comfortable.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-01 Tue&gt; </span></span> **0.9.17** {#0-dot-9-dot-17}

-   Added link to `ollama-buddy` info manual from the chat buffer and transient menu as MELPA has now picked it up and installed it!


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-28 Fri&gt; </span></span> **0.9.16** {#0-dot-9-dot-16}

-   Added `ollama-buddy-fix-encoding-issues` to handle text encoding problems.
-   Refactored and streamline fabric pattern description handling.
-   Removed unused fabric pattern categories to enhance maintainability.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-28 Fri&gt; </span></span> **0.9.15** {#0-dot-9-dot-15}

-   Implement asynchronous operations for model management
    -   Introduce non-blocking API requests for fetching, copying, and deleting models
-   Add caching mechanisms to improve efficiency
    -   Cache model data to reduce redundant API calls
    -   Manage cache expiration with timestamps and time-to-live settings
-   Update status line to reflect ongoing background operations
-   Ensure smooth user interaction by minimizing wait times and enhancing performance


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-26 Wed&gt; </span></span> **0.9.13** {#0-dot-9-dot-13}

-   Added automatic writing of last response to a register
-   Added M-r to search through prompt history

I was just thinking about a general workflow aspect and that is getting responses out of the `ollama-buddy` chat buffer.  Of course if you are already there then it will be easier, but even then the latest prompt, which is probably the one you are interested in will still have to be copied to the kill ring.

This issue is even more pronounced when you are sending text from other buffers to the chat.

So, the solution I have put in place is to always write the last response to a register of your choice.  I always think registers are an underused part of Emacs, I already have repurposed them for the multishot, so why not always make the last response available.

For example, you want to proofread a sentence, you can mark the text, send to the chat using the custom menu to proofread then the response will be available in maybe register "a".  The chat buffer will be brought up if not already visible so you can validate the output, then pop back to your buffer, delete the paragraph and insert the register "a"?, maybe.  I am going to put this in as I suspect no-one uses registers anyway and if they do, they can push the response writing register away using `ollama-buddy-default-register`, I don't think this will do any harm, and actually it is something I may starting using more often.

As a side note, I also need to think about popping into the chat buffer with a buffer text push to the chat, should I do it?, not sure yet, still getting to grips with the whole workflow aspect, so will need a little more time to see what works.

Also as a side note to this ramble, the general register prefix is annoyingly long `C-x r i <register>` so I have rebound in my config to `M-a`, as I never want to go back a sentence and also if I just write to the default "a" register then it feels ergonomically fast.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-25 Tue&gt; </span></span> **0.9.12** {#0-dot-9-dot-12}

-   Added experimental Claude AI support!
-   removed curl and replaced with url.el for online AI integration

A very similar implementation as for ChatGPT.

To activate, set the following:

```elisp
(require 'ollama-buddy-claude nil t)
(ollama-buddy-claude-api-key "<extremely long key>")
```
