---
title: "Ollama-Buddy 0.7.1 - Org-mode Chat, Parameter Control and JSON Debugging"
author: ["James Dyer"]
lastmod: 2025-03-11T18:07:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", "ai", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250311180746-emacs--Ollama-Buddy-0-7-1-Org-mode-Chat-Parameter-Control-and-JSON-Debugging.jpg"
---

Continuing the development of my local `ollama` LLM client called `ollama-buddy`...

<https://github.com/captainflasmr/ollama-buddy>

The basic functionality, I think, is now there (and now literally zero configuration required).  If a default model isn't set I just pick the first one, so LLM chat can take place immediately.

<!--more-->

{{< figure src="/emacs/20250311180746-emacs--Ollama-Buddy-0-7-1-Org-mode-Chat-Parameter-Control-and-JSON-Debugging.jpg" width="100%" >}}

Now I'm getting more into this chat client malarkey, my original idea of a very minimal chat client to interface to `ollama` is starting to skew into supporting as much of the `ollama` RESTful API as possible.  Hence in this update a more advanced approach is creeping in, including setting up various subtle model parameters and providing a debugging window to monitor incoming raw JSON (pretty printed of course).  Hopefully, these features will remain tucked away for advanced users, I’ve done my best to keep them unobtrusive (but not **too** hidden). The tool is still designed to be a helpful companion to interface to `ollama` through Emacs, just now with more powerful options under the hood.

Also a note about converting the chat buffer into org-mode.  My original intention was to keep the chat buffer as a very simple almost "no mode" buffer, with just text and nothing else.  However, with more consideration, I felt that converting this buffer into org-mode actually held quite a few benefits:

-   Each prompt could be a heading, hence outlining and folding can be activated!
-   Navigation between prompts now comes for free (especially if you are using `org-use-speed-commands`)
-   The org ox export backend now allows us to export to formats of many different kinds

I'm sure there are more as this list isn't quite the "quite a few benefits" I was hoping for :(

I have a local keymap defined with some ollama-buddy specific keybindings, and as of yet I haven’t encountered any conflicts with commonly used `org-mode` bindings but we shall see how it goes. I think for this package it is important to have a quick chatting mechanism, and what is faster than a good keybind?

Finally, just a note on the pain of implementing a good prompt mechanism.  I had a few goes at it and I think I now have an acceptable robust solution.  I kept running into little annoying edge cases and I ended up having to refactor quite a bit.  My original idea for this package involved a simple “mark region and send” as at the time I had a feeling that the implementation of a good prompt mechanism would be tough - how right I was!.  Things got even trickier with the move to `org-mode`, since each prompt heading should contain meaningful content for clean exports and I had to implement a mechanism to replace prompts intelligently. For example, if the model is swapped and the previous prompt is blank, it gets replaced, though, of course, even this has its own edge cases - gives a new meaning to prompt engineering! :)

Anyways, listed below are my latest changes, with a little deeper dive into more "interesting" implementations, my next ideas are a little more advanced and are kanban'd into my github README at <https://github.com/captainflasmr/ollama-buddy> for those that are interested.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-11 Tue&gt; </span></span> **0.7.1** {#0-dot-7-dot-1}

Added debug mode to display raw JSON messages in a debug buffer

-   Created new debug buffer to show raw JSON messages from Ollama API
-   Added toggle function to enable/disable debug mode (ollama-buddy-toggle-debug-mode)
-   Modified stream filter to log and pretty-print incoming JSON messages
-   Added keybinding C-c D to toggle debug mode
-   Updated documentation in welcome message


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-11 Tue&gt; </span></span> **0.7.0** {#0-dot-7-dot-0}

Added comprehensive Ollama parameter management

-   Added customization for all Ollama option API parameters with defaults
-   Only send modified parameters to preserve Ollama defaults
-   Display active parameters with visual indicators for modified values
-   Add keybindings and help system for parameter management
-   Remove redundant temperature controls in favor of unified parameters

Introduced parameter management capabilities that give you complete control over your Ollama model's behavior through the options in the ollamas API.

Ollama's API supports a rich set of parameters for fine-tuning text generation, from controlling creativity with `temperature` to managing token selection with `top_p` and `top_k`. Until now, Ollama Buddy only exposed the `temperature` parameter, but this update unlocks the full potential of Ollama's parameter system!


### Key Features: {#key-features}

-   **All Parameters** - set all custom options for the ollama LLM at runtime
-   **Smart Parameter Management**: Only modified parameters are sent to Ollama, preserving the model's built-in defaults for optimal performance
-   **Visual Parameter Interface**: Clear display showing which parameters are active with highlighting for modified values


## Keyboard Shortcuts {#keyboard-shortcuts}

Parameter management is accessible through simple keyboard shortcuts from the chat buffer:

-   `C-c P` - Edit a parameter
-   `C-c G` - Display current parameters
-   `C-c I` - Show parameter help
-   `C-c K` - Reset parameters to defaults


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-10 Mon&gt; </span></span> **0.6.1** {#0-dot-6-dot-1}

Refactored prompt handling so each org header line should now always have a prompt for better export

-   Added functionality to properly handle prompt text when showing/replacing prompts
-   Extracted inline lambdas in menu actions into named functions
-   Added fallback for when no default model is set


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-08 Sat&gt; </span></span> **0.6.0** {#0-dot-6-dot-0}

Chat buffer now in org-mode

-   Enabled `org-mode` in chat buffer for better text structure
-   Implemented `ollama-buddy--md-to-org-convert-region` for Markdown to Org conversion
-   Turn org conversion on and off
-   Updated keybindings `C-c C-o` to toggle Markdown to Org conversion

**Key Features**

1.  The chat buffer is now in `org-mode` which gives the buffer enhanced readability and structure. Now, conversations automatically format user prompts and AI responses with **org-mode headings**, making them easier to navigate.

2.  Of course with org-mode you will now get the additional benefits for free, such as:
    -   outlining
    -   org export
    -   heading navigation
    -   source code fontification

3.  Previously, responses in **Ollama Buddy** were displayed in markdown formatting, which wasn’t always ideal for **org-mode users**. Now, you can automatically convert Markdown elements, such as bold/italic text, code blocks, and lists, into proper org-mode formatting.  This gives you the flexibility to work with markdown or org-mode as needed.
