---
title: "Ollama-Buddy 0.9.38: Unload Models, Hide AI Reasoning, and Clearly View Modified Parameters on each request"
author: ["James Dyer"]
lastmod: 2025-05-01T13:33:00+01:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

More improvements to `ollama-buddy` <https://github.com/captainflasmr/ollama-buddy>

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-29 Tue&gt; </span></span> **0.9.38** {#0-dot-9-dot-38}

Added model unloading functionality to free system resources

-   Add unload capability for individual models via the model management UI
-   Create keyboard shortcut (C-c C-u) for quick unloading of all models
-   Display running model count and unload buttons in model management buffer

Large language models consume significant RAM and GPU memory while loaded. Until now, there wasn't an easy way to reclaim these resources without restarting the Ollama server entirely. This new functionality allows you to:

-   Free up GPU memory when you're done with your LLM sessions
-   Switch between resource-intensive tasks more fluidly
-   Manage multiple models more efficiently on machines with limited resources
-   Avoid having to restart the Ollama server just to clear memory

There are several ways to unload models with the new functionality:

1.  **Unload All Models**: Press `C-c C-u` to unload all running models at once (with confirmation)

2.  **Model Management Interface**: Access the model management interface with `C-c W` where you'll find:
    -   A counter showing how many models are currently running
    -   An "Unload All" button to free all models at once
    -   Individual "Unload" buttons next to each running model

3.  **Quick Access in Management Buffer**: When in the model management buffer, simply press `u` to unload all models

The unloading happens asynchronously in the background, with clear status indicators so you can see when the operation completes.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-25 Fri&gt; </span></span> **0.9.37** {#0-dot-9-dot-37}

-   Display modified parameters in token stats

Enhanced the token statistics section to include any modified parameters, providing a clearer insight into the active configurations. This update helps in debugging and understanding the runtime environment.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-25 Fri&gt; </span></span> **0.9.36** {#0-dot-9-dot-36}

Added Reasoning/Thinking section visibility toggle functionality

-   Introduced the ability to hide reasoning/thinking sections during AI responses, making the chat output cleaner and more focused on final results
-   Added a new customizable variable `ollama-buddy-hide-reasoning` (default: nil) which controls visibility of reasoning sections
-   Added `ollama-buddy-reasoning-markers` to configure marker pairs that encapsulate reasoning sections (supports multiple formats like &lt;think&gt;&lt;/think&gt; or ----)
-   Added `ollama-buddy-toggle-reasoning-visibility` interactive command to switch visibility on/off
-   Added keybinding `C-c V` for toggling reasoning visibility in chat buffer
-   Added transient menu option "V" for toggling reasoning visibility
-   When reasoning is hidden, a status message shows which section is being processed (e.g., "Think..." or custom marker names)
-   Reasoning sections are automatically detected during streaming responses
-   Header line now indicates when reasoning is hidden with "REASONING HIDDEN" text
-   All changes preserve streaming response functionality while providing cleaner output

This feature is particularly useful when working with AI models that output their "chain of thought" or reasoning process before providing the final answer, allowing users to focus on the end results while still having the option to see the full reasoning when needed.
