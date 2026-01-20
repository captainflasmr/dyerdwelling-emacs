---
title: "Ollama-Buddy 0.13.1: Curl backend support and some optimizations"
author: ["James Dyer"]
lastmod: 2025-06-21T10:05:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

This is more of a maintenance update. The main headline is the addition of an option to select a curl-based backend for those who may encounter networking issues. By default, ollama-buddy uses built-in low-level networking calls, but if you have network issues, you can now switch to curl!

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-06-21 Sat&gt; </span></span> **0.13.1** {#0-dot-13-dot-1}

Refactored content register processing to be more efficient and added a new Emacs package brainstorming prompt file.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-06-15 Sun&gt; </span></span> **0.13.0** {#0-dot-13-dot-0}

Added curl communication backend with fallback support

-   Added ollama-buddy-curl.el as separate backend implementation
-   Implemented backend dispatcher system in ollama-buddy-core.el
-   Updated all async functions to use backend dispatcher
-   Added curl backend validation and testing functions
-   Maintained full compatibility with existing network process backend

When building Emacs packages that communicate with external services, network connectivity can sometimes be a pain point. While Emacs's built-in `make-network-process` works great in most cases, some users have encountered issues on certain systems or network configurations. That's why now I have added a curl-based communication backend to give you an additional option, who knows, maybe it will solve your ollama communication issues!

The original ollama-buddy implementation relied exclusively on Emacs's native network process functionality. While this works well for most users, I occasionally heard from users who experienced network process failures/flakiness on certain systems.

Rather than trying to work around these edge cases in the network process code, I took a different approach: I added curl as an alternative communication backend! This gives users a battle-tested, widely-available HTTP client as a fallback option.

Users can enable the curl backend with a simple customization:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :config
  ;; Load curl backend first
  (require 'ollama-buddy-curl nil t)

  ;; Then set the backend
  (setq ollama-buddy-communication-backend 'curl))
```

and then switch backends from the chat buffer `C-c e`

The curl backend supports everything the network backend does:

-   Real-time streaming responses
-   Vision model support with image attachments
-   File attachments and context
-   All Ollama API parameters
-   Multishot model sequences

If curl is selected but not available, the system automatically falls back to the network process with a helpful warning message.

From a user perspective, the backend choice is largely transparent. The main indicators are:

-   Status line shows `[C]` for curl or `[N]` for network
-   Process list shows `ollama-chat-curl` vs `ollama-chat-stream` processes
-   Curl backend shows "Curl Processing..." in status messages

Everything else - streaming behaviour, response formatting, error handling - works identically regardless of the backend.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-31 Sat&gt; </span></span> **0.12.1** {#0-dot-12-dot-1}

Optimized Unicode escape function to fix blocking with large file attachments

-   Fixed UI blocking when sending large attached files
-   Used temp buffer with delete-char/insert instead of repeated concat calls
-   Reduced processing time from minutes to milliseconds for large payloads
