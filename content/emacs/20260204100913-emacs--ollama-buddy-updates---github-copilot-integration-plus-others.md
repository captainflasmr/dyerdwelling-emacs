---
title: "Spent a bit of free time polishing ollama-buddy - github Copilot is now onboard!"
author: ["James Dyer"]
lastmod: 2026-02-04T10:40:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

I've had a little free time recently (figuring out this baby stuff!) and thought I would spend time revisiting and refining my AI assistant [ollama-buddy](https://github.com/captainflasmr/ollama-buddy)

I've been playing around with agentic coding and keeping up-to-date on the rapid development of the Emacs AI package landscape and I think I have refined in my own mind my idea of what I would like to see in an Emacs AI assistant.

The headline change regarding the latest release of ollama-buddy is GitHub Copilot integration;  the rest of the work is about smoothing the UI and simplifying day-to-day use.

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}

What’s new - the Copilot addition (v1.2)

-   GitHub Copilot Chat API support via a new file, ollama-buddy-copilot.el, so Copilot models can be used alongside your existing providers.
-   Authentication uses GitHub’s device flow (OAuth). No API key required: M-x ollama-buddy-copilot-login opens a browser and guides you through secure authentication.
-   Copilot models are identified with a "p:" prefix (for example, p:gpt-4o). The header line shows a "p" indicator when the Copilot provider is loaded so you always know it’s available.
-   Copilot access exposes a broad set of models from multiple vendors through the Copilot interface: OpenAI (gpt-4o, gpt-5), Anthropic (claude-sonnet-4, claude-opus-4.5), Google (gemini-2.5-pro), and xAI models.
-   Quick usage notes:
    1.  Ensure you have an active GitHub Copilot subscription.
    2.  Run M-x ollama-buddy-copilot-login.
    3.  Enter the device code in your browser at github.com/login/device when prompted.
    4.  Select a Copilot model with C-c m (e.g., p:gpt-4o).
-   Example config to load Copilot support:

<!--listend-->

```nil
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :config
  (require 'ollama-buddy-copilot nil t))
```

Other notable updates in this release series

-   **v1.2.1 (2026-02-02)**
    -   Attachment count indicator on the header line so you get a constant visual reminder that the session has attachments.
-   **v1.1.5 (2026-01-31)**
    -   Global system prompt feature (enabled by default): sets a baseline set of instructions (for example, to prefer plain prose and avoid markdown tables) that is prepended to session-specific system prompts. This helps keep responses consistent across providers and things like malformed markdown tables for example, which seems to be common. There’s a toggle (ollama-buddy-global-system-prompt-enabled) and a quick command to flip it (ollama-buddy-toggle-global-system-prompt), plus a transient-menu entry.
    -   Consolidated model management: streamlined into a single model management buffer (C-c W) and the welcome screen now points to that buffer for model tasks.
-   **v1.1.4 (2026-01-31)**
    -   Header-line and keybinding cleanup: C-c RET to send prompts (matches gptel, as I feel this seems intuitive), removed a redundant backend indicator, shortened the markdown indicator to "MD", and fixed markdown → org heading conversion to keep structure sane.
-   **v1.1.3 (2026-01-31)**
    -   Chat UX improvements and simplification: added ollama-buddy-auto-scroll (default nil — don’t auto-scroll so you can read while streaming) and ollama-buddy-pulse-response (flashes the response on completion, taking from gptel again, as if there is no autoscrolling it is useful to visually see when the response has completed). Removed the model name coloring feature and related toggles to simplify code and improve org-mode performance.
-   **v1.1.2 (2026-01-30)**
    -   Streamlined welcome screen and model selection, clearer provider indicators in the header line and an improved list of enabled online LLM providers.
