---
title: "Expanding Ollama Buddy: Mistral Codestral Integration"
author: ["James Dyer"]
lastmod: 2025-12-11T08:19:00+00:00
tags: ["ollama-buddy", "llm", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions_001.jpg"
---

Ollama Buddy now supports Mistral's Codestral - a powerful code-generation model from Mistral AI that seamlessly integrates into the ollama-buddy ecosystem.

<!--more-->

<https://github.com/captainflasmr/ollama-buddy>

<https://melpa.org/#/ollama-buddy>

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions_001.jpg" width="100%" >}}

So now we have:

-   **Local Ollama models** — full control, complete privacy
-   **OpenAI** — extensive model options and API maturity
-   **Claude** — reasoning and complex analysis
-   **Gemini** — multimodal capabilities
-   **Grok** — advanced reasoning models
-   **Codestral** — specialized code generation **NEW**

To get up and running...

First, sign up at [Mistral AI](https://console.mistral.ai/) and generate an API key from your dashboard.

Add this to your Emacs configuration:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-codestral-api-key
   (auth-source-pick-first-password :host "ollama-buddy-codestral" :user "apikey"))
  :config
  (require 'ollama-buddy-codestral nil t))
```

Once configured, Codestral models will appear in your model list with an `s:` prefix (e.g., `s:codestral-latest`). You can:

-   Select it from the model menu (`C-c m`)
-   Use it with any command that supports model selection
-   Switch between local and cloud models on-the-fly
