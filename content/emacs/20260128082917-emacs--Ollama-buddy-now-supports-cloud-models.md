---
title: "Ollama buddy now supports cloud models!"
author: ["James Dyer"]
lastmod: 2026-01-28T08:29:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

Having another look at my AI assistant - ollama-buddy, its been a while and it seems ollama has moved on since I started creating this package last year, so I have developed a new roadmap and the first step is to add ollama cloud models!

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}

Here are some references to the project, including a youtube channel where I upload ollama-buddy demonstrations:

<https://melpa.org/#/ollama-buddy>

<https://github.com/captainflasmr/ollama-buddy>

Here is the changelog for the cloud model implementation:


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2026-01-28 Wed&gt; </span></span> **1.1** {#1-dot-1}

Added Ollama Cloud Models support

-   Cloud models (running on ollama.com infrastructure) now work seamlessly
-   `ollama-buddy-cloud-signin` to automatically open browser for authentication
-   Cloud models are proxied through the local Ollama server which handles authentication
-   Use `C-u C-c m` or transient menu "Model &gt; Cloud" to select cloud models
-   Status line shows ☁ indicator when using a cloud model
-   Available cloud models include: qwen3-coder:480b-cloud, deepseek-v3.1:671b-cloud, gpt-oss:120b-cloud, minimax-m2.1:cloud, and more
