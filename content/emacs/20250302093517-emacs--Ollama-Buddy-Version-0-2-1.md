---
title: "Ollama Buddy Version 0.2.1 - Same prompt to multiple LLMs and choose best answer!"
author: ["James Dyer"]
lastmod: 2025-03-02T09:35:00+00:00
tags: ["ollama-buddy", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250224154229-emacs--Ollama-Buddy-Now-On-MELPA.jpg"
---

Some improvements to my ollama LLM package...

With the new **multishot mode**, you can now send a prompt to multiple models in sequence, and compare their responses, the results are also available in named registers.

<!--more-->

{{< figure src="/emacs/ollama-buddy-banner.jpg" width="100%" >}}

**Letter-Based Model Shortcuts**

Instead of manually selecting models, each available model is now assigned a **letter** (e.g., `(a) mistral`, `(b) gemini`). This allows for quick model selection when sending prompts or initiating a **multishot sequence**.

**Multishot Execution (`C-c C-l`)**

Ever wondered how different models would answer the same question? With **Multishot Mode**, you can:

-   Send your prompt to a sequence of models in one shot.
-   Track progress as responses come in.
-   Store each model’s response in a **register**, making it easy to reference later, each assigned model letter corresponds to the named register.

**Status Updates**

When running a multishot execution, the status now updates dynamically:

-   **"Multi Start"** when the sequence begins.
-   **"Processing..."** during responses.
-   **"Multi Finished"** when all models have responded.

**How It Works**

1.  **`C-c C-l`** to start a multishot session in the chat buffer.
2.  Type a sequence of model letters (e.g., `abc` to use models `mistral`, `gemini`, and `llama`).
3.  The selected models will process the prompt **one by one**.
4.  The responses will be saved to registers of the same named letter for recalling later.

{{< figure src="/emacs/ollama-buddy-screen-recording_007.gif" width="100%" >}}
