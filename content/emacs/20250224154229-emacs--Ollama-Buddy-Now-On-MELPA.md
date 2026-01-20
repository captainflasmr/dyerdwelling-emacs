---
title: "Ollama Buddy - Now On MELPA!"
author: ["James Dyer"]
lastmod: 2025-02-24T15:42:00+00:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250224154229-emacs--Ollama-Buddy-Now-On-MELPA.jpg"
---

```sh
 ___ _ _      n _ n      ___       _   _ _ _
|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |
| | | | | .  |     | .  | . | | | . | . |__ |
|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|
```

<https://github.com/captainflasmr/ollama-buddy>

A friendly Emacs interface for interacting with Ollama models. This package provides a convenient way to integrate Ollama’s local LLM capabilities directly into your Emacs workflow with little or no configuration required.

Latest improvements:

-   Chat buffer now more prompt based rather than ad-hoc using C-c C-c to send and C-c C-k to cancel
-   Connection monitor now optional, ollama status visibility now maintained by strategic status checks simplifying setup.
-   Can now change models from chat buffer using C-c C-m
-   Updated intro message with ascii logo
-   Suggested default "C-c o" for `ollama-buddy-menu`
-   defcustom ollama-buddy-command-definitions now will work in the customization interface.
-   The presets directory on github contains elisp files that can be evaluated to generate a role-based menu.
-   Added to MELPA, install using the following:

<!--listend-->

```elisp
(use-package ollama-buddy
  :bind ("C-c o" . ollama-buddy-menu))
```

-   and to add initial model:

<!--listend-->

```elisp
(use-package ollama-buddy
   :bind ("C-c o" . ollama-buddy-menu)
   :custom ollama-buddy-default-model "llama3.2:1b")
```

<!--more-->

---

Roadmap :

-   DOING Multi-shot prompt to multiple LLMS to choose best answer

-   DOING Customizable Role-Based Menu Preset Generation System

-   DOING Distinguishing commands associated with LLMs using colours

For any more information just have a look at the github README!
