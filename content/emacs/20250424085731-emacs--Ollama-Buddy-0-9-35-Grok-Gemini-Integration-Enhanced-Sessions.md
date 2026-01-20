---
title: "Ollama-Buddy 0.9.35: Grok, Gemini Integration and Enhanced Sessions"
author: ["James Dyer"]
lastmod: 2025-04-24T09:20:00+01:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

Several improvements in the latest Ollama Buddy updates (versions 0.9.21 through 0.9.35):

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}


## 🎉 New AI Integrations with Grok and Gemini {#new-ai-integrations-with-grok-and-gemini}

-   Google's Gemini is now complementing existing support for Claude, ChatGPT (OpenAI), and Ollama models. Setting up is straightforward and consistent with other integrations.
-   Just like the existing integrations, Grok can now be easily configured with your API key.

<!--more-->


## 🔗 Improved Remote LLM Architecture {#improved-remote-llm-architecture}

LLM internal decoupling, making Ollama Buddy's core logic independent from any specific remote LLM. Each LLM integration now functions as a self-contained extension, significantly simplifying future additions and maintenance.


## 🎯 Standardized Model Prefixing {#standardized-model-prefixing}

Now there are more remote LLMs into the mix I thought it was probably time to more clearly distinguish between model collections, so I have defined the following prefixes:

-   Ollama:  `o:`
-   ChatGPT: `a:`
-   Claude:  `c:`
-   Gemini:  `g:`
-   Grok:    `k:`

This change helps ensure clarity, especially when recalling previous sessions. Note: existing session files will need the Ollama prefix (`o:`) added manually if you encounter issues recalling older sessions.


## 💾 Enhanced Session Management {#enhanced-session-management}

Saving session now makes a little more sense and is more consistent:

-   Automatic timestamped session names (you can still set your own).
-   Sessions now also save as `org` files alongside the original `elisp` files, allowing for richer recall and easy inspection later.
-   The current session name appears dynamically in your modeline, offering quick context.


## 🛠️ Additional Improvements {#️-additional-improvements}

-   UTF-8 encoding fixes for remote LLM stream responses.
-   Refactored history and model management so all the latest models are available for selection.  This is currently most relevant for remote LLMs which often change their model selection.
-   History view/edit functionality merged into one keybinding

---

and here is the change history


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-21 Mon&gt; </span></span> **0.9.35** {#0-dot-9-dot-35}

Added Grok support

Integration is very similar to other remote AIs:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-grok-api-key
   (auth-source-pick-first-password :host "ollama-buddy-grok" :user "apikey"))
  :config
  (require 'ollama-buddy-grok nil t))
```


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-20 Sun&gt; </span></span> **0.9.33** {#0-dot-9-dot-33}

Fixed utf-8 encoding stream response issues from remote LLMs.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-19 Sat&gt; </span></span> **0.9.32** {#0-dot-9-dot-32}

Finished the remote LLM decoupling process, meaning that the core `ollama-buddy` logic is now not dependent on any remote LLM, and each remote LLM package is self-contained and functions as a unique extension.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-18 Fri&gt; </span></span> **0.9.31** {#0-dot-9-dot-31}

Refactored model prefixing logic and cleaned up

-   Standardized model prefixing by introducing distinct prefixes for Ollama (`o:`), OpenAI (`a:`), Claude (`c:`), and Gemini (`g:`) models.
-   Centralized functions to get full model names with prefixes across different model types.
-   Removed redundant and unused variables related to model management.

Note that there may be some breaking changes here especially regarding session recall as all models will now have a prefix to uniquely identify their type.  For `ollama` recall, just edit the session files to prepend the ollama prefix of "o:"


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-17 Thu&gt; </span></span> **0.9.30** {#0-dot-9-dot-30}

Added Gemini integration!

As with the Claude and ChatGPT integration, you will need to add something similar to them in your configuration. I currently have the following set up to enable access to the remote LLMs:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-openai-api-key
   (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (ollama-buddy-claude-api-key
   (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (ollama-buddy-gemini-api-key
   (auth-source-pick-first-password :host "ollama-buddy-gemini" :user "apikey"))
  :config
  (require 'ollama-buddy-openai nil t)
  (require 'ollama-buddy-claude nil t)
  (require 'ollama-buddy-gemini nil t))
```

Also with the previous update all the latest model names will be pulled, so there should be a full comprehensive list for each of the main remote AI LLMs!


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-16 Wed&gt; </span></span> **0.9.23** {#0-dot-9-dot-23}

Refactored history and model management for remote LLMs

-   Now pulling in latest model list for remote LLMs (so now ChatGPT 4.1 is available!)
-   Removed redundant history and model management functions from `ollama-buddy-claude.el` and `ollama-buddy-openai.el`. Replaced them with shared implementations to streamline code and reduce duplication


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-15 Tue&gt; </span></span> **0.9.22** {#0-dot-9-dot-22}

Enhanced session management

-   Refactored `ollama-buddy-sessions-save` to autogenerate session names using timestamp and model.
-   Improved session saving/loading by integrating org file handling.
-   Updated mode line to display current session name dynamically.

Several improvements to session management, making it more intuitive and efficient for users. Here's a breakdown of the new functionality:

When saving a session, Ollama Buddy now creates a default name using the current timestamp and model name, users can still provide a custom name if desired.

An org file is now saved alongside the original elisp session file. This allows for better session recall as all interactions will be pulled back with the underlying session parameters still restored as before. There is an additional benefit in not only recalling precisely the session and any additional org interactions but also quickly saving to an org file for potential later inspection. Along with the improved autogenerated session name, this means it is much faster and more intuitive to save a snapshot of the current chat interaction.

The modeline now displays the current session name!


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-04-11 Fri&gt; </span></span> **0.9.21** {#0-dot-9-dot-21}

Add history edit/view toggle features, so effectively merging the former history display into the history edit functionality.
