---
title: "Ollama Buddy v1.0: A Simple AI Assistant"
author: ["James Dyer"]
lastmod: 2025-07-23T09:20:00+01:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

After months of development and refinement, I'm excited to announce **Ollama Buddy v1.0** - an Emacs package that simply interfaces mainly to ollama, for local LLM usage, but can be integrated to the major online players. This project initially started as a simple integration with Ollama and since then has somewhat evolved into a more fully fully-featured AI Emacs assistant.  The main focus with this package is a front facing simplicity but hiding (hopefully) all the features you would expect from an AI chatbot - wait I hate that term, I mean, assistant :).  There is also the ability to craft a customizable menu system for different roles.

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}

I had a blast developing this package and next up is RAG!. I saw recently that a package called `vecdb` was introduced into the package ecosystem to help with the storage of vector embeddings, so as ollama can return embedding vectors for semantic search I thought I would combine my package, vecdb, also probably initially a PostgreSQL database with a pgvector extension and ollama into something that could ingest files directly from Emacs.  I think I have figured this out now, I just need to do it (when the baby is asleep, probably!)


## Why Choose Ollama Buddy? {#why-choose-ollama-buddy}

I designed Ollama Buddy to be as simple as possible to set up, no backend configuration or complex setup required. This was achievable initially because I focused solely on Ollama integration, where models are automatically discoverable.

Since then, I've expanded support to major online AI providers while maintaining that same simplicity through a modular architecture. The system now handles multiple providers without adding complexity to the user experience.

Another key feature is the customizable menu system, which integrates with role-based switching. You can create specialized AI menus for different contexts, like a coding-focused setup or a writing-optimized configuration and switch between them instantly. Everything is fully configurable to match your workflow.


## Links {#links}

Here are some links:

<https://github.com/captainflasmr/ollama-buddy>
<https://melpa.org/#/ollama-buddy>

I will outline the major features below, but I do have a manual available!

<https://github.com/captainflasmr/ollama-buddy/blob/main/docs/ollama-buddy.org>


## Key Features {#key-features}


### Multiple AI Providers {#multiple-ai-providers}

-   **Local Models**: Full support for Ollama with automatic model management
-   **Cloud Services**: Integrated support for OpenAI (ChatGPT), Anthropic Claude, Google Gemini, and Grok
-   **Seamless Switching**: Change between local and cloud models with a single command
-   **Unified Interface**: Same commands work across all providers


### Role-Based Workflows - build your own AI menu {#role-based-workflows-build-your-own-ai-menu}

-   **Preset Roles**: Switch between different AI personalities (developer, writer, analyst, etc.)
-   **Custom Roles**: Create specialized workflows with specific models and parameters
-   **Menu Customization**: Each role can have its own set of commands and shortcuts


### Chat Interface {#chat-interface}

-   **Org-mode Integration**: Conversations rendered in structured org-mode format
-   **Real-time Streaming**: Watch responses appear token by token
-   **Context Management**: Visual context window monitoring with usage warnings
-   **History Tracking**: Full conversation history with model-specific storage


### File Handling {#file-handling}

-   **File Attachments**: Attach documents directly to conversations for context-aware analysis
-   **Vision Support**: Upload and analyse images with vision-capable models
-   **Dired Integration**: Bulk attach files directly from Emacs file manager


### Prompt Management {#prompt-management}

-   **System Prompts**: Create and manage reusable system prompts for different use cases
-   **Fabric Integration**: Auto-sync with Fabric patterns (200+ professional prompts)
-   **Awesome ChatGPT Prompts**: Built-in access to the popular prompt collection
-   **User Prompts**: Create and organize your own custom prompt library (which of course is org based)


### Session Management {#session-management}

-   **Save &amp; Restore**: Full session persistence including history, attachments, and settings
-   **Session Browser**: Visual interface to manage multiple conversation sessions
-   **Auto-naming**: Intelligent session naming based on conversation content


### Flexible Interface Options {#flexible-interface-options}

-   **Two Interface Levels**: Basic mode for beginners, advanced for power users
-   **Transient Menus**: Magit-style discoverable command interface
-   **Custom Menus**: Traditional text-based menu system
-   **Keyboard Shortcuts**: Comprehensive keybinding system for efficiency, I'm not sure there are any keys left!!


## What's Next? {#what-s-next}

Version 1.0 represents a stable, foundation, Ollama Buddy has been out there now for a few months with only a single github issue but development continues with:

-   RAG integration using perhaps the new `vecdb` package, as mentioned above
-   Additional AI provider integrations (Perplexity maybe?, any suggestions?)
-   Auto-completion (not sure how doable this is with ollama, but I do have a prototype)
