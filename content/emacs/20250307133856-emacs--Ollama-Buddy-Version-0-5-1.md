---
title: "Ollama-Buddy V0.5.1 - Session/History/Role Management, Real-Time Token Tracking and More!"
author: ["James Dyer"]
lastmod: 2025-03-07T13:50:00+00:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250224154229-emacs--Ollama-Buddy-Now-On-MELPA.jpg"
---

I've been a busy little bee in the last few days, so quite a few improvements to `ollama-buddy`, my Emacs LLM `ollama` client, they are listed below:

{{< figure src="/emacs/ollama-buddy-banner.jpg" width="100%" >}}

<!--more-->


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-07 Fri&gt; </span></span> **0.5.1** {#0-dot-5-dot-1}

Added temperature control

-   Implemented temperature control parameter
-   Added menu commands for setting (T), resetting (0)
-   Added keybindings (C-c t/T/0) for quick temperature adjustments
-   Updated header line and prompt displays to show current temperature
-   Included temperature info in welcome screen with usage guidance

This addition gives users fine-grained control over the creativity and randomness of their AI responses through a new temperature variable.

This update adds several convenient ways to control temperature in Ollama-Buddy:

**Key Features**

1.  **Direct Temperature Setting**: Use `C-c t` from the chat buffer or the menu command `[T]` to set an exact temperature value between 0.0 and 2.0.

2.  **Preset Temperatures**: Quickly switch between common temperature presets with `C-c T` from the chat buffer:
    -   Precise (0.1) - For factual responses
    -   Focused (0.3) - For deterministic, coherent outputs
    -   Balanced (0.7) - Default setting
    -   Creative (0.9) - For more varied, creative responses

3.  **Reset to Default**: Return to the default temperature (0.7) with `C-c 0` or the menu command `[0]`.

4.  **Visual Feedback**: The current temperature is displayed in the header line and before each response, so you always know what setting you're using.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-06 Thu&gt; </span></span> **0.5.0** {#0-dot-5-dot-0}

Implemented session management, so you can now save your conversations and bring them back with the relevant context and chat history!

-   Chat history is now maintained separately for each model
-   Added session new/load/save/delete/list functionality
-   A switch in context can now be achieved by any of the following methods:
    -   Loading a previous session
    -   Creating a new session
    -   Clearing history on the current session
    -   Toggling history on and off

**Key Benefits**

-   More relevant responses when switching between models
-   Prevents context contamination across different models
-   Clearer session management and organization

**Key Features**

1.  **Session Management**

With session management, you can now:

-   **Save session** with `ollama-buddy-sessions-save` (or through the ollama-buddy-menu) Preserve your current conversation with a custom name
-   **Load session** with `ollama-buddy-sessions-load` (or through the ollama-buddy-menu) Return to previous conversations exactly where you left off
-   **List all sessions** with `ollama-buddy-sessions-list` (or through the ollama-buddy-menu) View all saved sessions with metadata including timestamps and models used
-   **Delete session** with `ollama-buddy-sessions-delete` (or through the ollama-buddy-menu) Clean up sessions you no longer need
-   **New session** with `ollama-buddy-sessions-new`  (or through the ollama-buddy-menu) Begin a clean slate without losing your saved sessions

-   **Menu Commands**

The following commands have been added to the `ollama-buddy-menu`:

-   `E` New session
-   `L` Load session
-   `S` Save session
-   `Y` List sessions
-   `K` Delete session


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-04 Tue&gt; </span></span> **0.4.1** {#0-dot-4-dot-1}

Added a sparse version of `ollama-buddy` called `ollama-buddy-mini`, see the github repository for the elisp file and a description in `README-mini.org`


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-03 Mon&gt; </span></span> **0.4.0** {#0-dot-4-dot-0}

Added conversation history support and navigation functions

-   Implemented conversation history tracking between prompts and responses
-   Added configurable history length limits and visual indicators
-   Created navigation functions to move between prompts/responses in buffer

**Key Features**

1.  **Conversation History**

Ollama Buddy now maintains context between your interactions by:

-   Tracking conversation history between prompts and responses
-   Sending previous messages to Ollama for improved contextual responses
-   Displaying a history counter in the status line showing conversation length
-   Providing configurable history length limits to control memory usage

You can control this feature with:

```elisp
;; Enable/disable conversation history (default: t)
(setq ollama-buddy-history-enabled t)

;; Set maximum conversation pairs to remember (default: 10)
(setq ollama-buddy-max-history-length 10)

;; Show/hide the history counter in the header line (default: t)
(setq ollama-buddy-show-history-indicator t)
```

1.  **Enhanced Navigation**

Moving through longer conversations is now much easier with:

-   Navigation functions to jump between prompts using C-c n/p

-   **Menu Commands**

Three new menu commands have been added:

-   `H`: Toggle history tracking on/off
-   `X`: Clear the current conversation history
-   `V`: View the full conversation history in a dedicated buffer


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-02 Sun&gt; </span></span> **0.3.1** {#0-dot-3-dot-1}

Enhanced model colour contrast with themes, allowing `ollama-buddy-enable-model-colors` to be enabled by default.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-01 Sat&gt; </span></span> **0.3.0** {#0-dot-3-dot-0}

Added real-time token usage tracking and display

-   Introduce variables to track token counts, rates, and usage history
-   Implement real-time token rate updates with a timer
-   Add a function to display token usage statistics in a dedicated buffer
-   Allow toggling of token stats display after responses
-   Integrate token tracking into response processing and status updates
-   Ensure cleanup of timers and tracking variables on completion or cancellation

**Key Features**

1.  **Menu Commands**

    The following command has been added to the `ollama-buddy-menu`:

    -   `t` Show a summary of token model usage stats


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-02-28 Fri&gt; </span></span> **0.2.4** {#0-dot-2-dot-4}

Added model-specific color highlighting

-   Introduce \`ollama-buddy-enable-model-colors\` (default: nil) to toggle model-based color highlighting.
-   Assign consistent colors to models based on string hashing.
-   Apply colors to model names in the menu, status, headers, and responses.
-   Add \`ollama-buddy-toggle-model-colors\` command to toggle this feature.

This enhancement aims to improve user experience by visually distinguishing different AI models within the interface.

Note: I am likely to use both **colour** and **color** interchangeably in the following text! :)

**Key Features**

1.  **Model-Specific Colors**
    -   A new customizable variable, `ollama-buddy-enable-model-colors`, allows users to enable or disable model-specific colors.
    -   Colors are generated based on a model's name using a hashing function that produces consistent and visually distinguishable hues.
    -   However there could be an improvement regarding ensuring the contrast is sufficient and hence visibility maintained with differing themes.

2.  **Interactive Color Toggle**
    -   Users can toggle model-specific colors with the command `ollama-buddy-toggle-model-colors`, providing flexibility in interface customization.

3.  **Colored Model Listings**
    -   Model names are now displayed with their respective colors in various parts of the interface, including:
        -   The status line
        -   Model selection menus
        -   Command definitions
        -   Chat history headers

4.  **Menu Commands**

The following command hashing been added to the `ollama-buddy-menu`:

-   `C` Toggle colors


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-02-28 Fri&gt; </span></span> **0.2.3** {#0-dot-2-dot-3}

Added Prompt History Support

-   Prompts are now integrated into the Emacs history mechanism which means they persist across sessions.
-   Use `M-p` to navigate prompt history, and `M-p` / `M-n` within the minibuffer to insert previous prompts.

**Key Features**

-   Persistent prompt history
-   A new variable, `ollama-buddy--prompt-history`, now keeps track of past prompts. This means you can quickly recall and reuse previous queries instead of retyping them from scratch.
-   `M-p` - recall a previous prompt in the buffer which will bring up the minibuffer for prompt history selection.
-   Minibuffer `M-p` / `M-n` - Navigate through past prompts when prompted for input.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-02-27 Thu&gt; </span></span> **0.2.2** {#0-dot-2-dot-2}

Added support for role-based presets

-   Introduced \`ollama-buddy-roles-directory\` for storing role preset files.
-   Implemented interactive functions to manage roles:
    -   \`ollama-buddy-roles-switch-role\`
    -   \`ollama-buddy-role-creator-create-new-role\`
    -   \`ollama-buddy-roles-open-directory\`
-   Added ability to create and switch between role-specific commands.
-   Updated menu commands to include role management options.

This enhancement allows you to create, switch, and manage role-specific command configurations, which basically generates differing menu layouts and hence command options based on your context, making your workflow more personalized and efficient.

**What Are Role-Based Presets?**

Roles in Ollama Buddy are essentially **profiles** tailored to specific tasks. Imagine you're using Ollama Buddy for:

-   **Coding assistance** with one set of prompts
-   **Creative writing** with a different tone and response style
-   **Generating Buffy Style Quips** - just a fun one!

With this update, you can now create presets for each of these contexts and switch between them seamlessly without manually re-configuring settings every time. On each switch of context and hence role, a new ollama buddy menu will be generated with the associated keybinding attached to the relevant context commands.

**Key Features**

**1. Store Your Custom Roles**

A new directory `ollama-buddy-roles-directory` (defaulting to `~/.emacs.d/ollama-buddy-presets/`) now holds your role presets. Each role is saved as an `.el` file containing predefined **commands**, **shortcuts**, and **model preferences**.

**2. Easily Switch Between Roles**

With `M-x ollama-buddy-roles-switch-role` you can pick from available role presets and swap effortlessly between them (or use the menu item from `ollama-buddy-menu`)

**3. Create Custom Roles with Unique Commands**

You can now define **custom commands** for each role with `M-x ollama-buddy-role-creator-create-new-role` (or the menu item from `ollama-buddy-menu`)

This interactive function allows you to:

-   Assign menu shortcuts to commands
-   Describe command behaviour
-   Set a default AI model
-   Define a system prompt for guiding responses

Once saved, your new role is ready to load anytime!

**4. Open Role Directory in Dired**

Need to tweak a role manually? A simple, run `M-x ollama-buddy-roles-open-directory` or of course also from the `ollama-buddy-menu` which opens the presets folder in **dired**, where you can quickly edit, copy, or delete role configurations.

**5. Preconfigured presets are available if you'd like to use a ready-made setup.**

-   ollama-buddy--preset\__buffy.el
-   ollama-buddy--preset\__default.el
-   ollama-buddy--preset\__developer.el
-   ollama-buddy--preset\__janeway.el
-   ollama-buddy--preset\__translator.el
-   ollama-buddy--preset\__writer.el

If these files are put in the `ollama-buddy-roles-directory` then the role selection menu will pass through completing-read, and present the following:

{buffy | default | developer | janeway | translator | writer}

With the selection regenerating the `ollama-buddy-menu` accordingly, and off you go.

**6. Menu commands**

The following commands have been added to the `ollama-buddy-menu`:

-   `R` Switch Role
-   `N` Create New Role
-   `D` Open Roles Directory
