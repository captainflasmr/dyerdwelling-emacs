---
title: "Ollama-Buddy 0.8.0 - Added System Prompts, Model Info and simpler menu model assignment"
author: ["James Dyer"]
lastmod: 2025-03-14T13:21:00+00:00
tags: ["ollama-buddy", "ollama", "ai", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250314132104-emacs--Ollama-Buddy-0-8-0-Added-System-Prompts-Model-Info-and-simpler-menu-model-assignment.jpg"
---

More improvements to `ollama-buddy` <https://github.com/captainflasmr/ollama-buddy>

The main addition is that of system prompts, which allows setting the general tone and guidance of the overall chat.  Currently the system prompt can be set at any time and turned on and off but I think to enhance my model/command for each menu item concept, I could also add a :system property to the menu alist definition to allow even tighter control of a menu action to prompt response.

<!--more-->

{{< figure src="/emacs/20250314132104-emacs--Ollama-Buddy-0-8-0-Added-System-Prompts-Model-Info-and-simpler-menu-model-assignment.jpg" width="100%" >}}

Also now I have parameter functionality working for fine grained control, I could add these individual parameters for each menu command, for example the `temperature` could be very useful in this case to play around with the randomness/casualness of the response.

The next improvement will likely involve adding support for interacting more directly with Ollama to create and pull models. However, I'm still unsure whether performing this within Emacs is the best approach, I could assume that all models are already set up in Ollama.

That said, importing a GGUF file might be a useful feature, possibly from within `dired`. Currently, this process requires multiple steps: creating a simple model file that points to the GGUF file on disk, then running the `ollama create` command to import it. Streamlining this workflow could enhance usability.

Then maybe on to embeddings, of which I currently have no idea, haven't read up on it, nuffin, but that is something to look forward to! :)

Anyways, here is the latest set of updates to Ollama Buddy:


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-14 Fri&gt; </span></span> **0.8.0** {#0-dot-8-dot-0}

Added system prompt support

-   Added `ollama-buddy--current-system-prompt` variable to track system prompts
-   Updated prompt area rendering to distinguish system prompts
-   Modified request payload to include system prompt when set
-   Enhanced status bar to display system prompt indicator
-   Improved help menu with system prompt keybindings

So this is system prompt support in Ollama Buddy!, allowing you to set and manage system-level instructions for your AI interactions. This feature enables you to define a **persistent system prompt** that remains active across user queries, providing better control over conversation context.

**Key Features**

You can now designate any user prompt as a system prompt, ensuring that the AI considers it as a guiding instruction for future interactions. To set the system prompt, use:

```bash
C-u C-c C-c
```

**Example:**

1.  Type:

<!--listend-->

```bash
Always respond in a formal tone.
```

1.  Press `C-u C-c C-c` This prompt is now set as the **system prompt** and any further chat ollama responses will adhere to the overarching guidelines defined in the prompt.

If you need to clear the system prompt and revert to normal interactions, use:

```bash
C-u C-u C-c C-c
```

**How It Works**

-   The active **system prompt** is stored and sent with each user prompt.
-   A "S" indicator appears in the status bar when a system prompt is active.
-   The request payload now includes the system role, allowing AI to recognize persistent instructions.

**Demo**

Set the system message to:

You must always respond in a single sentence.

Now ask the following:

Tell me why Emacs is so great!

Tell me about black holes

clear the system message and ask again, the responses should now be more verbose!!

{{< figure src="/emacs/ollama-buddy-screen-recording_015.gif" width="100%" >}}


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-13 Thu&gt; </span></span> **0.7.4** {#0-dot-7-dot-4}

Added model info command, update keybindings

-   Added \`ollama-buddy-show-raw-model-info\` to fetch and display raw JSON details
    of the current model in the chat buffer.
-   Updated keybindings:
    -   \`C-c i\` now triggers model info display.
    -   \`C-c h\` mapped to help assistant.
    -   Improved shortcut descriptions in quick tips section.
-   Removed unused help assistant entry from menu.
-   Changed minibuffer-prompt key from \`?i\` to \`?b\`.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-12 Wed&gt; </span></span> **0.7.3** {#0-dot-7-dot-3}

Added function to associate models with menu commands

-   Added `ollama-buddy-add-model-to-menu-entry` autoload function
-   Enabled dynamic modification of command-model associations

This is a helper function that allows you to associate specific models with individual menu commands.

Configuration to apply a model to a menu entry is now straightforward, in your Emacs init file, add something like:

```elisp
(with-eval-after-load 'ollama-buddy
  (ollama-buddy-add-model-to-menu-entry 'dictionary-lookup "tinyllama:latest")
  (ollama-buddy-add-model-to-menu-entry 'synonym "tinyllama:latest"))
```

This configures simpler tasks like dictionary lookups and synonym searches to use the more efficient TinyLlama model, while your default model will still be used for more complex operations.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-12 Wed&gt; </span></span> **0.7.2** {#0-dot-7-dot-2}

Added menu model colours back in and removed some redundant code
