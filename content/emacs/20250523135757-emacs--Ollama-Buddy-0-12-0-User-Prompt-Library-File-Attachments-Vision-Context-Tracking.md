---
title: "Ollama Buddy 0.12.0: User Prompt Library, File Attachments, Vision and Context Tracking"
author: ["James Dyer"]
lastmod: 2025-05-23T14:10:00+01:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

There have been quite a few updates recently. The main highlights include support for attachments, so you can push a file to the chat directly from `dired` for potential inclusion in your next query.

Vision support has been added for models that can handle it. If you supply the path to an image file in the chat, it will be processed. This means you can now, for example, extract text from images using models like `o:gemma3:4b`.

I've also introduced the ability to save user system prompts. If you have a favorite prompt, or have crafted one that works especially well for you, you can now save it by category and title in a simple Org format for later recall. Prompt recall now works the same way as Fabric patterns and Awesome ChatGPT prompts. This makes it much easier to display the currently used system prompt concisely in the status bar, as it will be based on the prompt title (and thus likely the role).

What else? Oh yes, I received a request for better context tracking. Now, when context is nearing full capacity, or has exceeded it, it will be indicated in the status bar!

That’s probably it for the major changes. There was also some refactoring, but you probably don't care about that. Anyway, here is the full list of changes:

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-22 Thu&gt; </span></span> **0.12.0** {#0-dot-12-dot-0}

Full system prompt in the status bar replaced with a more meaningful simple role title

-   Added system prompt metadata tracking with title, source, and timestamp registry
-   Implemented automatic title extraction and unified completing-read interface
-   Enhanced fabric/awesome prompt integration with proper metadata handling
-   Improved transient menu organization and org-mode formatting with folding
-   Added system prompt history display and better error handling for empty files
-   Transient menu has been simplified and reorganised

Previously, the header status bar would show truncated system prompt text like `[You are a helpful assistant wh...]`, making it difficult to quickly identify which prompt was active. Now, the display shows meaningful role titles with source indicators:

-   `[F:Code Reviewer]` - Fabric pattern
-   `[A:Linux Terminal]` - Awesome ChatGPT prompt
-   `[U:Writing Assistant]` - User-defined prompt

The system now intelligently extracts titles from prompt content by recognizing common patterns like "You are a...", "Act as...", or "I want you to act as...". When these patterns aren't found, it generates a concise title from the first few words.

Behind the scenes, Ollama Buddy now maintains a registry of all system prompts with their titles, sources, and timestamps. This enables new features like system prompt history viewing and better organization across Fabric patterns, Awesome ChatGPT prompts, and user-defined prompts.

The result is a cleaner interface that makes it immediately clear which role your AI assistant is currently embodying, without cluttering the status bar with long, truncated text.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-21 Wed&gt; </span></span> **0.11.1** {#0-dot-11-dot-1}

Quite a bit of refactoring to generally make this project more maintainable and I have added a starter kit of user prompts.

-   Color System Reworking
    -   Removed all model color-related functions and variables
    -   Removed dependency on `color.el`
    -   Replaced with `highlight-regexp` and hashing to `^font-lock` faces, so now using a more native built-in solutions for model colouring rather than shoe-horning in overlays.

-   UI Improvements
    -   Simplified the display system by leveraging Org mode
    -   Added org-mode styling for output buffers
    -   Added `org-hide-emphasis-markers` and `org-hide-leading-stars` settings
    -   Changed formatting to use Org markup instead of text properties
    -   Converted plain text headers to proper Org headings
    -   Replaced color properties with Org emphasis (bold)

-   History Management Updates
    -   Streamlined history editing functionality
    -   Improved model-specific history editing
    -   Refactored history display and navigation

-   System Prompts
    -   Added library of system prompts in these categories:
        -   analysis (3 prompts)
        -   coding (5 prompts)
        -   creative (3 prompts)
        -   documentation (3 prompts)
        -   emacs (10 prompts)
        -   general (3 prompts)
        -   technical (3 prompts)
        -   writing (3 prompts)


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-19 Mon&gt; </span></span> **0.11.0** {#0-dot-11-dot-0}

Added user system prompts management

-   You can now save, load and manage system prompts
-   Created new transient menu for user system prompts (C-c s)
-   Organized prompts by categories with org-mode format storage
-   Supported prompt editing, listing, creation and deletion
-   Updated key bindings to integrate with existing functionality
-   Added prompts directory customization with defaults

This feature makes it easier to save, organize, and reuse your favorite system prompts when working with Ollama language models.

System prompts are special instructions that guide the behavior of language models. By setting effective system prompts, you can:

-   Define the AI's role (e.g., "You are a helpful programming assistant who explains code clearly")
-   Establish response formats
-   Set the tone and style of responses
-   Provide background knowledge for specific domains

The new `ollama-buddy-user-prompts` module organizes your system prompts in a clean, category-based system:

-   **Save your prompts** - Store effective system prompts you've crafted for future use
-   **Categorize** - Prompts are organized by domains like "coding," "writing," "technical," etc.
-   **Quick access** - Browse and load your prompt library with completion-based selection
-   **Edit in org-mode** - All prompts are stored as org files with proper metadata
-   **Manage with ease** - Create, edit, list, and delete prompts through a dedicated transient menu

The new functionality is accessible through the updated key binding `C-c s`, which opens a dedicated transient menu with these options:

-   **Save current (S)** - Save your active system prompt
-   **Load prompt (L)** - Choose a previously saved prompt
-   **Create new (N)** - Start fresh with a new prompt
-   **List all Prompts (l)** - View your entire prompt library
-   **Edit prompt (e)** - Modify an existing prompt
-   **Delete prompt (d)** - Remove prompts you no longer need

If you work frequently with Ollama models, you've likely discovered the power of well-crafted system prompts. They can dramatically improve the quality and consistency of responses. With this new management system, you can:

-   Build a personal library of effective prompts
-   Maintain context continuity across sessions
-   Share prompts with teammates
-   Refine your prompts over time


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-14 Wed&gt; </span></span> **0.10.0** {#0-dot-10-dot-0}

Added file attachment system for including documents in conversations

-   Added file attachment support with configurable file size limits (10MB default) and supported file types
-   Implemented session persistence for attachments in save/load functionality
-   Added attachment context inclusion in prompts with proper token counting
-   Created comprehensive attachment management commands:
    -   Attach files to conversations
    -   Show current attachments in dedicated buffer
    -   Detach specific files
    -   Clear all attachments
-   Added Dired integration for bulk file attachment
-   Included attachment menu in transient interface (C-c 1)
-   Updated help text to document new attachment keybindings
-   Enhanced context calculation to include attachment token usage

You can now seamlessly include text files, code, documentation, and more directly in your conversations with local AI models!

Simply use `C-c C-a` from the chat buffer to attach any file to your current conversation.

The attached files become part of your conversation context, allowing the AI to reference, analyze, or work with their contents directly.

The transient menu has also been updated with a new **Attachment Menu**

```nil
*File Attachments*
  a Attach file
  w Show attachments
  d Detach file
  0 Clear all attachments
```

Your attachments aren't just dumped into the conversation - they're intelligently integrated:

-   **Token counting** now includes attachment content, so you always know how much context you're using
-   **Session persistence** means your attachments are saved and restored when you save/load conversations
-   **File size limits** (configurable, 10MB default) prevent accidentally overwhelming your context window

Managing attached files is intuitive with dedicated commands:

-   `C-c C-w` - View all current attachments in a nicely formatted org mode buffer, folded to each file
-   `C-c C-d` - Detach specific files when you no longer need them
-   `C-c 0` - Clear all attachments at once
-   `C-c 1` - Access the full attachment menu via a transient interface

Working in Dired? No problem! You can attach files directly from your file browser:

-   Mark multiple files and attach them all at once
-   Attach the file at point with a single command

Use the configuration as follows:

```elisp
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c C-a") #'ollama-buddy-dired-attach-marked-files)))
```


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-12 Mon&gt; </span></span> **0.9.50** {#0-dot-9-dot-50}

Added context size management and monitoring

-   Added configurable context sizes for popular models (llama3.2, mistral, qwen, etc.)
-   Implemented real-time context usage display in status bar
-   Can display in text or bar display types
-   Added context size thresholds with visual warnings
-   Added interactive commands for context management:
    -   `ollama-buddy-show-context-info`: View all model context sizes
    -   `ollama-buddy-set-model-context-size`: Manually configure model context
    -   `ollama-buddy-toggle-context-percentage`: Toggle context display
-   Implemented context size validation before sending prompts
-   Added token estimation and breakdown (history/system/current prompt)
-   Added keybindings: C-c $ (set context), C-c % (toggle display), C-c C (show info)
-   Updated status bar to show current/max context with fontification

I've added context window management and monitoring capabilities to Ollama Buddy!

This update helps you better understand and manage your model's context usage, preventing errors and optimizing your conversations.

Enable it with the following:

```elisp
(setq ollama-buddy-show-context-percentage t)
```


### Usage {#usage}

After implementing these changes:

1.  **Text mode**: Shows `1024/4096` style display
2.  **Bar mode** (default): Shows `███████░░░░ 2048` style display
3.  Use `C-c 8` to toggle between modes
4.  The **Text mode** will change fontification based on your thresholds:
    -   Normal: regular fontification
    -   (85%+): underlined and bold
    -   (100%+): inverse video and bold
5.  The **Bar mode** will just fill up as normal

The progress bar will visually represent how much of the context window you're using, making it easier to see at a glance when you're approaching the limit.


### Implementation Details {#implementation-details}


#### Context Size Detection {#context-size-detection}

Determining a model's context size proved more complex than expected. While experimenting with parsing model info JSON, I discovered that context size information can be scattered across different fields. Rather than implementing a complex JSON parser (which may come later), I chose a pragmatic approach:

I created a new `defcustom` variable `ollama-buddy-fallback-context-sizes` that includes hard-coded values for popular Ollama models. The fallback mechanism is deliberately simple: substring matching followed by a sensible default of 4096 tokens.

```elisp
(defcustom ollama-buddy-fallback-context-sizes
  '(("llama3.2:1b" . 2048)
    ("llama3:8b" . 4096)
    ("tinyllama" . 2048)
    ("phi3:3.8b" . 4096)
    ("gemma3:1b" . 4096)
    ("gemma3:4b" . 8192)
    ("llama3.2:3b" . 8192)
    ("llama3.2:8b" . 8192)
    ("llama3.2:70b" . 8192)
    ("starcoder2:3b" . 8192)
    ("starcoder2:7b" . 8192)
    ("starcoder2:15b" . 8192)
    ("mistral:7b" . 8192)
    ("mistral:8x7b" . 32768)
    ("codellama:7b" . 8192)
    ("codellama:13b" . 8192)
    ("codellama:34b" . 8192)
    ("qwen2.5-coder:7b" . 8192)
    ("qwen2.5-coder:3b" . 8192)
    ("qwen3:0.6b" . 4096)
    ("qwen3:1.7b" . 8192)
    ("qwen3:4b" . 8192)
    ("qwen3:8b" . 8192)
    ("deepseek-r1:7b" . 8192)
    ("deepseek-r1:1.5b" . 4096))
  "Mapping of model names to their default context sizes.
Used as a fallback when context size can't be determined from the API."
  :type '(alist :key-type string :value-type integer)
  :group 'ollama-buddy)
```

This approach may not be perfectly accurate for all models, but it's sufficient for getting the core functionality working. More importantly, as a `defcustom`, users can easily customize these values for complete accuracy with their specific models. Users can also set context values within the chat buffer through `C-c C` (Show Context Information) for each individual model if desired.

This design choice allowed me to focus on the essential features without getting stuck on complex context retrieval logic.

One final thing!, if the `num_ctx: Context window size in tokens` is set, then that number will also be taken into consideration.  An assumption will be made that the model is honouring the context size requested and will incorporated into the context calculations accordingly.


#### Token Estimation {#token-estimation}

For token counting, I've implemented a simple heuristic: each word (using string-split) is multiplied by 1.3. This follows commonly recommended approximations and works well enough in practice. While this isn't currently configurable, I may add it as a customization option in the future.


### How to Use Context Management in Practice {#how-to-use-context-management-in-practice}

The `C-c C` (Show Context Information) command is central to this feature. Rather than continuously monitoring context size while you type (which would be computationally expensive and potentially distracting), I've designed the system to calculate context on-demand when you choose.


#### Typical Workflows {#typical-workflows}

**Scenario 1: Paste-and-Send Approach**

Let's say you want to paste a large block of text into the chat buffer. You can simply:

1.  Paste your content
2.  Press the send keybinding
3.  If the context limit is exceeded, you'll get a warning dialog asking whether to proceed anyway

**Scenario 2: Preemptive Checking**

For more control, you can check context usage before sending:

1.  Paste your content
2.  Run `C-c C` to see the current context breakdown
3.  If the context looks too high, you have several options:
    -   Trim your current prompt
    -   Remove or simplify your system prompt
    -   Edit conversation history using Ollama Buddy's history modification features
    -   Switch to a model with a larger context window

**Scenario 3: Manage the Max History Length**

Want tight control over context size without constantly monitoring the real-time display? Since conversation history is part of the context, you can simply limit `ollama-buddy-max-history-length` to control the total context size.

For example, when working with small context windows, set `ollama-buddy-max-history-length` to 1. This keeps only the last exchange (your prompt + model response), ensuring your context remains small and predictable, perfect for maintaining control without manual monitoring.

**Scenario 4: Parameter num_ctx: Context window size in tokens**

Simply set this parameter and off you go!


### Current Status: Experimental {#current-status-experimental}

Given the potentially limiting nature of context management, I've set this feature to **disabled by default**.

But to enable set the following :

```elisp
(setq ollama-buddy-show-context-percentage t)
```

This means:

-   Context checks won't prevent sending prompts
-   Context usage won't appear in the status line
-   However, calculations still run in the background, so `C-c C` (Show Context Information) remains functional

As the feature matures and proves its value, I may enable it by default. For now, consider it an experimental addition that users can opt into.


### More Details {#more-details}

The status bar now displays your current context usage in real-time. You'll see a fraction showing used tokens versus the model's maximum context size (e.g., "2048/8192"). The display automatically updates as your conversation grows.

Context usage changes fontification to help you stay within limits:

-   **Normal font**: Normal usage (under 85%)
-   **Bold and Underlined**: Approaching limit (85-100%)
-   **Inversed**: At or exceeding limit (100%+)

Before sending prompts that exceed the context limit, Ollama Buddy now warns you and asks for confirmation. This prevents unexpected errors and helps you manage long conversations more effectively.

There are now three new interactive commands:

`C-c $` - Set Model Context Size. Manually configure context sizes for custom or fine-tuned models.

`C-c %` - Toggle Context Display. Show or hide the context percentage in the status bar.

`C-c C` - Show Context Information. View a detailed breakdown of:

-   All model context sizes
-   Current token usage by category (history, system prompt, current prompt)
-   Percentage usage

---

The system estimates token counts for:

-   **Conversation history**: All previous messages
-   **System prompts**: Your custom instructions
-   **Current input**: The message you're about to send

This gives you a complete picture of your context usage before hitting send.

The context monitoring is not enabled by default.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-05 Mon&gt; </span></span> **0.9.44** {#0-dot-9-dot-44}

-   Sorted model names alphabetically in intro message
-   Removed multishot writing to register name letters

For some reason, when I moved the .ollama folder to an external disk, the models returned with api/tags were inconsistent, which meant it broke consistent letter assignment. I'm not sure why this happened, but it is probably sensible to sort the models alphabetically anyway, as this has the benefit of naturally grouping together model families.

I also removed the multishot feature of writing to the associated model letter. Now that I have to accommodate more than 26 models, incorporating them into the single-letter Emacs register system is all but impossible. I suspect this feature was not much used, and if you think about it, it wouldn't have worked anyway with multiple model shots, as the register letter associated with the model would just show the most recent response. Due to these factors, I think I should remove this feature. If someone wants it back, I will probably have to design a bespoke version fully incorporated into the ollama-buddy system, as I can't think of any other Emacs mechanism that could accommodate this.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-05 Mon&gt; </span></span> **0.9.43** {#0-dot-9-dot-43}

Fix model reference error exceeding 26 models #15

Update `ollama-buddy` to handle more than 26 models by using prefixed combinations for model references beyond 'z'. This prevents errors in `create-intro-message` when the local server hosts a large number of models.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-03 Sat&gt; </span></span> **0.9.42** {#0-dot-9-dot-42}

Added the following to recommended models:

-   qwen3:0.6b
-   qwen3:1.7b
-   qwen3:4b
-   qwen3:8b

and fixed pull model


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-02 Fri&gt; </span></span> **0.9.41** {#0-dot-9-dot-41}

Refactored model prefixing again so that when using only ollama models no prefix is applied and is only applied when online LLMs are selected (for example claude, chatGPT e.t.c)

I think this makes more sense and is cleaner for I suspect the majority who may use this package are probably more interested in just using ollama models and the prefix will probably be a bit confusing.

This could be a bit of a breaking change once again I'm afraid for those ollama users that have switched and are now familiar with prefixing "o:", sorry!


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-05-02 Fri&gt; </span></span> **0.9.40** {#0-dot-9-dot-40}

Added vision support for those ollama models that can support it!

Image files are now detected within a prompt and then processed if a model can support vision processing. Here's a quick overview of how it works:

1.  **Configuration**: Users can configure the application to enable vision support and specify which models and image formats are supported.  Vision support is enabled by default.

2.  **Image Detection**: When a prompt is submitted, the system automatically detects any image files referenced in the prompt.

3.  **Vision Processing**: If the model supports vision, the detected images are processed in relation to the defined prompt. Note that the detection of a model being vision capable is defined in `ollama-buddy-vision-models` and can be adjusted as required.

4.  In addition, a menu item has been added to the custom ollama buddy menu :
    ```nil
          [I] Analyze an Image
    ```

When selected, it will allow you to describe a chosen image. At some stage, I may allow integration into `dired`, which would be pretty neat. :)
