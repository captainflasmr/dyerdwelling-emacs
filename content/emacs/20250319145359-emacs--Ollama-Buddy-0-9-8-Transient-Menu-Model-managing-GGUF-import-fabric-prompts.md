---
title: "Ollama-Buddy 0.9.8: Transient Menu, Model Managing, GGUF Import, fabric Prompts and History Editing"
author: ["James Dyer"]
lastmod: 2025-03-19T16:08:00+00:00
tags: ["ollama-buddy", "ollama", "ai", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250319145359-emacs--Ollama-Buddy-0-9-8-Transient-Menu-Model-managing-GGUF-import-fabric-prompts.jpg"
---

This week in [ollama-buddy](https://github.com/captainflasmr/ollama-buddy) updates, I have been continuing on the busy bee side of things.

The headlines are :

-   Transient menu - yes, I know I said I would never do it, but, well I did and as it turns out I kinda quite like it and works especially well when setting parameters.
-   Support for `fabric` prompts presets - mainly as I thought generally user curated prompts was a pretty cool idea, and now I have system prompts implemented it seemed like a perfect fit.  All I needed to do was to pull the patterns directory and then parse accordingly, of course Emacs is good at this.
-   GGUF import - I don't always pull from ollama's command line, sometimes I download a GGUF file, it is a bit of a process to import to ollama, create a model file, run a command, e.t.c, but now you can import from within `dired`!
-   More support for the `ollama` API - includes model management, so pulling, stopping, deleting and more!
-   Conversation history editing - as I store the history in a hash table, I can easily just display an alist, and editing can leverage the `sexp` usual keybindings and then load back in to the variable.
-   Parameter profiles - When implementing the transient menu I thought it might be fun to try parameter profiles where a set of parameters can be applied in a block for each preset.

<!--more-->

{{< figure src="/emacs/20250319145359-emacs--Ollama-Buddy-0-9-8-Transient-Menu-Model-managing-GGUF-import-fabric-prompts.jpg" width="100%" >}}

And now for the detail, version by version...


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-19 Wed&gt; </span></span> **0.9.8** {#0-dot-9-dot-8}

Added model management interface to pull and delete models

-   Introduced \`ollama-buddy-manage-models\` to list and manage models.
-   Added actions for selecting, pulling, stopping, and deleting models.

You can now manage your Ollama models directly within Emacs with `ollama-buddy`

With this update, you can now:

-   **Browse Available Models** – See all installed models at a glance.
-   **Select Models Easily** – Set your active AI model with a single click.
-   **Pull Models from Ollama Hub** – Download new models or update existing ones.
-   **Stop Running Models** – Halt background processes when necessary.
-   **Delete Unused Models** – Clean up your workspace with ease.

-   **Open the Model Management Interface**
    Press **`C-c W`** to launch the new **Model Management** buffer or through the transient menu.

-   **Manage Your Models**
    -   Click on a model to **select** it.
    -   Use **"Pull"** to fetch models from the Ollama Hub.
    -   Click **"Stop"** to halt active models.
    -   Use **"Delete"** to remove unwanted models.

-   **Perform Quick Actions**
    -   **`g`** → Refresh the model list.
    -   **`i`** → Import a **GGUF model file**.
    -   **`p`** → Pull a new model from the **Ollama Hub**.

When you open the management interface, you get a structured list like this:

```nil
Ollama Models Management
=======================

Current Model: mistral:7b
Default Model: mistral:7b

Available Models:
  [ ] llama3.2:1b  Info  Pull  Delete
  [ ] starcoder2:3b  Info  Pull  Delete
  [ ] codellama:7b  Info  Pull  Delete
  [ ] phi3:3.8b  Info  Pull  Delete
  [x] llama3.2:3b  Info  Pull  Delete Stop

Actions:
[Import GGUF File]  [Refresh List]  [Pull Model from Hub]
```

Previously, managing Ollama models required manually running shell commands. With this update, you can now **do it all from Emacs**, keeping your workflow smooth and efficient!


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-19 Wed&gt; </span></span> **0.9.7** {#0-dot-9-dot-7}

-   Added GGUF file import and Dired integration

Import GGUF Models into Ollama from `dired` with the new `ollama-buddy-import-gguf-file` function. In `dired` just navigate to your file and press `C-c i` or `M-x ollama-buddy-import-gguf-file` to start the import process. This eliminates the need to manually input file paths, making the workflow smoother and faster.

The model will then be immediately available in the `ollama-buddy` chat interface.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-18 Tue&gt; </span></span> **0.9.6** {#0-dot-9-dot-6}

-   Added a transient menu containing all commands currently presented in the chat buffer
-   Added fabric prompting support, see <https://github.com/danielmiessler/fabric>
-   Moved the presets to the top level so they will be present in the package folder

Ollama Buddy now includes a transient-based menu system to improve usability and streamline interactions. Yes, I originally stated that I would never do it, but I think it compliments my crafted simple textual menu and the fact that I have now defaulted the main chat interface to a simple menu.

This can give the user more options for configuration, they can use the chat in advanced mode where the keybindings are presented in situ, or a more minimal basic setup where the transient menu can be activated.  For my use-package definition I current have the following set up, with the two styles of menus sitting alongside each other :

```elisp
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu)
```

The new menu provides an organized interface for accessing the assistant’s core functions, including chat, model management, roles, and Fabric patterns. This post provides an overview of the features available in the Ollama Buddy transient menus.

Yes that's right also `fabric` patterns!, I have decided to add in auto syncing of the patterns directory in <https://github.com/danielmiessler/fabric>

Simply I pull the patterns directory which contain prompt guidance for a range of different topics and then push them through a completing read to set the `ollama-buddy` system prompt, so a special set of curated prompts can now be applied right in the `ollama-buddy` chat!

Anyways, here is a description of the transient menu system.


### What is the Transient Menu? {#what-is-the-transient-menu}

The transient menu in Ollama Buddy leverages Emacs' `transient.el` package (the same technology behind Magit's popular interface) to create a hierarchical, discoverable menu system. This approach transforms the user experience from memorizing numerous keybindings to navigating through logical groups of commands with clear descriptions.


### Accessing the Menu {#accessing-the-menu}

The main transient menu can be accessed with the keybinding `C-c O` when in an Ollama Buddy chat buffer. You can also call it via `M-x ollama-buddy-transient-menu` from anywhere in Emacs.


### What the Menu Looks Like {#what-the-menu-looks-like}

When called, the main transient menu appears at the bottom of your Emacs frame, organized into logical sections with descriptive prefixes. Here's what you'll see:

```nil
|o(Y)o| Ollama Buddy
[Chat]             [Prompts]            [Model]               [Roles & Patterns]
o  Open Chat       l  Send Region       m  Switch Model       R  Switch Roles
O  Commands        s  Set System Prompt v  View Model Status  E  Create New Role
RET Send Prompt    C-s Show System      i  Show Model Info    D  Open Roles Directory
h  Help/Menu       r  Reset System      M  Multishot          f  Fabric Patterns
k  Kill/Cancel     b  Ollama Buddy Menu

[Display Options]          [History]              [Sessions]             [Parameters]
A  Toggle Interface Level  H  Toggle History      N  New Session         P  Edit Parameter
B  Toggle Debug Mode       X  Clear History       L  Load Session        G  Display Parameters
T  Toggle Token Display    V  Display History     S  Save Session        I  Parameter Help
U  Display Token Stats     J  Edit History        Q  List Sessions       K  Reset Parameters
C-o Toggle Markdown->Org                          Z  Delete Session      F  Toggle Params in Header
c  Toggle Model Colors                                                   p  Parameter Profiles
g  Token Usage Graph
```

This visual layout makes it easy to discover and access the full range of Ollama Buddy's functionality. Let's explore each section in detail.


### Menu Sections Explained {#menu-sections-explained}


#### Chat Section {#chat-section}

This section contains the core interaction commands:

-   **Open Chat (o)**: Opens the Ollama Buddy chat buffer
-   **Commands (O)**: Opens a submenu with specialized commands
-   **Send Prompt (RET)**: Sends the current prompt to the model
-   **Help/Menu (h)**: Displays the help assistant with usage tips
-   **Kill/Cancel Request (k)**: Cancels the current ongoing request


#### Prompts Section {#prompts-section}

These commands help you manage and send prompts:

-   **Send Region (l)**: Sends the selected region as a prompt
-   **Set System Prompt (s)**: Sets the current prompt as a system prompt
-   **Show System Prompt (C-s)**: Displays the current system prompt
-   **Reset System Prompt (r)**: Resets the system prompt to default
-   **Ollama Buddy Menu (b)**: Opens the classic menu interface


#### Model Section {#model-section}

Commands for model management:

-   **Switch Model (m)**: Changes the active LLM
-   **View Model Status (v)**: Shows status of all available models
-   **Show Model Info (i)**: Displays detailed information about the current model
-   **Multishot (M)**: Sends the same prompt to multiple models


#### Roles &amp; Patterns Section {#roles-and-patterns-section}

These commands help manage roles and use fabric patterns:

-   **Switch Roles (R)**: Switch to a different predefined role
-   **Create New Role (E)**: Create a new role interactively
-   **Open Roles Directory (D)**: Open the directory containing role definitions
-   **Fabric Patterns (f)**: Opens the submenu for Fabric patterns

When you select the Fabric Patterns option, you'll see a submenu like this:

```nil
Fabric Patterns (42 available, last synced: 2025-03-18 14:30)
[Actions]             [Sync]              [Categories]          [Navigation]
s  Send with Pattern  S  Sync Latest      u  Universal Patterns q  Back to Main Menu
p  Set as System      P  Populate Cache   c  Code Patterns
l  List All Patterns  I  Initial Setup    w  Writing Patterns
v  View Pattern Details                   a  Analysis Patterns
```


#### Display Options Section {#display-options-section}

Commands to customize the display:

-   **Toggle Interface Level (A)**: Switch between basic and advanced interfaces
-   **Toggle Debug Mode (B)**: Enable/disable JSON debug information
-   **Toggle Token Display (T)**: Show/hide token usage statistics
-   **Display Token Stats (U)**: Show detailed token usage information
-   **Toggle Markdown-&gt;Org (C-o)**: Enable/disable conversion to Org format
-   **Toggle Model Colors (c)**: Enable/disable model-specific colors
-   **Token Usage Graph (g)**: Display a visual graph of token usage


#### History Section {#history-section}

Commands for managing conversation history:

-   **Toggle History (H)**: Enable/disable conversation history
-   **Clear History (X)**: Clear the current history
-   **Display History (V)**: Show the conversation history
-   **Edit History (J)**: Edit the history in a buffer


#### Sessions Section {#sessions-section}

Commands for session management:

-   **New Session (N)**: Start a new session
-   **Load Session (L)**: Load a saved session
-   **Save Session (S)**: Save the current session
-   **List Sessions (Q)**: List all available sessions
-   **Delete Session (Z)**: Delete a saved session


#### Parameters Section {#parameters-section}

Commands for managing model parameters:

-   **Edit Parameter (P)**: Opens a submenu to edit specific parameters
-   **Display Parameters (G)**: Show current parameter settings
-   **Parameter Help (I)**: Display help information about parameters
-   **Reset Parameters (K)**: Reset parameters to defaults
-   **Toggle Params in Header (F)**: Show/hide parameters in header
-   **Parameter Profiles (p)**: Opens the parameter profiles submenu

When you select the Edit Parameter option, you'll see a comprehensive submenu of all available parameters:

```nil
Parameters
[Generation]                [More Generation]          [Mirostat]
t  Temperature              f  Frequency Penalty       M  Mirostat Mode
k  Top K                    s  Presence Penalty        T  Mirostat Tau
p  Top P                    n  Repeat Last N           E  Mirostat Eta
m  Min P                    x  Stop Sequences
y  Typical P                l  Penalize Newline
r  Repeat Penalty

[Resource]                  [More Resource]            [Memory]
c  Num Ctx                  P  Num Predict             m  Use MMAP
b  Num Batch                S  Seed                    L  Use MLOCK
g  Num GPU                  N  NUMA                    C  Num Thread
G  Main GPU                 V  Low VRAM
K  Num Keep                 o  Vocab Only

[Profiles]                  [Actions]
d  Default Profile          D  Display All
a  Creative Profile         R  Reset All
e  Precise Profile          H  Help
A  All Profiles             F  Toggle Display in Header
                            q  Back to Main Menu
```


### Parameter Profiles {#parameter-profiles}

Ollama Buddy includes predefined parameter profiles that can be applied with a single command. When you select "Parameter Profiles" from the main menu, you'll see:

```nil
Parameter Profiles
Current modified parameters: temperature, top_k, top_p
[Available Profiles]
d  Default
c  Creative
p  Precise

[Actions]
q  Back to Main Menu
```


### Commands Submenu {#commands-submenu}

The Commands submenu provides quick access to specialized operations:

```nil
Ollama Buddy Commands
[Code Operations]       [Language Operations]    [Pattern-based]         [Custom]
r  Refactor Code        l  Dictionary Lookup     f  Fabric Patterns      C  Custom Prompt
d  Describe Code        s  Synonym Lookup        u  Universal Patterns   m  Minibuffer Prompt
g  Git Commit Message   p  Proofread Text        c  Code Patterns

[Actions]
q  Back to Main Menu
```


### Direct Keybindings {#direct-keybindings}

For experienced users who prefer direct keybindings, all transient menu functions can also be accessed through keybindings with the prefix of your choice (or `C-c O` when in the chat minibuffer) followed by the key shown in the menu. For example:

-   `C-c O s` - Set system prompt
-   `C-c O m` - Switch model
-   `C-c O P` - Open parameter menu


### Customization {#customization}

The transient menu can be customized by modifying the `transient-define-prefix` definitions in the package. You can add, remove, or rearrange commands to suit your workflow.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-17 Mon&gt; </span></span> **0.9.5** {#0-dot-9-dot-5}

Added conversation history editing

-   Added functions to edit conversation history (`ollama-buddy-history-edit`, `ollama-buddy-history-save`, etc.).
-   Updated `ollama-buddy-display-history` to support history editing.
-   Added keybinding `C-c E` for history editing.

Introducing conversation history editing!!

**Key Features**

Now, you can directly modify past interactions, making it easier to refine and manage your `ollama-buddy` chat history.

Previously, conversation history was static, you could view it but not change it. With this update, you can now:

-   Edit conversation history directly in a buffer.
-   Modify past interactions for accuracy or clarity.
-   Save or discard changes with intuitive keybindings (`C-c C-c` to save, `C-c C-k` to cancel).
-   Edit the history of all models or a specific one.

Simply use the new command **`C-c E`** to open the conversation history editor. This will display your past interactions in an editable format (alist). Once you’ve made your changes, press `C-c C-c` to save them back into Ollama Buddy’s memory.

and with a universal argument you can leverage `C-c E` to edit an individual model.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-17 Mon&gt; </span></span> **0.9.1** {#0-dot-9-dot-1}

New simple basic interface is available.

As this package becomes more advanced, I've been adding more to the intro message, making it increasingly cluttered. This could be off-putting for users who just want a simple interface to a local LLM via Ollama.

Therefore I have decided to add a customization option to simplify the menu.

Note: all functionality will still be available through keybindings, so just like Emacs then! :)

Note: some could see this initially as a breaking change as the intro message will look different, but rest assured all the functionality is still there (just to re-emphasize), so if you have been using it before and want the original functionality/intro message, just set :

```nil
(setq ollama-buddy-interface-level 'advanced)
```

```elisp
(defcustom ollama-buddy-interface-level 'basic
  "Level of interface complexity to display.
'basic shows minimal commands for new users.
'advanced shows all available commands and features."
  :type '(choice (const :tag "Basic (for beginners)" basic)
                (const :tag "Advanced (full features)" advanced))
  :group 'ollama-buddy)
```

By default the menu will be set to Basic, unless obviously set explictly in an init file.  Here is an example of the basic menu:

```nil
*** Welcome to OLLAMA BUDDY

#+begin_example
 ___ _ _      n _ n      ___       _   _ _ _
|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |
| | | | | .  |     | .  | . | | | . | . |__ |
|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|
#+end_example

**** Available Models

  (a) another:latest     (d) jamesio:latest
  (b) funnyname2:latest  (e) tinyllama:latest
  (c) funnyname:latest   (f) llama:latest

**** Quick Tips

- Ask me anything!                    C-c C-c
- Change model                        C-c m
- Cancel request                      C-c k
- Browse prompt history               M-p/M-n
- Advanced interface (show all tips)  C-c A
```

and of the more advanced version

```nil
*** Welcome to OLLAMA BUDDY

#+begin_example
 ___ _ _      n _ n      ___       _   _ _ _
|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |
| | | | | .  |     | .  | . | | | . | . |__ |
|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|
#+end_example

**** Available Models

  (a) another:latest     (d) jamesio:latest
  (b) funnyname2:latest  (e) tinyllama:latest
  (c) funnyname:latest   (f) llama:latest

**** Quick Tips

- Ask me anything!                    C-c C-c
- Show Help/Token-usage/System-prompt C-c h/U/C-s
- Model Change/Info/Cancel            C-c m/i/k
- Prompt history                      M-p/M-n
- Session New/Load/Save/List/Delete   C-c N/L/S/Y/W
- History Toggle/Clear/Show           C-c H/X/V
- Prompt to multiple models           C-c l
- Parameter Edit/Show/Help/Reset      C-c P/G/I/K
- System Prompt/Clear   C-u/+C-u +C-u C-c C-c
- Toggle JSON/Token/Params/Format     C-c D/T/Z/C-o
- Basic interface (simpler display)   C-c A
- In another buffer? M-x ollama-buddy-menu
```


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-17 Mon&gt; </span></span> **0.9.0** {#0-dot-9-dot-0}

Added command-specific parameter customization

-   Added :parameters property to command definitions for granular control
-   Implemented functions to apply and restore parameter settings
-   Added example configuration to refactor-code command

With the latest update, you can now define specific parameter sets for each command in the menu, enabling you to optimize each AI interaction for its particular use case.

Different AI tasks benefit from different parameter settings. When refactoring code, you might want a more deterministic, precise response (lower temperature, higher repetition penalty), but when generating creative content, you might prefer more variation and randomness (higher temperature, lower repetition penalty). Previously, you had to manually adjust these parameters each time you switched between different types of tasks.

The new command-specific parameters feature lets you pre-configure the optimal settings for each use case. Here's how it works:


### Key Features {#key-features}

-   **Per-Command Parameter Sets**: Define custom parameter values for each command in your menu
-   **Automatic Application**: Parameters are applied when running a command and restored afterward
-   **Non-Destructive**: Your global parameter settings remain untouched
-   **Easy Configuration**: Simple interface for adding or updating parameters


### Example Configuration {#example-configuration}

```elisp
;; Define a command with specific parameters
(refactor-code
 :key ?r
 :description "Refactor code"
 :prompt "refactor the following code:"
 :system "You are an expert software engineer..."
 :parameters ((temperature . 0.2) (top_p . 0.7) (repeat_penalty . 1.3))
 :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))

;; Add parameters to an existing command
(ollama-buddy-add-parameters-to-command 'git-commit
 :temperature 0.4
 :top_p 0.9
 :repeat_penalty 1.1)

;; Update properties and parameters at once
(ollama-buddy-update-command-with-params 'describe-code
 :model "codellama:latest"
 :parameters '((temperature . 0.3) (top_p . 0.8)))
```

This feature is particularly useful for:

1.  **Code-related tasks**: Lower temperature for more deterministic code generation
2.  **Creative writing**: Higher temperature for more varied and creative outputs
3.  **Technical explanations**: Balanced settings for clear, accurate explanations
4.  **Summarization tasks**: Custom parameters to control verbosity and focus


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-16 Sun&gt; </span></span> **0.8.5** {#0-dot-8-dot-5}

Added system prompt support for commands

-   Introduced \`:system\` field to command definitions.
-   Added \`ollama-buddy-show-system-prompt\` to view active system prompt.
-   Updated UI elements to reflect system prompt status.

Previously, individual menu commands in `ollama-buddy` only included a user prompt. Now, each command can define a **system prompt**, which provides background context to guide the AI's responses. This makes interactions more precise and tailored.

**Key Features**

-   **System prompts per command**: Specify background instructions for each AI-powered command using the new `:system` field.
-   **View active system prompt**: Use `C-c C-s` to display the current system prompt in a dedicated buffer.
-   **Updated UI elements**: The status line now indicates whether a system prompt is active.

A helper function has also been added to update the default menu, for example, you might want to tweak a couple of things:

```elisp
(use-package ollama-buddy
  :bind ("C-c o" . ollama-buddy-menu)
  :custom
  (ollama-buddy-default-model "llama3.2:3b")
  :config
  (ollama-buddy-update-menu-entry
   'refactor-code
   :model "qwen2.5-coder:7b"
   :system "You are an expert software engineer who improves code and only mainly using the principles exhibited by Ada")
  (ollama-buddy-update-menu-entry
   'git-commit
   :model "qwen2.5-coder:3b"
   :system "You are a version control expert and mainly using subversion"))
```
