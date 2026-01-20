---
title: "Ollama-Buddy 0.9.11: Experimental ChatGPT Integration, Customizable Streaming and Texinfo documentation"
author: ["James Dyer"]
lastmod: 2025-03-25T10:00:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg"
---

This week in [ollama-buddy](https://github.com/captainflasmr/ollama-buddy) updates, I have mostly been experimenting with ChatGPT integration! Yes, it is not a local LLM, so not ollama, hence entirely subverting the whole notion and fundamental principles of this package! This I know, and I don't care; I'm having fun. I use ChatGPT and would rather use it in Emacs through the now-familiar `ollama-buddy` framework, so why not? I'm also working on Claude integration too.

<!--more-->

My original principles of a no-config Emacs ollama integration still hold true, as by default, you will only see ollama models available. But with a little tweak to the configuration, with a require here and an API key there, you can now enable communication with an online AI. At the moment, I use Claude and ChatGPT, but if I can get Claude working, I might think about just adding in a basic template framework to easily slot in others. At the moment, there is a little too much internal ollama-buddy faffing to incorporate these external AIs into the main package, but I'm sure I can figure out a way to accommodate separate elisp external AIs.

{{< figure src="/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg" width="100%" >}}

In other `ollama-buddy` news, I have now added support for the `stream` variable in the ollama API. By default, I had streaming on, and I guess why wouldn't you? It is a chat, and you would want to see "typing" or tokens arriving as they come in?. But to support more of the API, you can toggle it on and off, so if you want, you can sit there and wait for the response to arrive in one go and maybe it can be less distracting (and possibly more efficient?).

Just a note back on the topic of online AI offerings: to simplify those integrations, I just disabled streaming for the response to arrive in one shot. Mainly, I just couldn't figure out the ChatGPT streaming, and for an external offering, I wasn't quite willing to spend more time on it, and due to the speed of these online behemoths, do you really need to see each token come in as it arrives?

Oh, there is something else too, something I have been itching to do for a while now, and that is to write a Texinfo document so a manual can be viewed in Emacs. Of course, this being an AI-based package, I fed in my `ollama-buddy` files and got Claude to generate one for me (I have a baby and haven't the time!). Reading through it, I think it turned out pretty well :) It hasn't been made automatically available on MELPA yet, as I need to tweak the recipe, but you can install it for yourself.

Anyways, see below for the changelog gubbins:


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-24 Mon&gt; </span></span> **0.9.11** {#0-dot-9-dot-11}

Added the ability to toggle streaming on and off

-   Added customization option to enable/disable streaming mode
-   Implemented toggle function with keybindings (C-c x) and transient menu option
-   Added streaming status indicator in the modeline

The latest update introduces the ability to toggle between two response modes:

-   **Streaming mode (default)**: Responses appear token by token in real-time, giving you immediate feedback as the AI generates content.
-   **Non-streaming mode**: Responses only appear after they're fully generated, showing a "Loading response..." placeholder in the meantime.

While watching AI responses stream in real-time is often helpful, there are situations where you might prefer to see the complete response at once:

-   When working on large displays where the cursor jumping around during streaming is distracting
-   When you want to focus on your work without the distraction of incoming tokens until the full response is ready

The streaming toggle can be accessed in several ways:

1.  Use the keyboard shortcut `C-c x`
2.  Press `x` in the transient menu
3.  Set the default behaviour through customization:
    ```elisp
          (setq ollama-buddy-streaming-enabled nil) ;; Disable streaming by default
    ```

The current streaming status is visible in the modeline indicator, where an "X" appears when streaming is disabled.


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-22 Sat&gt; </span></span> **0.9.10** {#0-dot-9-dot-10}

Added experimental OpenAI support!

Yes, that's right, I said I never would do it, and of course, this package is still very much `ollama`-centric, but I thought I would just sneak in some rudimentary ChatGPT support, just for fun!

It is a very simple implementation, I haven't managed to get streaming working, so Emacs will just show "Loading Response..." as it waits for the response to arrive. It is asynchronous, however, so you can go off on your Emacs day while it loads (although being ChatGPT, you would think the response would be quite fast!)

By default, OpenAI/ChatGPT will not be enabled, so anyone wanting to use just a local LLM through `ollama` can continue as before. However, you can now sneak in some experimental ChatGPT support by adding the following to your Emacs config as part of the `ollama-buddy` set up.

```elisp
(require 'ollama-buddy-openai nil t)
(setq ollama-buddy-openai-api-key "<big long key>")
```

and you can set the default model to ChatGPT too!

```elisp
(setq ollama-buddy-default-model "GPT gpt-4o")
```

With this enabled, chat will present a list of ChatGPT models to choose from. The custom menu should also now work with chat, so from anywhere in Emacs, you can push predefined prompts to the `ollama` buddy chat buffer now supporting ChatGPT.

There is more integration required to fully incorporate ChatGPT into the `ollama` buddy system, like token rates and history, etc. But not bad for a first effort, methinks!

Here is my current config, now mixing ChatGPT with `ollama` models:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-openai-api-key "<very long key>")
  (ollama-buddy-default-model "GPT gpt-4o")
  :config
  (require 'ollama-buddy-openai nil t)
  (ollama-buddy-update-menu-entry
   'refactor-code :model "qwen2.5-coder:7b")
  (ollama-buddy-update-menu-entry
   'git-commit :model "qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'describe-code :model "qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'dictionary-lookup :model "llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'synonym :model "llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'proofread :model "GPT gpt-4o")
  (ollama-buddy-update-menu-entry
   'custom-prompt :model "deepseek-r1:7b"))
```


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-22 Sat&gt; </span></span> **0.9.9** {#0-dot-9-dot-9}

Added texinfo documentation for future automatic installation through MELPA and created an Emacs manual.

If you want to see what the manual would look like, just download the docs directory from github, cd into it, and run:

```bash
make
sudo make install-docs
```

Then calling up `info` `C-h i` and ollama buddy will be present in the Emacs menu, or just select `m` and search for `Ollama Buddy`

For those interested in the manual, I have converted it into html format, which is accessible here:

[{{< relref "ollama-buddy" >}}]({{< relref "ollama-buddy" >}})

It has been converted using the following command:

```bash
makeinfo --html --no-split ollama-buddy.texi -o ollama-buddy.html
pandoc -f html -t org -o ollama-buddy.org ollama-buddy.html
```


## <span class="timestamp-wrapper"><span class="timestamp">&lt;2025-03-20 Thu&gt; </span></span> **0.9.9** {#0-dot-9-dot-9}

Intro message with model management options (select, pull, delete) and option for recommended models to pull

-   Enhance model management and selection features
-   Display models available for download but not yet pulled
