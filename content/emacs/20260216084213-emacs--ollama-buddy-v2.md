---
title: "Ollama Buddy v2.0 - LLMs can now call Emacs functions!"
author: ["James Dyer"]
lastmod: 2026-02-16T08:56:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

Tool calling has landed in ollama-buddy!, it's originally not something I really thought I would end up doing, but as ollama has provided tool enabled models and an API for this feature then I felt obliged to add it. So now LLMs through ollama can now actually do things inside Emacs rather than just talk about them, my original "do things only in the chat buffer and copy and paste" might have gone right out the window in an effort to fully support the ollama API!

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}

What is Tool Calling?

The basic idea is simple: instead of the model only generating text, it can request to invoke functions. You ask "what files are in my project?", and instead of guessing, the model calls list_directory, gets the real answer, and responds with actual information.

This creates a conversational loop:

1.  You send a prompt
2.  The model decides it needs to call a tool
3.  ollama-buddy executes the tool and feeds the result back
4.  The model generates a response using the real data
5.  Steps 2-4 repeat if more tools are needed

All of this is transparent - you just see the final response in the chat buffer.

The new ollama-buddy-tools.el module ships with 8 built-in tools:

Safe tools (read-only, enabled by default):

-   **read_file** - read file contents
-   **list_directory** - list directory contents
-   **get_buffer_content** - read an Emacs buffer
-   **list_buffers** - list open buffers with optional regex filtering
-   **search_buffer** - regex search within a buffer
-   **calculate** - evaluate math expressions via calc-eval

Unsafe tools (require safe mode off):

-   **write_file** - write content to files
-   **execute_shell** - run shell commands

Safe mode is on by default, so the model can only read - it can't modify anything unless you explicitly allow it, I think this is quite a nice simple implementation, at the moment I generally have safe mode off but always allowing confirmation for each tool action, but of course you can configure as necessary.

**Example Session**

With a tool-capable model like qwen3:8b and tools enabled (C-c W):

**&gt;&gt; PROMPT: What defuns are defined in ollama-buddy-tools.el?**

The model calls **search_buffer** with a regex pattern, gets the list of function definitions, and gives you a nicely formatted summary. No copy-pasting needed.

**Custom Tools**

You can register your own tools with ollama-buddy-tools-register:

```nil
  (ollama-buddy-tools-register
   'my-tool
   "Description of what the tool does"
   '((type . "object")
     (required . ["param1"])
     (properties . ((param1 . ((type . "string")
                                (description . "Parameter description"))))))
   (lambda (args)
     (let ((param1 (alist-get 'param1 args)))
       (format "Result: %s" param1)))
   t)  ; t = safe tool
```

The registration API takes a name, description, JSON schema for parameters, an implementation function, and a safety flag. The model sees the schema and decides when to call your tool based on the conversation.

A ⚒ symbol now appears next to tool-capable models everywhere - header line, model selector (C-c m), and model management buffer (C-c M). This follows the same pattern as the existing ⊙ vision indicator, so you can see at a glance which models support tools.

That's it. Pull a tool-capable model (qwen3, llama3.1, mistral, etc.) or use an online tool enabled model from ollama and start chatting. Next up is probably some web searching!, as again the ollama API supports this, so you will be able to pull in the latest from the interwebs to augment your prompt definition!
