---
title: "Ollama-Buddy 0.9.20: Curated AI Prompting with Awesome ChatGPT Prompts"
author: ["James Dyer"]
lastmod: 2025-04-09T13:43:00+01:00
tags: ["ollama-buddy", "ollama", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg"
---

-   Added `ollama-buddy-awesome.el` to integrate Awesome ChatGPT Prompts.

`ollama-buddy-awesome` is an `ollama-buddy` extension that integrates the popular [Awesome ChatGPT Prompts](https://github.com/f/awesome-chatgpt-prompts) repository, allowing you to leverage hundreds of curated prompts for various tasks and roles right within your Emacs environment, I thought that since I have integrated the `fabric` set of curated prompts, so then why not these!

<!--more-->

{{< figure src="/emacs/20250325093201-emacs--Ollama-Buddy-0-9-11-Experimental-ChatGPT-Integration-Customizable-AI-Streaming-and-Texinfo-documentation.jpg" width="100%" >}}

There is a video demonstration here : <https://www.youtube.com/watch?v=5A4bTvjmPeo>


## Key Features {#key-features}

1.  **Seamless Sync**: Automatically fetch the latest prompts from the GitHub repository, ensuring you always have access to the most up-to-date collection.

2.  **Smart Categorization**: Prompts are intelligently categorized based on their content, making it easy to find the perfect prompt for your task.

3.  **Interactive Selection**: Choose prompts through Emacs' familiar completion interface, with category and title information for quick identification.

4.  **Effortless Application**: Apply selected prompts as system prompts in ollama-buddy with a single command, streamlining your AI-assisted workflow.

5.  **Prompt Management**: List available prompts, preview their content, and display full prompt details on demand.


## Getting Started {#getting-started}

To access the Awesome ChatGPT prompts, just select the transient menu as normal and select "[a] Awesome ChatGPT Prompts", this will fetch the prompts and prepare everything for your first use and give you a transient menu as follows:

```text
Actions
[s] Send with Prompt
[p] Set as System Prompt
[l] List All Prompts
[c] Category Browser
[S] Sync Latest Prompts
[q] Back to Main Menu
```

Now available are a vast array of role-based and task-specific prompts, enhancing your `ollama-buddy` interactions in Emacs!
