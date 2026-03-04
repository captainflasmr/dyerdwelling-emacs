---
title: "Ollama Buddy - Web Search Integration"
author: ["James Dyer"]
lastmod: 2026-03-04T09:34:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

One of the fundamental limitations of local LLMs is their knowledge cutoff - they don't know about anything that happened after their training data ended. The new web search integration in [ollama-buddy](https://github.com/captainflasmr/ollama-buddy) solves this by fetching current information from the web and injecting it into your conversation context.  Ollama has a specific API web search section, so it has now been activated!

<!--more-->

Here is a demonstration:

<https://www.youtube.com/watch?v=05VzAajH404>

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}

The web search feature implements a multi-stage pipeline that transforms search queries into clean, LLM-friendly context, your search query is sent to Ollama's Web Search API, the API returns structured search results with URLs and snippets.

I have decided that each URL by default is fetched and processed through Emacs' built-in `eww` and `shr` HTML rendering, but this can of course be configured, set `ollama-buddy-web-search-content-source` to control how content is retrieved:

-   \`eww' (default): Fetch each URL and render through eww/shr for clean text
-   \`api': Use content returned directly from Ollama API (faster, less refined)

The `shr` (Simple HTML Renderer) library does an excellent job of converting HTML to readable plain text, stripping ads, navigation, and other noise, so I thought why not just use this rather than the return results from the ollama API, as they didn't seem to be particularly accurate.

The cleaned text is formatted with org headings showing the source URL and attached to your conversation context, so when you send your next prompt, the search results are automatically included in the context. The LLM can now reason about current information as if it had this knowledge all along.

There are multiple ways to search; firstly, is inline `@search()` syntax in your prompts (gradually expanding the inline prompting language!), so for example:

```nil
What are the key improvements in @search(Emacs 31 new features)?

Compare @search(Rust async programming) with @search(Go concurrency model)
```

ollama-buddy automatically detects these markers, executes the searches, attaches the results, and then sends your prompt, so you can carry out multiple searches.

You can also manual Search and Attach, Use `C-c / a` (or `M-x ollama-buddy-web-search-attach`)

The search executes, results are attached to your session, and the `♁1` indicator appears in the header line and the results can be viewed from the attachments menu, so for example would display something like:

```text
* Web Searches (1)
** latest Emacs 31 features
*** 1. Hide Minor Modes in the Modeline in Emacs 31
*** 2. New Window Commands For Emacs 31
*** 3. Latest version of Emacs (GNU Emacs FAQ)
*** 4. bug#74145: 31.0.50; Default lexical-binding to t
*** 5. New in Emacs 30 (GNU Emacs FAQ)
```

with each header foldable, containing the actual search results.

There is a little configuration required to go through the ollama API, first, get an API key from <https://ollama.com/settings/keys> (it's free). Then configure:

```elisp
(use-package ollama-buddy
  :bind
  ("C-c o" . ollama-buddy-role-transient-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  ;; Required: Your Ollama web search API key
  (ollama-buddy-web-search-api-key "your-api-key-here"))
```

For clarification, the content source options are as follows:

The `ollama-buddy-web-search-content-source` variable controls how content is retrieved:

**`eww` (default, recommended)**

Fetches each URL and renders HTML through Emacs' eww/shr. Produces cleaner, more complete content but requires additional HTTP requests.

Pros:

-   Much cleaner text extraction
-   Full page content, not just snippets
-   Removes ads, navigation, clutter
-   Works with any website

Cons:

-   Slightly slower (additional HTTP requests)
-   Requires network access for each URL

**`api` (experimental)**

Uses content returned directly from the Ollama API without fetching individual URLs. Faster but content quality depends on what the API provides.

Pros:

-   Faster (single API call)
-   Less network traffic

Cons:

-   Content may be truncated
-   Quality varies by source
-   May miss important context

I strongly recommend sticking with `eww` - the quality difference is substantial.

By default, web search fetches up to 5 URLs with 2000 characters per result. This provides rich context without overwhelming the LLM's context window.

For longer research sessions, you can adjust:

```elisp
(setq ollama-buddy-web-search-max-results 10)      ;; More sources
(setq ollama-buddy-web-search-snippet-length 5000) ;; Longer excerpts
```

Be mindful of your LLM's context window limits. With 5 results at 2000 chars each, you're adding ~10K characters to your context.

The web search integration fundamentally expands what your local LLMs can do. They're no longer limited to their training data - they can reach out, fetch current information, and reason about it just like they would with any other context, so hopefully this will now make `ollama-buddy` a little more useful
