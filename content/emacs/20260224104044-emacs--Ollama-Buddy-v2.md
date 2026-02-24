---
title: "Ollama Buddy v2.5 - RAG (Retrieval-Augmented Generation) Support"
author: ["James Dyer"]
lastmod: 2026-02-24T11:50:00+00:00
tags: ["ollama-buddy", "ollama", "emacs", 2026]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg"
---

One of the things that has always slightly bothered me about chatting with a local LLM is that it only knows what it was trained on (although I suppose most LLMs are like that) . Ask it about your own codebase, your org notes, your project docs - and it's just guessing. Well, not anymore! Ollama Buddy now ships with proper Retrieval Augmented Generation support built-in

<!--more-->

{{< figure src="/emacs/20250424085731-emacs--Ollama-Buddy-0-9-35-Grok-Gemini-Integration-Enhanced-Sessions.jpg" width="100%" >}}


## What even is RAG? {#what-even-is-rag}

If you haven't come across the term before, the basic idea is simple. Instead of asking the LLM a question cold, you first go off and find the most relevant bits of text from your own documents, then you hand those bits to the LLM along with your question. The LLM now has actual context to work with rather than just vibes. The "retrieval" part is done using vector embeddings - each chunk of your documents gets turned into a mathematical representation, and at query time your question gets the same treatment. Chunks that are mathematically "close" to your question are the ones that get retrieved.

In this case, I have worked to keep the whole pipeline inside Emacs; it talks to Ollama directly to contact an embedding model, which then returns the required information. I have tried to make this as Emacs Org-friendly as possible by storing the embedding information in Org files.


## Getting started {#getting-started}

You'll need an embedding model pulled alongside your chat model. The default is `nomic-embed-text` which is a solid general-purpose choice:

```sh
ollama pull nomic-embed-text
```

or just do it within ollama-buddy from the Model Management page.


## Indexing your documents {#indexing-your-documents}

The main entry point is `M-x ollama-buddy-rag-index-directory`. Point it at a directory and it will crawl through, chunk everything up, generate embeddings for each chunk, and save an index file. The first time you run this it can take a while depending on how much content you have and how fast your machine is - subsequent updates are much quicker as it only processes changed files.

Supported file types (and I even managed to get pdf text extraction working!):

-   Emacs Lisp (`.el`)
-   Python, JavaScript, TypeScript, Go, Rust, C/C++, Java, Ruby - basically most languages
-   Org-mode and Markdown
-   Plain text
-   PDF files (if you have `pdftotext` from poppler-utils installed)
-   YAML, TOML, JSON, HTML, CSS

Files over 1MB are skipped (configurable), and the usual suspects like `.git`, `node_modules`, `__pycache__` are excluded automatically.

The index gets saved into `~/.emacs.d/ollama-buddy/rag-indexes/` as a `.rag` file named after the directory. You can see what you've got with `M-x ollama-buddy-rag-list-indexes`.


## The chunking strategy {#the-chunking-strategy}

One thing I'm quite happy with here is the chunking. Rather than just splitting on a fixed character count, documents are split into overlapping word-based chunks. The defaults are:

```elisp
(setq ollama-buddy-rag-chunk-size 400)    ; ~500 tokens per chunk
(setq ollama-buddy-rag-chunk-overlap 50)  ; 50-word overlap between chunks
```

The overlap is important - it means a piece of information that sits right at a chunk boundary doesn't get lost. Each chunk also tracks its source file and line numbers, so you can see exactly where a result came from.


## Searching and attaching context {#searching-and-attaching-context}

Once you have an index, there are two main ways to use it:

-   `M-x ollama-buddy-rag-search` - searches and displays the results in a dedicated buffer so you can read through them

-   `M-x ollama-buddy-rag-attach` - searches and attaches the results directly to your chat context

The second one is the useful one for day-to-day work. After running it, your next chat message will automatically include the retrieved document chunks as context. The status line shows `♁N` (where N is the number of attached searches) so you always know what context is in play. Clear everything with `M-x ollama-buddy-clear-attachments` or `C-c 0`.

You can also trigger searches inline using the `@rag()` syntax directly in your prompt and is something fun I have been working on to include an inline command language of sorts, but more about that in a future post.

The similarity search uses cosine similarity with sensible defaults (hopefully!)

```elisp
(setq ollama-buddy-rag-top-k 5)                  ; return top 5 matching chunks
(setq ollama-buddy-rag-similarity-threshold 0.3)  ; filter out low-relevance results
```

Bump `top-k` if you want more context, lower the threshold if you're not getting enough results.


## A practical example {#a-practical-example}

Say you've been working on a large Emacs package and you want the LLM to help you understand something specific. You'd do:

1.  `M-x ollama-buddy-rag-index-directory` → point at your project directory
2.  Wait for indexing to complete (the chat header-line shows progress)
3.  `M-x ollama-buddy-rag-attach` → type your search query, e.g. "streaming filter process"
4.  Ask your question in the chat buffer as normal

The LLM now has the relevant source chunks as context and can give you a much more grounded answer than it would cold.

And the important aspect, especially regarding local models which don't often have the huge context sizes often found in online LLMs is that it allows for very efficient context retrieval.


## That's pretty much it! {#that-s-pretty-much-it}

The whole thing is self-contained inside Emacs, no external packages or vector databases, you index once, search as needed, and the LLM gets actual information rather than hallucinating answers about your codebase or anything else that you would want to ingest and it will hopefully make working with local LLMs through ollama noticeably more useful and accurate.
