---
title: "Setting up a C# Language Server (csharp-ls) on an Air-Gapped Windows System"
author: ["James Dyer"]
lastmod: 2025-06-27T22:27:00+01:00
tags: ["windows", "emacs", "eglot", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20250627222703-emacs--Setting-up-CSharp-Language-Server-csharp-ls-on-Air-Gapped-Windows-System.jpg"
---

For a little while now, I have been developing C# applications at work and on Windows!, possibly the perfect storm for Emacs, but actually, it copes pretty well, especially with eglot and the LSP server OmniSharp.

<!--more-->

But recently my work setup has become even more air-gapped, and unfortunately, I reached a point where I couldn't get eglot and OmniSharp working together. I gave it a really good try, but I think the mishmash and dependency installation madness finally took a toll.

It has something to do with trying to find MSBuild.exe, but due to my potentially wonky installation, I couldn't figure out how to detect it. If Visual Studio is installed on the system, there are no problems; however, offline installation, even of the Community edition, is not an easy task.

So, the solution? Well, I thought I would try another C# LSP in the form of csharp-ls. Its deliverable form is a NuGet package, which can be extracted, placed in the relevant location, and then you're pretty much off and running!

{{< figure src="/emacs/20250627222703-emacs--Setting-up-CSharp-Language-Server-csharp-ls-on-Air-Gapped-Windows-System.jpg" width="100%" >}}

So here is how I did it:


## Step 1: Download csharp-ls nuget Components {#step-1-download-csharp-ls-nuget-components}

On a machine with internet access (which fortunately I do have access to):


### Download csharp-ls {#download-csharp-ls}

-   Go to the [csharp-ls NuGet page](https://www.nuget.org/packages/csharp-ls/)
-   Click "Download package" to get the `.nupkg` file
-   Rename the file from `.nupkg` to `.zip`
-   Extract the contents


## Step 2: Install csharp-ls (Airgapped Machine) {#step-2-install-csharp-ls--airgapped-machine}

1.  Create a directory in your Emacs installation:
    ```cmd
          mkdir "c:\path\to\emacs\bin\csharp-ls"
    ```

2.  Copy the extracted csharp-ls contents to this directory. The key file you need is:
    ```nil
          tools\net9.0\any\CSharpLanguageServer.dll
    ```


## Step 3: Configure Emacs {#step-3-configure-emacs}

Add this configuration to your Emacs init file:

```elisp
;; Configure csharp-ls as the language server for C#
(setq eglot-server-programs
      '((csharp-mode . ("dotnet" "c:/path/to/emacs/bin/csharp-ls/tools/net9.0/any/CSharpLanguageServer.dll"))))
```


## Step 4: Set Up Your C# Project Structure {#step-4-set-up-your-c-project-structure}

Basically defining a source code collection as a project in Emacs. This can be a solution file, a source code control directory or as I generally have set up, an empty .project file


## Step 5: Set up paths {#step-5-set-up-paths}

This may not be needed, but for my own sanity and just in case, absorb the new directory in to the PATH and exec-path variables.

```elisp
(when (eq system-type 'windows-nt)
  (let ((xPaths
         `(
           ,(expand-file-name "~/bin/csharp-ls/tools/net9.0/any")
           )))
    (setenv "PATH" (concat
                    (mapconcat 'identity xPaths ";")))
    (setq exec-path (append (split-string winPaths ";") xPaths (list "." exec-directory)))))
```

So it works as I had it with `OmniSharp` and that really is all I can ask for, lets see how I get on!
