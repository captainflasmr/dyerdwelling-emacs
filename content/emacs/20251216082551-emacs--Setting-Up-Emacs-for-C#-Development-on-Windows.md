---
title: "Setting Up Emacs for C# Development on Windows"
author: ["James Dyer"]
lastmod: 2025-12-16T08:25:00+00:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20251216082551-emacs--Setting-Up-Emacs-for-Csharp-Development-on-Windows.jpg"
---

{{< figure src="/emacs/20251216082551-emacs--Setting-Up-Emacs-for-Csharp-Development-on-Windows.jpg" width="100%" >}}


## Introduction {#introduction}

I have been developing C# with .NET 9.0 for the last year on Windows and I thought it was probably time to write down my current setup, and maybe someone might even find this useful!

So, this guide documents my setup for running Emacs 30.1 on Windows with full C# development support, including LSP, debugging (through DAPE), and all the ancillary tools you'd expect from a modern development environment. The setup is designed to be portable and self-contained, which is particularly useful in air-gapped or restricted environments.

A version of this can be found at <https://github.com/captainflasmr/Emacs-on-windows> which will be a living continually updated version!

<!--more-->


## Prerequisites {#prerequisites}

Before we begin, you'll need:

-   **Windows 10 or 11** (64-bit)
-   **.NET 9.0 SDK** - Required for csharp-ls and building .NET projects
-   **Visual Studio 2022** (optional) - Useful for MSBuild and if you need the full IDE occasionally
-   **Administrator access** - For initial setup only

You can verify your .NET installation by opening a command prompt and running:

```shell
dotnet --version
```

If you see version 9.0.x or later, you're ready to proceed.


## The Big Picture {#the-big-picture}

Here's what we're building:

```text
D:\source\emacs-30.1\
├── bin\
│   ├── emacs.exe, runemacs.exe, etc.
│   ├── PortableGit\          # Git for version control
│   ├── Apache-Subversion\    # SVN (if needed)
│   ├── csharp-ls\            # C# Language Server
│   ├── netcoredbg\           # .NET debugger
│   ├── omnisharp-win-x64\    # Alternative C# LSP
│   ├── hunspell\             # Spell checking
│   ├── find\                 # ripgrep for fast searching
│   ├── ffmpeg-7.1.1-.../     # Video processing
│   └── ImageMagick-.../      # Image processing
└── share\
    └── emacs\...
```

The key insight here is keeping everything within the Emacs installation directory. This makes the whole setup portable—you can copy it to another machine or keep it on a USB drive.


## Step 1: Installing Emacs {#step-1-installing-emacs}

Download Emacs 30.1 from the [GNU Emacs download page](https://www.gnu.org/software/emacs/download.html). For Windows, grab the installer or the zip archive.

I install to an external drive `D:\source\emacs-30.1` rather than Program Files—it avoids permission issues and keeps everything in one place.

Test your installation by running `bin\runemacs.exe`. You should see a fresh Emacs frame.


## Step 2: Setting Up csharp-ls (The C# Language Server) {#step-2-setting-up-csharp-ls--the-c-language-server}

This is the heart of the C# development experience. `csharp-ls` provides code completion, go-to-definition, find references, diagnostics, and more through the Language Server Protocol (LSP).


### Option A: Installing via dotnet tool (Recommended for Internet Access) {#option-a-installing-via-dotnet-tool--recommended-for-internet-access}

If you have internet access, the easiest way to install csharp-ls is as a .NET global tool:

```shell
# Install the latest version globally
dotnet tool install --global csharp-ls

# Or install a specific version
dotnet tool install --global csharp-ls --version 0.20.0

# Verify installation
csharp-ls --version
```

By default, global tools are installed to:

-   Windows: `%USERPROFILE%\.dotnet\tools`

The executable will be `csharp-ls.exe` and can be called directly once the tools directory is in your PATH.


### Option B: Offline Installation via NuGet Package {#option-b-offline-installation-via-nuget-package}

For air-gapped environments, you can download the NuGet package and extract it manually:

1.  On a machine with internet, download the package:
    ```shell
          # Download the nupkg file
          nuget install csharp-ls -Version 0.20.0 -OutputDirectory ./packages

          # Or download directly from NuGet Gallery:
          # https://www.nuget.org/packages/csharp-ls/
          # Click "Download package" on the right side
    ```

2.  The `.nupkg` file is just a ZIP archive. Extract it:
    ```shell
          # Rename to .zip and extract, or use 7-Zip
          # The DLLs are in tools/net9.0/any/
    ```

3.  Copy the `tools/net9.0/any/` directory to your Emacs bin:
    ```shell
          xcopy /E packages\csharp-ls.0.20.0\tools\net9.0\any D:\source\emacs-30.1\bin\csharp-ls\
    ```

4.  The language server is now at:
    `D:\source\emacs-30.1\bin\csharp-ls\CSharpLanguageServer.dll`


### Configuring Eglot for csharp-ls {#configuring-eglot-for-csharp-ls}

In your `init.el`, configure Eglot to use csharp-ls:

```emacs-lisp
(require 'eglot)

;; Option A: If installed as a global tool
(setq eglot-server-programs
      '((csharp-mode . ("csharp-ls"))))

;; Option B: If running from extracted DLL
(setq eglot-server-programs
      '((csharp-mode . ("dotnet"
                        "D:/source/emacs-30.1/bin/csharp-ls/CSharpLanguageServer.dll"))))
```

I also have the following commented out if there are some eglot functions that causes slowdowns or I just think I don't need:

```elisp
(setq eglot-ignored-server-capabilities
      '(
        ;; :hoverProvider                    ; Documentation on hover
        ;; :completionProvider               ; Code completion
        ;; :signatureHelpProvider            ; Function signature help
        ;; :definitionProvider               ; Go to definition
        ;; :typeDefinitionProvider           ; Go to type definition
        ;; :implementationProvider           ; Go to implementation
        ;; :declarationProvider              ; Go to declaration
        ;; :referencesProvider               ; Find references
        ;; :documentHighlightProvider        ; Highlight symbols automatically
        ;; :documentSymbolProvider           ; List symbols in buffer
        ;; :workspaceSymbolProvider          ; List symbols in workspace
        ;; :codeActionProvider               ; Execute code actions
        ;; :codeLensProvider                 ; Code lens
        ;; :documentFormattingProvider       ; Format buffer
        ;; :documentRangeFormattingProvider  ; Format portion of buffer
        ;; :documentOnTypeFormattingProvider ; On-type formatting
        ;; :renameProvider                   ; Rename symbol
        ;; :documentLinkProvider             ; Highlight links in document
        ;; :colorProvider                    ; Decorate color references
        ;; :foldingRangeProvider             ; Fold regions of buffer
        ;; :executeCommandProvider           ; Execute custom commands
        ;; :inlayHintProvider                ; Inlay hints
        ))
```


## Step 3: Setting Up the Debugger (netcoredbg) {#step-3-setting-up-the-debugger--netcoredbg}

For debugging .NET applications, we'll use `netcoredbg`, which implements the Debug Adapter Protocol (DAP).


### Installing netcoredbg {#installing-netcoredbg}

1.  Download from [Samsung's GitHub releases](https://github.com/Samsung/netcoredbg/releases)
2.  Extract to `D:\source\emacs-30.1\bin\netcoredbg\`
3.  Verify: `netcoredbg.exe --version`


### Configuring dape for Debugging {#configuring-dape-for-debugging}

`dape` is an excellent DAP client for Emacs. Here's my configuration:

```emacs-lisp
(use-package dape
  :load-path "z:/SharedVM/source/dape-master"
  :init
  ;; Set key prefix BEFORE loading dape
  (setq dape-key-prefix (kbd "C-c d"))
  :config
  ;; Define common configuration
  (defvar project-netcoredbg-path "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.exe"
    "Path to netcoredbg executable.")
  (defvar project-netcoredbg-log "d:/source/emacs-30.1/bin/netcoredbg/netcoredbg.log"
    "Path to netcoredbg log file.")
  (defvar project-project-root "d:/source/PROJECT"
    "Root directory of PROJECT project.")
  (defvar project-build-config "Debug"
    "Build configuration (Debug or Release).")
  (defvar project-target-arch "x64"
    "Target architecture (x64, x86, or AnyCPU).")

  ;; Helper function to create component configs
  (defun project-dape-config (component-name dll-name &optional stop-at-entry)
    "Create a dape configuration for a component.
COMPONENT-NAME is the component directory name
DLL-NAME is the DLL filename without extension.
STOP-AT-ENTRY if non-nil, stops at program entry point."
    (let* ((component-dir (format "%s/%s" project-project-root component-name))
           (bin-path (format "%s/bin/%s/%s/net9.0"
                             component-dir
                             project-target-arch
                             project-build-config))
           (dll-path (format "%s/%s.dll" bin-path dll-name))
           (config-name (intern (format "netcoredbg-launch-%s"
                                        (downcase component-name)))))
      `(,config-name
        modes (csharp-mode csharp-ts-mode)
        command ,project-netcoredbg-path
        command-args (,(format "--interpreter=vscode")
                      ,(format "--engineLogging=%s" project-netcoredbg-log))
        normalize-path-separator 'windows
        :type "coreclr"
        :request "launch"
        :program ,dll-path
        :cwd ,component-dir
        :console "externalTerminal"
        :internalConsoleOptions "neverOpen"
        :suppressJITOptimizations t
        :requireExactSource nil
        :justMyCode t
        :stopAtEntry ,(if stop-at-entry t :json-false))))

  ;; Register all component configurations
  (dolist (config (list
                   (project-dape-config "DM" "DM.MSS" t)
                   (project-dape-config "Demo" "Demo.MSS" t)
                   (project-dape-config "Test_001" "Test" t)))
    (add-to-list 'dape-configs config))

  ;; Set buffer arrangement and other options
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-debug t)
  (setq dape-repl-echo-shell-output t))
```

Now you can start debugging with `M-x dape` and selecting your configuration.


## Step 4: Installing Supporting Tools {#step-4-installing-supporting-tools}


### Portable Git {#portable-git}

1.  Download `PortableGit-2.50.0-64-bit.7z.exe` from [git-scm.com](https://git-scm.com/download/win)
2.  Run and extract to `D:\source\emacs-30.1\bin\PortableGit\`

This gives you `git.exe`, `bash.exe`, and a whole Unix-like environment.


### ripgrep (Fast Searching) {#ripgrep--fast-searching}

1.  Download from [ripgrep releases](https://github.com/BurntSushi/ripgrep/releases)
2.  Extract `rg.exe` to `D:\source\emacs-30.1\bin\find\`

ripgrep is dramatically faster than grep for searching codebases.


### Hunspell (Spell Checking) {#hunspell--spell-checking}

1.  Download `hunspell-1.3.2-3-w32-bin.zip`
2.  Extract to `D:\source\emacs-30.1\bin\hunspell\`
3.  Download dictionary files (`en_GB.dic` and `en_GB.aff`) and place in `hunspell\share\hunspell\`


### ImageMagick (Image Processing) {#imagemagick--image-processing}

1.  Download the portable Q16 x64 version from [imagemagick.org](https://imagemagick.org/script/download.php)
2.  Extract to `D:\source\emacs-30.1\bin\ImageMagick-7.1.2-9-portable-Q16-x64\`

This enables `image-dired` thumbnail generation.


### FFmpeg (Video Processing) {#ffmpeg--video-processing}

1.  Download from [ffmpeg.org](https://ffmpeg.org/download.html) (essentials build is fine)
2.  Extract to `D:\source\emacs-30.1\bin\ffmpeg-7.1.1-essentials_build\`

Useful for video thumbnails in dired and media processing.


## Step 5: Configuring the PATH {#step-5-configuring-the-path}

This is crucial—Emacs needs to find all these tools. Here's the PATH configuration from my `init.el`:

```emacs-lisp
(when (eq system-type 'windows-nt)
  (let* ((emacs-bin "d:/source/emacs-30.1/bin")
         (xPaths
          `(,emacs-bin
            ,(concat emacs-bin "/PortableGit/bin")
            ,(concat emacs-bin "/PortableGit/usr/bin")
            ,(concat emacs-bin "/hunspell/bin")
            ,(concat emacs-bin "/find")
            ,(concat emacs-bin "/netcoredbg")
            ,(concat emacs-bin "/csharp-ls/tools/net9.0/any")
            ,(concat emacs-bin "/ffmpeg-7.1.1-essentials_build/bin")
            ,(concat emacs-bin "/ImageMagick-7.1.2-9-portable-Q16-x64")))
         (winPaths (getenv "PATH")))
    (setenv "PATH" (concat (mapconcat 'identity xPaths ";") ";" winPaths))
    (setq exec-path (append xPaths (parse-colon-path winPaths)))))
```


## Step 6: Installing Emacs Packages {#step-6-installing-emacs-packages}

Extract these to a shared location or download from MELPA

| Package                 | Purpose                       |
|-------------------------|-------------------------------|
| corfu                   | Modern completion UI          |
| dape                    | Debug Adapter Protocol client |
| highlight-indent-guides | Visual indentation guides     |
| ztree                   | Directory tree comparison     |
| web-mode                | Web template editing          |

Example package configuration:

```emacs-lisp
(use-package corfu
  :load-path "z:/SharedVM/source/corfu-main"
  :custom
  (corfu-auto nil)         ; Manual completion trigger
  (corfu-cycle t)          ; Cycle through candidates
  (corfu-preselect 'first))

(use-package ztree
  :load-path "z:/SharedVM/source/ztree"
  :config
  (setq ztree-diff-filter-list
        '("build" "\\.dll" "\\.git" "bin" "obj"))
  (global-set-key (kbd "C-c z d") 'ztree-diff))

(use-package web-mode
  :load-path "z:/SharedVM/source/web-mode-master"
  :mode "\\.cshtml?\\'"
  :hook (html-mode . web-mode)
  :bind (:map web-mode-map ("M-;" . nil)))
```

Note that I turn off autocomplete for corfu and complete using `complete-symbol` manually, otherwise the LSP is constantly accessed with slowdown.

I often use `Meld` but am currently am looking to adapt `ztree` to perform better for directory comparisons.

Web-mode is the best package I have found for html type file navigation and folding, very useful when developing Razor pages for example.


## Step 7: auto open file modes {#step-7-auto-open-file-modes}

Of course running and building in windows means in Emacs probably having to open .csproj files from time to time, well `nxml-mode` comes in useful for this:

```elisp
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
```


## Step 8: build script {#step-8-build-script}

Here is my general build script, leveraging msbuild and running generally from `eshell`

New projects are added to :

```bat
set PROJECTS
set PROJECT_NAMES
```

```bat
@echo off
setlocal

REM =================================================================
REM Build Management Script
REM =================================================================
REM Usage: build-selected.bat [action] [verbosity] [configuration] [platform]
REM   action: build, clean, restore, rebuild (default: build)
REM   verbosity: quiet, minimal, normal, detailed, diagnostic (default: minimal)
REM   configuration: Debug, Release (default: Debug)
REM   platform: x64, x86, "Any CPU" (default: x64)
REM =================================================================

REM Set defaults
set ACTION=%1
set VERBOSITY=%2
set CONFIGURATION=%3
set PLATFORM=%4

if "%ACTION%"=="" set ACTION=build
if "%VERBOSITY%"=="" set VERBOSITY=minimal
if "%CONFIGURATION%"=="" set CONFIGURATION=Debug
if "%PLATFORM%"=="" set PLATFORM=x64

echo Build Script - Action=%ACTION%, Verbosity=%VERBOSITY%, Config=%CONFIGURATION%, Platform=%PLATFORM%
echo.

REM Common build parameters
set BUILD_PARAMS=/p:Configuration=%CONFIGURATION% /p:Platform="%PLATFORM%" /verbosity:%VERBOSITY%

REM Set MSBuild target based on action
if /I "%ACTION%"=="build" set TARGET=Build
if /I "%ACTION%"=="clean" set TARGET=Clean
if /I "%ACTION%"=="restore" set TARGET=Restore
if /I "%ACTION%"=="rebuild" set TARGET=Rebuild

if "%TARGET%"=="" (
    echo Error: Invalid action '%ACTION%'. Use: build, clean, restore, or rebuild
    exit /b 1
)

echo Executing %ACTION% action...
echo.

set PROJECTS[1]=Demo/Demo.csproj
set PROJECT_NAMES[1]=Demo

set PROJECTS[2]=Test/Test.csproj
set PROJECT_NAMES[2]=Test

set PROJECT_COUNT=2

REM Special handling for rebuild (clean then build)
if /I "%ACTION%"=="rebuild" (
    echo === CLEANING PHASE ===
    for /L %%i in (1,1,%PROJECT_COUNT%) do (
        call :process_project %%i Clean
        if errorlevel 1 goto :error
    )
    echo.
    echo === BUILDING PHASE ===
    set TARGET=Build
)

REM Process all active projects
for /L %%i in (1,1,%PROJECT_COUNT%) do (
    call :process_project %%i %TARGET%
    if errorlevel 1 goto :error
)

echo.
if /I "%ACTION%"=="clean" (
    echo All selected components cleaned successfully!
) else if /I "%ACTION%"=="restore" (
    echo All selected components restored successfully!
) else if /I "%ACTION%"=="rebuild" (
    echo All selected components rebuilt successfully!
) else (
    echo All selected components built successfully!
)
goto :end

:process_project
    setlocal EnableDelayedExpansion
    set idx=%1
    set target=%2

    REM Get project path and name using the index
    for /f "tokens=2 delims==" %%a in ('set PROJECTS[%idx%] 2^>nul') do set PROJECT_PATH=%%a
    for /f "tokens=2 delims==" %%a in ('set PROJECT_NAMES[%idx%] 2^>nul') do set PROJECT_NAME=%%a

    if "!PROJECT_PATH!"=="" goto :eof

    echo ----------------------------------------
    echo [%idx%/%PROJECT_COUNT%] %target%ing !PROJECT_NAME!...

    REM Build the project normally
    msbuild "!PROJECT_PATH!" /t:%target% %BUILD_PARAMS%
    if errorlevel 1 exit /b 1

goto :eof

:error
echo.
echo %ACTION% failed! Check the output above for errors.
exit /b 1

:end
echo %ACTION% completed at %time%
```

to launch applications of course, if it is a pure DOTNET project you would use `dotnet run`


## Troubleshooting {#troubleshooting}


### "Cannot find csharp-ls" or Eglot won't start {#cannot-find-csharp-ls-or-eglot-won-t-start}

1.  Check the PATH: `M-x getenv RET PATH`
2.  Verify the DLL exists at the configured location
3.  Try running manually: `dotnet path\to\CSharpLanguageServer.dll --version`
4.  Check `*eglot-events*` buffer for detailed error messages


### LSP is slow or uses too much memory {#lsp-is-slow-or-uses-too-much-memory}

Try adding to your configuration:

```emacs-lisp
;; Increase garbage collection threshold during LSP operations
(setq gc-cons-threshold 100000000)  ; 100MB
(setq read-process-output-max (* 1024 1024))  ; 1MB
```


### Debugger won't attach {#debugger-won-t-attach}

1.  Ensure the project is built in Debug configuration
2.  Check the DLL path matches your build output
3.  Look at `*dape-repl*` for error messages
4.  Verify netcoredbg runs: `netcoredbg.exe --version`


## Conclusion {#conclusion}

This setup has served me well for my windows .NET 9.0 projects and various other C# work. The key benefits:

-   **Portability**: Everything lives in one directory
-   **Speed**: csharp-ls is notably faster than OmniSharp
-   **Flexibility**: Easy to customise and extend
-   **Offline-capable**: Works in air-gapped environments

The initial setup takes some effort, but once it's done, you have a powerful, consistent development environment that travels with you.
