---
title: "Integrating CMake with Emacs"
author: ["James Dyer"]
lastmod: 2024-07-28T17:25:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240728172557-emacs--Integrating-CMake-with-Emacs.jpg"
---

Emacs is an incredibly versatile text editor, and with a bit of Elisp magic, you can tightly integrate your build systems into it. In this tutorial, we will illustrate how to use Emacs for seamless integration with **CMake** through custom Elisp functions and transients. So, let's get started!

<!--more-->

**Introducing the Key Functions**

Here's an overview of the custom Elisp functions to handle CMake commands:

```elisp
(defvar cmake-preset
  "build/linux/debug"
  "cmake-preset")

(defun change-directory-and-run (dir command bufname)
  "Change to DIR and run the COMMAND."
  (let ((default-directory dir))
    (async-shell-command command bufname)
    (message "Running command: %s:%s" dir command)))

(defun run-exe-command (dir exe bufname)
  "Run EXE from a specified DIR."
  (message "run-exe-command: %s:%s:%s" dir exe bufname)
  (change-directory-and-run dir exe bufname))

(defun run-cmake-command (command)
  "Run COMMAND from the top level of the project."
  (message command)
  (change-directory-and-run (project-root (project-current t)) command "*cmake*"))

(defun kill-async-buffer (buffer-name)
  "Kill the async buffer with BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer)
      (message "Killed buffer: %s" buffer-name))))

(defun list-cmake-presets ()
  "List available CMake presets using `cmake --list-presets=configure`."
  (let ((output (shell-command-to-string "cmake --list-presets=configure")))
    (delq nil
          (mapcar (lambda (line)
                    (if (string-match "^\\s-+\"\\([^\"]+\\)\"\\s-*$" line)
                        (match-string 1 line)))
                  (split-string output "\n")))))

(defun transient-select-cmake-preset ()
  "Function to select a CMake preset."
  (interactive)
  (let* ((presets (list-cmake-presets))
         (preset (completing-read "Select CMake preset: " presets nil t)))
    (setq cmake-preset preset)
    (message "Selected CMake preset: %s" preset)))
```

**Explanation of Functions**

1.  \`change-directory-and-run\`:
    -   This helper function changes the directory to \`dir\` and runs the specified \`command\` in an asynchronous shell.

2.  \`run-exe-command\`:
    -   Executes an executable \`exe\` from a specific directory \`dir\` and associates the output with \`bufname\`.

3.  \`run-cmake-command\`:
    -   Runs a CMake command from the top-level of the current project directory.

4.  \`kill-async-buffer\`:
    -   Kills the async buffer named \`buffer-name\`.

5.  \`list-cmake-presets\`:
    -   Lists all available CMake presets using the \`cmake --list-presets=configure\` command.

6.  \`transient-select-cmake-preset\`:
    -   Allows the user to select a CMake preset interactively.

**Creating the Transient Interface**

Next, we define our transient interface for various CMake-related tasks:

```elisp
(transient-define-prefix build-transient ()
  "Build and Diagnostic transient commands."
  ["Build"
   ["CMake"
    ("p" "Set Preset" transient-select-cmake-preset)
    ("c" "Configure"
     (lambda () (interactive)
       (run-cmake-command (format "cmake --preset %s" cmake-preset))))
    ("RET" "Build"
     (lambda () (interactive)
       (run-cmake-command (format "cmake --build --preset %s" cmake-preset))))
    ("i" "Install"
     (lambda () (interactive)
       (run-cmake-command (format "cmake --install %s" cmake-preset))))
    ("f" "Refresh"
     (lambda () (interactive)
       (run-cmake-command (format "cmake --preset %s --fresh" cmake-preset))))
    ("x" "Clean"
     (lambda () (interactive)
       (run-cmake-command "rm -rf build")))
    ("s" "List Presets"
     (lambda () (interactive)
       (run-cmake-command "cmake --list-presets=configure")))]
   ["Flymake"
    ("t" "Toggle Flycheck" flymake-mode)
    ("d" "Show Diagnostics" flymake-show-buffer-diagnostics)]
   ["Coding"
    ("j" "Fancy Stuff"
     (lambda () (interactive)
       (call-interactively 'eglot)
       (flymake-mode 1)
       (company-mode 1)))
    ("u" "Undo Fancy Stuff"
     (lambda () (interactive)
       (eglot-shutdown-all)
       (flymake-mode -1)
       (company-mode -1)))
    ("g" "Stop eglot"
     (lambda () (interactive)
       (eglot-shutdown-all)))]
   ["Run"
    ("r" "All"
     (lambda () (interactive)
       (run-exe-command
        (concat (project-root (project-current t))
                "build/windows/debug/bin/Debug")
        "CigiDummyIG.exe" "*Running CigiDummyIG.exe*")
       (run-exe-command
        (concat (project-root (project-current t))
                "build/windows/debug/bin/Debug")
        "CigiMiniHostCSharp.exe" "*Running CigiMiniHostCSharp.exe*")))
    ("1" "CigiDummyIG"
     (lambda () (interactive)
       (run-exe-command
        (concat (project-root (project-current t))
                "build/windows/debug/bin/Debug")
        "CigiDummyIG.exe"
        "*Running CigiDummyIG.exe*")))
    ("2" "CigiMiniHost"
     (lambda () (interactive)
       (run-exe-command
        (concat (project-root (project-current t))
                "build/windows/debug/bin/Debug")
        "CigiMiniHost.exe"
        "*Running CigiMiniHost.exe*")))
    ("3" "CigiMiniHostCSharp"
     (lambda () (interactive)
       (run-exe-command
        (concat (project-root (project-current t))
                "build/windows/debug/bin/Debug")
        "CigiMiniHostCSharp.exe"
        "*Running CigiMiniHostCSharp.exe*")))]
   ["Kill"
    ("5" "CigiDummyIG (k)"
     (lambda () (interactive)
       (kill-async-buffer "*Running CigiDummyIG.exe*")))
    ("6" "CigiMiniHost (k)"
     (lambda () (interactive)
       (kill-async-buffer "*Running CigiMiniHost.exe*")))
    ("7" "CigiMiniHostCSharp (k)"
     (lambda () (interactive)
       (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))
    ("k" "All (k)"
     (lambda () (interactive)
       (kill-async-buffer "*Running CigiDummyIG.exe*")
       (kill-async-buffer "*Running CigiMiniHost.exe*")
       (kill-async-buffer "*Running CigiMiniHostCSharp.exe*")))]])
```

**Key Bindings for Transient Interface**

1.  **CMake Commands**:
    -   Set CMake preset (\`p\`)
    -   Run CMake configure (\`c\`)
    -   Build project (\`RET\`)
    -   Install project (\`i\`)
    -   Refresh build (\`f\`)
    -   Clean build directory (\`x\`)
    -   List available presets (\`s\`)

2.  **Flymake Diagnostics**:
    -   Toggle Flycheck (\`t\`)
    -   Show diagnostics (\`d\`)

3.  **Coding Helpers**:
    -   Engage coding helpers like Eglot and Flymake (\`j\`)
    -   Disable coding helpers (\`u\`)
    -   Stop Eglot (\`g\`)

4.  **Executable Commands**:
    -   Run all predefined executables (\`r\`)
    -   Individual executable commands (\`1\`, \`2\`, \`3\`)

5.  **Kill Async Buffers**:
    -   Kill specific async buffers (\`5\`, \`6\`, \`7\`)
    -   Kill all async buffers at once (\`k\`)

**Setting Up Global Keybinding**

Finally, bind the transient to a global key for easy access:

```elisp
(global-set-key (kbd "M-RET") 'build-transient)
```

**Conclusion**

By integrating these Elisp functions and transients into your Emacs setup, you can significantly streamline your CMake-based development workflow. This custom interface allows quick access to build commands, diagnostic tools, and executable runners, making Emacs a powerful IDE for CMake projects. Happy coding!
