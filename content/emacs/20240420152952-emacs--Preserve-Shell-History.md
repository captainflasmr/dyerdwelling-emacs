---
title: "Preserve-Shell-History"
author: ["James Dyer"]
lastmod: 2024-04-20T15:29:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240420152952-emacs--Preserve-Shell-History.jpg"
---

To preserve shell history in Emacs for sessions running through its built-in shell (generally \`M-x shell\` for a basic shell or \`M-x eshell\` for Emacs' own shell), you will typically want to ensure the shell's command history is saved between Emacs sessions. Here’s how to do it for both \`shell\` and \`eshell\`.

<!--more-->

\### For \`shell\`

\`M-x shell\` launches a standard shell subprocess (such as Bash or Zsh), and to preserve history across sessions, you'd usually rely on the shell's own mechanism for history management. However, Emacs doesn't automatically load or save the shell history for \`shell\` mode by default like it might when running a shell in a terminal emulator outside of Emacs. You can set this up manually by configuring your shell’s history feature to write to a file and then reading from this file when a new shell session starts in Emacs.

\#### Bash Example

If you are using Bash, you can add the following to your \`.bashrc\` to save the history after each command (this ensures even commands from Emacs' \`shell\` get saved):

\`\`\`bash
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
\`\`\`

Then, in your Emacs configuration (typically \`~/.emacs.d/init.el\` for newer setups or \`~/.emacs\` for older configurations), you can add a function to read the history file when you start \`shell\`:

\`\`\`elisp
(add-hook 'shell-mode-hook 'my-read-shell-history)

(defun my-read-shell-history ()
(let ((history-path "~/.bash_history")) ;; Adjust the path as necessary
(when (file-exists-p history-path)
(setq comint-input-ring-file-name history-path)
(comint-read-input-ring t))))
\`\`\`

Make sure to adjust \`"~/.bash_history"\` to the actual path of your shell's history file if different.

\### For \`eshell\`

\`eshell\` stores its history in a different way, as it is a shell written entirely in Emacs Lisp and doesn’t use an external shell program’s history mechanism. To ensure \`eshell\`'s command history is preserved, you can use Emacs' built-in functionality:

\`\`\`elisp
(setq eshell-history-size 10000) ;; Adjust size as needed
(setq eshell-save-history-on-exit t) ;; Enable history saving on exit
(setq eshell-hist-ignoredups t) ;; Ignore duplicates
\`\`\`

You can put this code in your \`.emacs.d/init.el\` or \`.emacs\` configuration file to make sure your \`eshell\` history is maintained across sessions.

By adjusting these settings, you can ensure that your command history is preserved between sessions in both \`shell\` and \`eshell\` modes in Emacs.
