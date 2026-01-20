---
title: "vc-to-push-to-github-using-ssh"
author: ["James Dyer"]
lastmod: 2024-12-17T11:20:00+00:00
tags: ["vc", "github", "emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20241217112011-emacs--vc-to-push-to-github-using-ssh.jpg"
---

Are you getting the following issue when trying to push to github from Emacs in vc-dir mdoe?

```nil
Running "git push"...
ssh_askpass: exec(/usr/lib/ssh/ssh-askpass): No such file or directory
git@github.com: Permission denied (publickey).
fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.
```

Well the ssh-askpass is not installed and doesn't exist in `/usr/lib/ssh/ssh-askpass`

Is there a way to point to a different name in Emacs?, not sure

But perform the following as a current workaround

Install the following:

`openssh-askpass`

Which make available the following:

/usr/bin/qt4-ssh-askpass

Emacs is looking for:

/usr/lib/ssh/ssh-askpass

So why not provide a symbolic link as root!?, seems to work:

```nil
  su -
  cd /usr/lib/ssh
  ln -s /usr/bin/qt4-ssh-askpass ssh-askpass
```

Although still raises the following:

```nil
  Running "git push"...
  ErrorHandler::Throw - warning: QFSFileEngine::open: No file name specified file:  line: 0 function:
  To github.com:captainflasmr/Emacs-enhanced.git
  6735e12..4766e6c  main -> main
```
