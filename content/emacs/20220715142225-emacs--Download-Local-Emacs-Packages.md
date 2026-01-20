---
title: "Download Local Emacs Packages"
author: ["James Dyer"]
lastmod: 2022-07-15T00:00:00+01:00
tags: ["org", "melpa", "emacs", "elpa", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20220715142225-emacs--Download-Local-Emacs-Packages.jpg"
---

Steps to locally download emacs packages for offline installation.

<!--more-->

{{< figure src="/emacs/20220715142225-emacs--Download-Local-Emacs-Packages.jpg" class="emacs-img" >}}

--- TOC

<div class="ox-hugo-toc toc local">

- [Local ELPA MELPA ORG](#local-elpa-melpa-org)
- [Local from Mirror](#local-from-mirror)
- [Extra](#extra)

</div>
<!--endtoc-->

---


## Local ELPA MELPA ORG {#local-elpa-melpa-org}

```shell
cd ~ mkdir -p emacs-pkgs/melpa mkdir -p emacs-pkgs/elpa

# mirror the melpa emacs archive
echo
echo "updating MELPA..."
echo
rsync -avz --delete --progress rsync://melpa.org/packages/ ~/emacs-pkgs/melpa/.

# elpa
echo
echo "updating ELPA..."
echo
rsync -avz --delete --progress elpa.gnu.org::elpa/. ~/emacs-pkgs/elpa

# org (currently no rsync support)
echo
echo "updating ORG..."
echo
cd ~/emacs-pkgs
git clone https://git.savannah.gnu.org/git/emacs/org-mode.git
```

I then copy the emacs-pkgs directory to the offline target machine and change the default package manager archives to point to these packages.

Modify **.emacs** in the following manner commenting out the online package communication:

```elisp
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-archives '(("melpa". "~/emacs-pkgs/melpa")
                         ("org" . "~/emacs-pkgs/elpa")
                         ("elpa" . "~/emacs-pkgs/org-mode/lisp")))
```


## Local from Mirror {#local-from-mirror}

First, you need to clone this repository.

```bash
git clone --depth 1 git@gitlab.com:d12frosted/elpa-mirror.git ~/.elpa-mirror
```

And then setup package-archives in your init.el file.

```elisp
(setq package-archives
      `(("melpa" . ,(concat user-home-directory ".elpa-mirror/melpa/"))
        ("org"   . ,(concat user-home-directory ".elpa-mirror/org/"))
        ("gnu"   . ,(concat user-home-directory ".elpa-mirror/gnu/"))))
```

<https://github.com/d12frosted/elpa-mirror>


## Extra {#extra}


### ada-mode {#ada-mode}

ada-mode.el version 4.0 before AdaCore

```elisp
(add-to-list 'load-path "~/emacs-pkgs/old/ada-mode-4.1")
(require 'ada-mode)
```
