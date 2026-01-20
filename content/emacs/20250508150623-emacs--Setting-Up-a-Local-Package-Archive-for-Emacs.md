---
title: "Setting Up a Local Package Archive for Emacs"
author: ["James Dyer"]
lastmod: 2025-05-08T15:06:00+01:00
tags: ["emacs", 2025]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20250508150623-emacs--Setting-Up-a-Local-Package-Archive-for-Emacs.jpg"
---

A local package archive allows you to test packages as if they were coming from an official repository like MELPA, but with your local files. Here's a detailed guide on how to set one up:


## 1. Create the Archive Directory Structure {#1-dot-create-the-archive-directory-structure}

First, create a directory structure for your local archive:

```bash
mkdir -p ~/.emacs.d/my-local-elpa/archive-contents.d
```


## 2. Prepare Your Package {#2-dot-prepare-your-package}

Your package should be organized according to the Emacs package format:

```nil
my-package/
├── my-package.el  # Main file with package headers
├── my-package-utils.el  # Optional additional files
└── ... other files ...
```

Make sure your main `.el` file includes proper package headers:

```elisp
;;; my-package.el --- Description of my package  -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dependency1 "1.0") ...)
;; Keywords: convenience
;; URL: https://github.com/yourusername/my-package

;;; Commentary:
;; Brief description of what your package does.

;;; Code:
...
;;; my-package.el ends here
```


## 3. Create the Package Archive Files {#3-dot-create-the-package-archive-files}


### 3.1. Build your package tarball {#3-dot-1-dot-build-your-package-tarball}

```bash
cd /path/to/my-package
tar -cf ../my-package-0.1.0.tar --exclude=.git --exclude=.gitignore .
gzip ../my-package-0.1.0.tar
mv ../my-package-0.1.0.tar.gz ~/my-local-elpa/
```


### 3.2. Create the package descriptor {#3-dot-2-dot-create-the-package-descriptor}

Create a file in `~/my-local-elpa/archive-contents.d/my-package` with this content:

```elisp
(1 . [my-package
      (0 1 0)  ; Version
      ((emacs "25.1") (dependency1 "1.0")) ; Requirements
      "Description of my package" ; Description
      tar ; Package type
      nil]) ; Extra info
```


### 3.3. Generate the archive-contents file {#3-dot-3-dot-generate-the-archive-contents-file}

Create a simple script to generate the archive-contents file:

```elisp
(require 'package)

(let ((default-directory "~/.emacs.d/my-local-elpa/")
      package-archive-contents)
  (dolist (file (directory-files "archive-contents.d" t "^[^\\.]"))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((pkg-desc (read (current-buffer))))
        (add-to-list 'package-archive-contents pkg-desc))))
  (with-temp-file "archive-contents"
    (pp package-archive-contents (current-buffer))))
```

Save this as `~/.emacs.d/generate-archive.el` and run it with:

```bash
emacs --batch -l ~/generate-archive.el
```


## 4. Configure Emacs to Use Your Local Archive {#4-dot-configure-emacs-to-use-your-local-archive}

Add to your init file:

```elisp
(require 'package)
(add-to-list 'package-archives '("local" . "file:///home/yourusername/my-local-elpa/"))
(package-initialize)
```


## 5. Use with use-package {#5-dot-use-with-use-package}

Now you can use your local package:

```elisp
(use-package my-package
  :ensure t
  :pin local)
```


## Automation Tip {#automation-tip}

For easier testing, you can create a Makefile in your package directory:

```makefile
VERSION=0.1.0
PACKAGE_NAME=my-package
ARCHIVE_DIR=~/my-local-elpa

.PHONY: install

install:
	@echo "Building package..."
	tar -cf ../$(PACKAGE_NAME)-$(VERSION).tar --exclude=.git --exclude=.gitignore .
	gzip ../$(PACKAGE_NAME)-$(VERSION).tar
	mv ../$(PACKAGE_NAME)-$(VERSION).tar.gz $(ARCHIVE_DIR)/
	@echo "Updating archive-contents..."
	emacs --batch -l ~/generate-archive.el
	@echo "Package $(PACKAGE_NAME)-$(VERSION) installed to local archive"
```

With this setup, you can run `make install` to update your local archive whenever you make changes to your package.

Would you like me to explain any specific aspect of this process in more detail?
