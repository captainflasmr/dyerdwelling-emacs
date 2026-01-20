---
title: "Building a New Package From Scratch - Cursor Heatmap Pt1"
author: ["James Dyer"]
lastmod: 2025-07-20T13:17:00+01:00
tags: ["heatmap", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/cursor-heatmap-banner_pt1.jpg"
---

---

<div class="ox-hugo-toc toc local">

- [Lets Create a New Package!](#lets-create-a-new-package)
- [Building Our Foundation](#building-our-foundation)
- [Package Naming Conventions](#package-naming-conventions)
- [Testing Our Foundation](#testing-our-foundation)
- [Our Complete Foundation](#our-complete-foundation)
- [What We've Accomplished](#what-we-ve-accomplished)
- [Next Time](#next-time)

</div>
<!--endtoc-->

---


## Lets Create a New Package! {#lets-create-a-new-package}

Given that I now have greater experience in Emacs package creation, I thought I would create a short series of blog posts over the next few weeks in taking an initial idea I have for a new Emacs package and working it up into a full, working version!

The idea here is to hopefully be useful to someone who has an idea for doing something new in Emacs and would like to start working it up into a package. There are resources out there, but I thought I would give a go at creating my own.

As usual with writing a blog and having to explain a concept to someone in an understandable format, I suspect that I'm also actually likely to learn something from this activity.

Right, so what is my idea?

{{< figure src="/emacs/cursor-heatmap-banner_pt1.jpg" width="100%" >}}

---

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

_Have you ever wondered where your cursor/point spends most of its time when you are fiddling around with Emacs?, do you generally split two windows side-by-side and which window do you typically work in?, do you use olivetti or a similar mode to centralise the text, do you use a four window split on a large monitor and where do you spend most of your time?_

_So how about an Emacs package that monitors your cursor/point position and can provide a graphical report showing by some kind of graphical heat map the often used Emacs window locations_

</div>

---

Lets quickly break this concept down into a very high level design (so high it is in fact just a speck!), should we go the full software engineering approach and define requirements?, then start deriving a design, (maybe not!), so lets see what functional aspects we can start with:

-   Tracks cursor movements throughout your Emacs editing session
-   Creates a visual heat map showing movement patterns
-   Provides analytics about your editing behaviour
-   Persists data across sessions so we can have recall and overall data across many days


## Building Our Foundation {#building-our-foundation}

Lets crack on and get something down on paper and begin with the absolute minimum structure and expand from there.


### Step 1: Create the Basic File Structure {#step-1-create-the-basic-file-structure}

Create a new file called `cursor-heatmap.el` with the following contents:

```elisp
;;; cursor-heatmap.el --- Monitor and visualize cursor movement patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, analytics, visualization
;; URL: https://github.com/yourusername/cursor-heatmap

;;; Commentary:

;; This package will monitor cursor movements throughout an Emacs session and
;; generate heatmap reports showing where you move your cursor most frequently.
;;
;; Basic usage (to be implemented):
;;   M-x cursor-heatmap-mode - start tracking
;;   M-x cursor-heatmap-show-report - view results

;;; Code:

;; Package implementation will go here

(provide 'cursor-heatmap)

;;; cursor-heatmap.el ends here
```


### Step 2: Understanding Key Components {#step-2-understanding-key-components}

**Why `lexical-binding: t`?**

This enables modern variable scoping in Emacs Lisp. Without it, all variables have dynamic scope, which can lead to surprising behaviour and bugs. Always use lexical binding for new packages.

**The `provide` statement:**

```elisp
(provide 'cursor-heatmap)
```

This tells Emacs that loading this file provides the `cursor-heatmap` feature. Other packages can then use `(require 'cursor-heatmap)` to load your package.

There are other more advanced ways of loading packages but we shall stick with the basic built-in Emacs package.el for now.

**Version numbering:**

We start with `0.1.0` following a simple versioning mechanism. As we add features, we'll increment appropriately.


### Step 3: Setting Up Customization Groups {#step-3-setting-up-customization-groups}

One of Emacs' greatest strengths is customizability. Let's set up a customization group for our package:

```elisp
;;; Code:

(defgroup cursor-heatmap nil
  "Cursor movement heatmap monitoring and visualization."
  :group 'tools
  :prefix "cursor-heatmap-")
```

This creates a customization group that will appear under the "Tools" category in Emacs' customization interface. Users can access it via `M-x customize-group cursor-heatmap`


### Step 4: Adding Our First Variables {#step-4-adding-our-first-variables}

Let's add some basic configuration variables:

```elisp
(defcustom cursor-heatmap-grid-width 100
  "Width of the heatmap grid (number of columns)."
  :type 'integer
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-grid-height 50
  "Height of the heatmap grid (number of rows)."
  :type 'integer
  :group 'cursor-heatmap)
```

The `defcustom` macro creates variables that users can customize through the Emacs' interface. The `:type` specification helps Emacs validate user input and provide appropriate UI widgets.


## Package Naming Conventions {#package-naming-conventions}

Notice how all our variables start with `cursor-heatmap-` This is a crucial convention in Emacs package development:

-   **Prevents namespace collisions**: With thousands of packages, prefixing prevents conflicts
-   **Makes code searchable**: Users can easily find all functions related to your package
-   **Shows ownership**: Clear indication of what belongs to your package

Common patterns:

-   Variables: `cursor-heatmap-variable-name`
-   Functions: `cursor-heatmap-function-name`
-   Internal functions: `cursor-heatmap--internal-function` (note the double dash)


## Testing Our Foundation {#testing-our-foundation}

Let's make sure our basic structure works. Add this simple function to test:

```elisp
(defun cursor-heatmap-hello ()
  "Test function to verify package structure."
  (interactive)
  (message "Cursor heatmap package loaded successfully! Grid size: %dx%d"
           cursor-heatmap-grid-width cursor-heatmap-grid-height))
```

Now you can:

1.  Load the file: `M-x load-file` and select `cursor-heatmap.el`
2.  Test it: `M-x cursor-heatmap-hello`
3.  Check customization: `M-x customize-group cursor-heatmap`


## Our Complete Foundation {#our-complete-foundation}

Here's our complete foundation file, I think I shall show this at the end of each post so progress can be monitored.

```elisp
;;; cursor-heatmap.el --- Monitor and visualize cursor movement patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, analytics, visualization
;; URL: https://github.com/yourusername/cursor-heatmap

;;; Commentary:

;; This package will monitor cursor movements throughout an Emacs session and
;; generate heatmap reports showing where you move your cursor most frequently.
;;
;; Basic usage (to be implemented):
;;   M-x cursor-heatmap-mode - start tracking
;;   M-x cursor-heatmap-show-report - view results

;;; Code:

(defgroup cursor-heatmap nil
  "Cursor movement heatmap monitoring and visualization."
  :group 'tools
  :prefix "cursor-heatmap-")

(defcustom cursor-heatmap-grid-width 100
  "Width of the heatmap grid (number of columns)."
  :type 'integer
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-grid-height 50
  "Height of the heatmap grid (number of rows)."
  :type 'integer
  :group 'cursor-heatmap)

(defun cursor-heatmap-hello ()
  "Test function to verify package structure."
  (interactive)
  (message "Cursor heatmap package loaded successfully! Grid size: %dx%d"
           cursor-heatmap-grid-width cursor-heatmap-grid-height))

(provide 'cursor-heatmap)
```


## What We've Accomplished {#what-we-ve-accomplished}

In this first post, we've established the foundation of our Emacs package:

-   [X] **Package structure**: Proper header, metadata, and organization
-   [X] **Customization system**: User-configurable options with validation
-   [X] **Naming conventions**: Consistent, collision-free naming
-   [X] **Basic functionality**: A working, loadable package
-   [X] **Testing framework**: Simple way to verify our progress


## Next Time {#next-time}

In the next post, we'll dive into the core functionality: tracking cursor position.

We'll explore:

-   How Emacs represents cursor positions
-   Different coordinate systems (pixels vs. characters)
-   Setting up our first movement detection
-   Handling edge cases and error conditions
