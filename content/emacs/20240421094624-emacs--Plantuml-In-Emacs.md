---
title: "Plantuml-Setup-In-Emacs"
author: ["James Dyer"]
lastmod: 2024-04-21T09:46:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240421094624-emacs--Plantuml-In-Emacs.jpg"
---

## install {#install}

To enable plantuml (local installation) in emacs perform the following:

<!--more-->

-   install openjdk (jdk21-openjdk) - for java &gt; 8.1
-   download plantuml.jar and move to &lt;user-emacs-directory&gt;
-   in Emacs, install the plantuml-mode package


#### GraphViz (optional) {#graphviz--optional}

Depending on diagrams you want to generate, PlantUML might need a working version of GraphViz to be able to generate the following diagrams:

Usecase diagrams
Class diagrams
Object diagrams
Component diagrams
Deployment diagram
State diagrams
Legacy activity diagrams

There is however an alternative by putting in the following pragma at the start of the plantuml syntax  Since version 1.2021.5, you can experimentally use PlantUML without installing Graphviz using Semtana which is an experimental port of GraphViz from C to Java, this may or may not work for you so it is work a try first before you install GraphViz:

!pragma layout smetana


## setup {#setup}

By default for plantuml-mode `plantuml-default-exec-mode` is set to _server_ but I prefer to use the local jar file just downloaded, in this case we need to add the following to the emacs init :

```elisp
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar")))
```

Now adding something like the following running in `plantuml-mode` and C-c C-c will generate a plantuml diagram in an emacs buffer:

@startuml
:plant;
:uml;
@enduml


## org babel {#org-babel}

to enable org babel functionality add the following:

```elisp
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
```

and then insert the following for text inserted into buffer:

```plantuml
:plant;
:uml;
```

or to a file;

```plantuml
:plant;
:uml;
```


## troubleshooting {#troubleshooting}

When running from org bable something like the following comes up :

```bash
: java.io.IOException: Cannot run program "/opt/local/bin/dot": error=2, No such file or directory
```

Then it is likely that GraphViz (dot) needs to be installed.

or if running from \*.plantuml so an SVG is generated and not being able to be loaded by emacs with something like :

```bash
Error parsing SVG image: XML parse error: Error domain 1 code 4 on line 1 column 1 of data: Start tag expected, '<' not found
```
