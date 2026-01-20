---
title: "Using ripgrep within Projects"
author: ["James Dyer"]
lastmod: 2023-01-24T21:07:00+00:00
tags: ["ripgrep", "projects", "emacs", "deadgrep", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230124195440-emacs--My-Tentative-Steps-to-Working-With-Projects.jpg"
---

Given my recent forays into the world of grepping in emacs using `deadgrep` (and hence ripgrep) and my use of `find-file-rg` which feeds into my current completion system of **ivy** I think the next step is to try to set up a project and to see if I can gain any advantages in my workflow.

<!--more-->

{{< figure src="/emacs/20230124195440-emacs--My-Tentative-Steps-to-Working-With-Projects.jpg" >}}

I am not yet going to dive head first into `projectile` but dangle a tentative pinky into the deep project pools of the built in project system, namely EDE (Emacs Development Environment).

As I have mentioned before I typically would only be working within a single directory hierarchy, hence my previous `deadgrep` enhancement of using a defined `setq home-dir` variable, but I think it might be an enhancement if I could leverage `deadgreps` use of projects; for example, wandering around the deadgrep code I found the following:

```elisp
(defun deadgrep--project-root ()
  "Guess the project root of the given FILE-PATH."
  (let ((root default-directory)
        (project (project-current)))
    (when project
      (cond ((fboundp 'project-root)
             ;; This function was defined in Emacs 28.
             (setq root (project-root project)))
            (t
             ;; Older Emacsen.
             (-when-let (roots (project-roots project))
               (setq root (car roots))))))
    (when root
      (deadgrep--lookup-override root))))
```

as far as I can tell the default-directory is the current directory of the buffer which can be displayed by `M-x pwd` but it is the (project-current) that I am more interested in.

A quick investigation and it seems I need to turn on **global-ede-mode** and then run **ede-new** in the top level directory of my prospective project.  This results in the creation of a Project.ede file containing some project information allowing the setting up of a collection of source files and instructions on how to build them.  For the moment I'm not too interested in building anything, but more on how to trigger `deadgreps` project logic by enabling this directory hierarchy as a project.

As part of creating this project file it seems **ede-project-directories** has also been added to my init file and an extra menu has appeared on the menu bar called **Development**

Is this all I need?, well lets evaluate the following in the top level folder:

```elisp
(project-current)
```

I seem to get a long list of project information, including the project top level directory.  Moving down the directory hierarchy and reevaluating the same expression gives me the same directory!

So I am assuming that in **my/deadgrep** bespoke wrapper, if I now replace:

```elisp
(deadgrep search-term home-dir)
```

with:

```elisp
(deadgrep search-term)
```

if I create a project in my former **home-dir** directory location then my former functionality will have been preserved but with the added flexibility of being able to define other project locations, and also apparently subdirectories!  this is actually pretty neat :)

Now I have defined a rudimentary project and seems to work well with `deadgrep` will it work well with my other favourite `ripgrep` wrapper, namely `find-file-rg`?  Well there does seem to be some code to accommodate projects too, namely:

```elisp
(let* ((dir (if current-prefix-arg
                (find-file-rg--read-dir)
              (or (let ((project (project-current)))
                    (when project
                      (if (fboundp 'project-root)
                          (project-root project)
                        (cdr project))))
```

and yes, it works!, so anywhere I am within the hierarchy of a project I can call up a list of files fed from **ripgrep --files** into **ivy** and complete as normal.

As a bonus `find-file-rg` allows the passing of a universal argument and a **current-prefix-arg** check enables the navigation to any directory.

In summary then, I can now define a project, I can grep throughout the project from any file / directory within that project and I can pull up a list of files within the project all leveraging the power of **ripgrep** through `deadgrep` and `find-file-rg`!
