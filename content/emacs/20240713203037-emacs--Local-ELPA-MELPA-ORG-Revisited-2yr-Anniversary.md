---
title: "Emacs Blog 2 Year Anniversary - First Post Revisit - Create Local Offline ELPA MELPA ORG"
author: ["James Dyer"]
lastmod: 2024-07-15T21:00:00+01:00
tags: ["emacs", "bash", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240713203037-emacs--Local-ELPA-MELPA-ORG-Revisited-2yr-Anniversary.jpg"
---

I've noticed that my Emacs blog is now almost 2 years old!

<!--more-->

{{< figure src="/emacs/20240713203037-emacs--Local-ELPA-MELPA-ORG-Revisited-2yr-Anniversary.jpg" width="100%" >}}

Is there something absurd about writing about a text editor in that very text editor for such a long period? - I think the answer to this is clearly..., no!

My first post was on 2022-07-15 and this will be blog number 97.

And yes before you ask, how do I know this to be true?, well I found out the Emacs way!

I recorded its inception date in org-agenda, so I saw this date creep up this week :

```org
** blog anni :james:web:
<2022-07-15 .+1y>
```

The date of my first post is in an org drawer in my single Emacs blog org file:

```org
:EXPORT_HUGO_LASTMOD: 2022-07-15
```

and the blog number?, well, I org-copy-visible'd my org blog file of all the top level org headers, pasted into the **scratch** buffer, flush-line'd out all the TODO entries, then finally entered (display-line-numbers-mode) to get the number of lines (I suspect there might be an easier way to do this 😀)

I also wrote this post ahead of time and to remind me when to post I created an org schedule entry as follows:

```org
** TODO post 2 yr emacs blog post
SCHEDULED: <2024-07-15 Mon>
```

and appeared in the agenda as :

```org
Monday     15 July 2024 W29
aab--calendar:Scheduled:  TODO post 2 yr emacs blog post
```

and in (cfw:open-org-calendar) as :

```org
+-------------------------+
| 15 (3)                  |
| TODO post 2 yr emacs    |
| blog anni               |
|                         |
|                         |
|                         |
+-------------------------+
```

Living an Emacs life and especially partially through org means that any and all information can be easily extracted let alone initially logged, the basic text format is so simple and uniform that all the data items above were pretty much extracted using the same mechanism through Emacs.

Anyway enough of all that, in celebration of being able to maintain some level of blogging discipline for 2 years, I decided to revisit my very first post, which was on how to create an offline version of the major Emacs repositories, namely:

-   ELPA
-   MELPA
-   ORG

As it turns out this post is still quite relevant as from time to time, I need an offline version for an Emacs setup without an internet connection.

So lets re-post!, just for fun!, including the bash script that I still use:

---

> Steps to locally download emacs packages for offline installation.


## Local ELPA MELPA ORG {#local-elpa-melpa-org}

```bash
#! /bin/bash
cd ~
mkdir -p emacs-pkgs/melpa
mkdir -p emacs-pkgs/elpa

echo
echo "updating MELPA..."
echo
rsync -avz --delete --progress rsync://melpa.org/packages/ ~/emacs-pkgs/melpa/.

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
# wget -r -l1 -nc -np https://orgmode.org/elpa
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
