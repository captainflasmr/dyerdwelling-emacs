---
title: "Expanding Text using abbrev and skeletons"
author: ["James Dyer"]
lastmod: 2022-11-23T00:00:00+00:00
tags: ["skel", "emacs", "abbrev", 2022]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20221123131342-emacs--Expanding-Text-using-abbrev.jpg"
---

My next investigation into trying to improve my emacs workflow is expanding entered text for repetitive tasks.

<!--more-->

{{< figure src="/emacs/20221123131342-emacs--Expanding-Text-using-abbrev.jpg" class="emacs-img" >}}

I'm not yet sure how useful this will be for me as I typically copy and paste my way through the creation of text files, especially org files.  Maybe org-capture could be a help?

As always I want to try built in options to augment my vanilla emacs setup, so the obvious place to look is abbrev.

This facility can be geared towards fixing common spelling _misteaks_ (ha!, see what I did there!)  or expanding acronyms.  I don't really want to be using that aspect however as I read somewhere that you can link abbrevs to skels, my aim is to type a few unique characters which would autofill a common textual structure, for example PROPERTIES in an org file.

What about yasnippet! I all hear you vehemently proclaim!, but of course this is not built in, so no I shall not be considering this option for now.

I am not even familiar with defining a skeleton (skel), so what on earth am I thinking about jumping in like this for potentially a negligible return, well of course the answer is twofold:

1.  it is fun, this is of course emacs we are talking about.
2.  in the long run it will improve my productivity.

The usual way I approach absorbing something new is to first read around all the options and then to start simple.  By simple I mean setting up some uncomplicated skeletons and elementary abbrevs to initially test my setup.  Then after a while adjusting to this new way of working to gradually start learning the more complex and subtle aspects of the functionality.

Firstly lets try and set up a simple properties skeleton, namely :

```elisp
(define-skeleton org-hugo-properties-skel
  "org properties skeleton."
  "\n"
  ":PROPERTIES:\n"
  ":EXPORT_FILE_NAME:\n"
  ":EXPORT_HUGO_LASTMOD: " (format-time-string "%Y-%m-%d") "\n"
  ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail\n"
  ":END:\n")
```

This is a typical setup for inserting a Hugo article into an org file ready for export, in fact this article is written using this framework.

I can call this by defining keybinding such as :

```elisp
(global-set-key (kbd "C-c i p") 'org-hugo-properties-skel)
```

but I suspect my tiny brain is currently full to capacity of emacs keybindings and the ones that are in my head need a little more time to settle in before I commit to any more.

My overall idea is to define a family of common characters that I can insert for expansion as in this case I am talking about inserting a predefined set of text rather than some emacs function, so why commit to another keybinding?

So lets set up **abbrev** for this task, the first thing to do is to set up an **abbrev_defs** file for my abbreviation definitions in my emacs init file as thus:

```elisp
(setq abbrev-file-name (concat home-dir "/content/abbrev_defs"))
(setq-default abbrev-mode t)
```

In this file I can define an elisp definition, by either linking a sequence of characters to a string or a skel, so I define thus :

```elisp
(define-abbrev-table 'global-abbrev-table
  '(
    ("hugprop" "" org-hugo-properties-skel)
    ))
```

There are commands to insert, edit and perform various different operations on this file but really all I want for now is a simple text file that I edit in emacs to add in any new definitions.

So typing **hugprop** &lt;space&gt; gives me:

```elisp
:PROPERTIES:
:EXPORT_FILE_NAME:
:EXPORT_HUGO_LASTMOD: <2022-12-11>
:EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail
:END:
#+hugo: more
```

In the **define-abbrev-table** I can also define a mapping to a simple string, for example:

```elisp
(define-abbrev-table 'global-abbrev-table
  '(
    ("hugprop" "" org-hugo-properties-skel)
    ("btw" "by the way" nil)
    ))
```

So now I have figured this out I am ready to go!

After a little while of using this method I was starting to struggle with updating the abbrev_defs and seeing its effects immediately in emacs.  When I update to a new abbrev it means that I pretty much want to use it now!

I couldn't really figure out an easy method of instantly making the newly added abbrev command available (except restarting emacs of course).  There will be a way but it wasn't quickly apparent to me and I suspect it was related to the fact that I am not adding to the abbrev_defs using an emacs function.

Before my investigations consumed too much time however I thought I would try something!.

I am not a great fan of configuration files cluttering up my system and certainly with emacs configuration I really am aiming with my vanilla-ish emacs setup to keep everything in one init file and as concise as possible.  So why can't I move the **define-abbrev-table** straight into my **.emacs** file?  and just reevaluate an updated abbrev table?

Well the answer is that I can!

Problem solved.  So if I feel the need to add more skeletons they can now be defined next to the abbreviation table and I can update immediately with a reevaluation in my emacs init file.

Next on my expanding text journey I think might take a look at **dabbrev** and **hippie-expand**
