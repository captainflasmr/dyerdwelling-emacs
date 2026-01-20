---
title: "Creating a Spelling Transient"
author: ["James Dyer"]
lastmod: 2024-07-12T08:25:00+01:00
tags: ["spelling", "jinx", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240712082430-emacs--Spelling-Powerthesaurus.jpg"
---

I really want to finally get to grips with my spelling / dictionary set-up.

I'm happy with `jinx` instead of `flyspell`. I like `powerthesaurus` and, of course, `dictionary-lookup-definition`. It is mainly the keybindings I want to finally settle on. I have been moving them around for a while now but haven't really established something comfortable.

<!--more-->

jjjNow Emacs 29 comes with `transient`, which is the keyboard-driven interface used by Magit, I am going to see if I can fit my spelling keybindings into a more menu-driven system. I have tried Hydra in the past but found I wasn't really using it. Now that `transient` is built-in and the syntax seems simple, let's give it a go!

Here are my original keybindings:

```elisp
(use-package jinx)
(use-package powerthesaurus)
(global-set-key (kbd "M-s y") 'powerthesaurus-lookup-synonyms-dwim)
(global-set-key (kbd "M-s a") 'powerthesaurus-lookup-antonyms-dwim)
(global-set-key (kbd "M-s x") 'jinx-mode)
(global-set-key (kbd "M-s c") 'jinx-correct)
(global-set-key (kbd "M-s d") 'dictionary-lookup-definition)
```

and converted into :

```elisp
(use-package jinx)

(use-package powerthesaurus
  :init
  (require 'transient)
  (transient-define-prefix my/transient-spelling ()
    "Spelling commands"
    ["Spelling"
     ["Lookups"
      ("y" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
      ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)]
     ["Spelling Tools"
      ("x" "Jinx" jinx-mode)
      ("c" "Jinx correct" jinx-correct)]
     ["Dictionary"
      ("d" "Lookup" dictionary-lookup-definition)]
     ["Miscellaneous"
      ("q" "Quit" transient-quit-one)]])
  :bind
  ("C-c s" . my/transient-spelling))
```

and produces the following menu:

{{< figure src="/emacs/20240712082430-emacs--Spelling-Powerthesaurus.jpg" width="100%" >}}

Well that was pretty simple, lets see how this goes and if I might then think about translating some more of my keybindings to transient menus.
