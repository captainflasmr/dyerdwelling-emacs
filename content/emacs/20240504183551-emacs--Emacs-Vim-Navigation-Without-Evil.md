---
title: "Emacs Vim Navigation Without Evil"
author: ["James Dyer"]
lastmod: 2024-05-11T21:40:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240504183551-emacs--Emacs-Vim-Navigation-Without-Evil.jpg"
---

Every now and then I find it necessary to use Vim! (mainly for work) - was that clickbait on an Emacs blog?! 😀

<!--more-->

---

<div class="ox-hugo-toc toc local">

- [Introduction](#introduction)
- [Modal Editing and Emacs](#modal-editing-and-emacs)
- [My Non-Modal Solution](#my-non-modal-solution)
- [Conclusion](#conclusion)

</div>
<!--endtoc-->

---

```plantuml
!pragma layout smetana
hide footbox
box "Emacs Vim Navigation Without Evil" #lightblue
h -> j
j -> k
k -> l
end box
```


## Introduction {#introduction}

I therefore like to have the muscle memory for basic Vim navigation keybindings already built up as that for me is half the battle when using Vim and I also appreciate the efficiency and natural feel of the navigation keybindings \`hjkl\`.

In light of this I have decided to incorporate this Vim navigation paradigm into Emacs.  My muscle memory is already fully baked and attuned to the Emacs default navigation keys so why not learn something new? with the benefits of an already oven warm familiar feel if I do have to use Vim and maybe anything else that might support vim keybindings (which is not uncommon), for example `pacseek` and seemingly most browser key navigation add-ons.


## Modal Editing and Emacs {#modal-editing-and-emacs}

Of course before delving a little further I shall have to talk about the herd of elephants in the room... and that is the current plethora of Emacs modal editing packages, such as \`evil-mode\`, \`Meow\` et al.  They have been developed to bridge the modal gap, for example, \`evil-mode\` is a comprehensive emulation layer that replicates Vim's keybindings and modes, offering Vim users a more familiar experience within Emacs. On the other hand, \`Meow\` presents a more streamlined approach, designed with simplicity in mind, aiming to provide the efficiency of modal editing without mirroring Vim's functionality in its entirety.


## My Non-Modal Solution {#my-non-modal-solution}

Despite the advantages of these packages, I wanted to find a method that involves neither the full adoptation of \`evil-mode\` nor the simplified modal editing of \`Meow\` or anything similar. My goal here is to utilize Vim's \`hjkl\` navigation keys within Emacs in a non-modal context, thereby retaining Emacs's modeless editing advantages while enjoying the familiarity and comfort of Vim's navigation system.

To achieve this, I tapped into the power of Emacs's keybinding customization capabilities.

Simply, I reassigned the original Emacs functions defined by \`M-hjkl\`.  This allows a Vim-style navigation in Emacs's default mode-less environment using the following keybindings:

```elisp
(bind-key* (kbd "M-h") #'backward-char)
(bind-key* (kbd "M-j") #'next-line)
(bind-key* (kbd "M-k") #'previous-line)
(bind-key* (kbd "M-l") #'forward-char)
```

The original functions bound to these keys were seldom used in my workflow so I didn't mind replacing them:

-   \`M-h\` (\`mark-paragraph\`) found a new home at \`M-s h\`.
-   \`M-j\` (\`default-indent-new-line\`), which I rarely used, was easily replaced by simply using \`tab\` or \`C-i\`.
-   \`M-k\` (\`kill-sentence\`), another feature I never utilized, was made redundant by the use of \`kill-line\`.
-   \`M-l\` (\`downcase-word\`), also rarely used, didn’t find a new binding as it wasn’t needed in my daily tasks.

The only disadvantage is that it requires holding down the Meta (or Alt) key with my left hand, but ergonomically that doesn't seem too bad as that will be the only requirement for my left hand as my right will be busy with the navigation.


## Conclusion {#conclusion}

While \`evil-mode\` and \`Meow\` offer powerful modal editing solutions that closely mimic or simplify Vim's interface within Emacs, my approach demonstrates an alternative path. By creatively reassigning keybindings, I have integrated Vim's efficient navigation into Emacs without adopting a modal editing framework, blending the best of both worlds to enhance my text editing efficiency. This approach underscores the adaptability of Emacs, proving it to be an incredibly versatile tool that can accommodate a wide range of user preferences and workflows.
