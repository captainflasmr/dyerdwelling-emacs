---
title: "Using-Linux-Pass"
author: ["James Dyer"]
lastmod: 2023-11-23T15:41:00+00:00
tags: ["password", 2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20231123154118-emacs--Using-Linux-Pass.jpg"
---

Using the **pass** linux CLI

<!--more-->

Install **pass**

```nil
gpg --full-generate-key
```

A directory _home/jdyer_.gnupg contains:

```nil
gpg.conf
```

which contains the generated key for encryption

This can now be used to initialise a password database:

```nil
pass init <gpg key>
```

which creates _home/jdyer_.password-store with .gpg-id containing the creation key.

when creating and encrypting a user database password is entered which can be used to unlock.

add in items with:

```nil
pass insert <item>
```

and input password

An individual &lt;item&gt;.gpg is created in .password-store and can be opened in emacs with the user passsword.

Passwords can be retrieved by either opening in emacs or running the **pass** command, for example:

```nil
pass <item>
```

retrieves the password, if the database is already open then the password is to stdout.

Then this can be used for retrieving for example an openai-key from emacs chatgpt-shell:

```elisp
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
     (lambda ()
       (auth-source-pass-get 'secret "openai-key")))))
```
