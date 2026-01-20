---
title: "Defining Categories in Org Files for Hugo"
author: ["James Dyer"]
lastmod: 2023-07-21T13:02:00+01:00
tags: ["markdown", "hugo", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230513130655-emacs--Hugo-Adding-Categories.jpg"
---

I use Hugo to generate my web site and I made a decision early on to use `ox-hugo` withing emacs and to manage a single large `org` file with each subheading a blog post and each subheading tag representing... well, tags! I was aware of the concept of defining categories but decided to sort that out at a later date until I really understood what I was doing - this is a common thing that I do.

<!--more-->

{{< figure src="/emacs/20230513130655-emacs--Hugo-Adding-Categories.jpg" class="emacs-img" >}}

Well now I think I understand what I'm doing and now I want to create categories for my web site and as it turns out managing my posts within org files means that this can be achieved very easily.

For example, I have the following set up in my org file:

```nil
* Emacs [0/0] :emacs:linux:
** DONE Initial focus in Occur Buffer :occur:elisp:2023:
** DONE Cursor Blinking Rate :2023:
```

This means that the subheadings / posts under the top level `Emacs` heading inherit the `emacs` and `linux` tags and then define any extra specific to each post as desired.  As these tags are defined at the top level then you could almost say they are defining a more broad definition, lets say a category!

So how do I change these top level tags to categories?, well I define the following:

```nil
* Emacs [0/0] :@emacs:@linux:
```

It is just a very slight change to the org tag definition after which I just need to re-export all my blog posts / subheadings using the `ox-hugo` dispatcher.

The generated markdown files now contain the following front matter which Hugo can now process accordingly:

```text
tags = ["occur", "elisp", "2023"]
categories = ["emacs", "linux"]
```

Simple! and in fact you can see an example of how the categories are incorporated in this very web site!
