---
title: "Hugo Text Title Tidying"
author: ["James Dyer"]
lastmod: 2022-09-02
tags: ["hugo", 2022]
categories: ["linux", "open-source"]
draft: false
thumbnail: "/linux/linux--hugo-text-title-tidying__linux_hugo.jpg"
---

Currently I am just displaying the title of my posts with no filtering.  Now that I have developed a general format to help with some emacs Deft categorisation I have run into a little bit of a problem.

<!--more-->

{{< figure src="/ox-hugo/linux--hugo-text-title-tidying__linux_hugo.jpg" width="100%" >}}

For example a typical title format is the following:

```text
Linux ---> Hugo_Format_Title[tag@subtag-art]
```

and the hugo rendered html of course displays this on a card list and on a single page.  I would like to remove the `Linux --->` part and the tag part to optimise the space taken up by the title.

I am simply just using:

```text
{{ .Title) }}
```

which gives the full title.

I want to utilise hugo functions to trim off the front up to the first `->` and to remove the tag part including the closing brackets.

**replaceRE** seems perfect for this to search and replace using regular expressions, so lets give this a go:

```text
{{ replaceRE "^.*->|\\[.*\\]" "" .Title }}
```

this gives me:

```text
Hugo_Format_Title
```

the only issue I had was to figure out how to escape the bracket character, usually in a regex it is just one backslash, but unfortunately that gave a parse error.  But this can be solved by a double backslash.

Note the use of the pipe character to remove both the prepend with the tags and the removal takes place by defining a replacement string of empty double quotes.

But there is a final thing left to do which I had completely forgotten about, to really tidy up the title I would like to replace the underscores with spaces.  I can't add in another piped OR command as the replacement string has to be a single space and not deleting the text.

The solution is to create a compound nested statement, as thus:

```text
{{ replaceRE "_" " " (replaceRE "^.*->|\\[.*\\]" "" .Title) }}
```

so the output string of the original substitution can be fed into another occurrence of **replaceRE**

So the final title is

```text
Hugo Format Title
```

This is much neater!
