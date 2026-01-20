---
title: "Org Table From Org Headings using a Babel Block"
author: ["James Dyer"]
lastmod: 2024-11-12T20:20:00+00:00
tags: ["org", "emacs", "elisp", "babel", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241110085851-emacs--Babel-Block-Generating-Org-Table-From-Org-Headings.jpg"
---

In this post, I'll walk you through how I use an Org Babel block to generate a dynamic Org table based on Org headings.

This approach is handy for anyone who wishes to programmatically extract information from an Org file (such as TODO states, tags, and content) and automatically format it as a neatly structured Org table. You can then export this table to various formats --- like HTML, Markdown, or LaTeX --- with built-in Org mode support.

<!--more-->

At work, I'm currently using Emacs and Confluence, so my idea was to figure out how to get an org file into a Confluence post in a structured manner. For the particular task I'm working on, I've decided that I would like to convert an org file into a table. The table format essentially flattens the information presented by org structured text, and putting it as a table in Confluence also has the advantage of column sorting  and filtering.

This seems for the task in hand the most efficient way of representing a certain set of data.  I will have the advantage of always working within a familiar org document at the basic text level, leveraging all those years of Emacs muscle memory and as always forming the markup base for a myriad of export options.

However, I'm going to devise another method of export, lets see if I can find an effectual way to convert an org file into a table that can be efficiently used in Confluence.

I did some research and thought that maybe the org column mode could be somewhat useful. It's tabular, right? Can't I just grab that data and put it somewhere? Well, from what I can gather, it's mainly for more easily representing properties and uses overlays, which are not conducive to easy export.

A dynamic block looks quite interesting and seems to be more for representing some form of dynamic data that is consistently updated and is tied to an org document.  I think a pattern of tabular representation can be generated using this form and I might take a look at this in the future but for now it doesn't quite seem like the incremental learning opportunity I'm looking for.

And another option?, well that is to play around with an ox org back-end?, for example generating an html table and tailoring to my own needs.  This might be a bit too advanced for me at this stage, I think I will stick with the approach I decided on below which is to create an org babel block and generate a table through the output header parameter `:results table` mechanism.

Of course I am familiar with org babel blocks, it is how I generate my Emacs init file through the tangle mechanism, but I didn't quite realise just how powerful it could be.  My idea here is to parse the current org buffer with a babel block and output an org table simply as a string which would be interpreted as an org table in org-mode, that would work right?.  Well yes, yes it did work, I essentially just kept appending row strings as you would see in a typical org table, separated by the pipe character.  The code however was not particularly efficient, I would gather (push) all the relevant org items onto a list and then loop over them to construct the org table string.  As far as I was aware at the time the only babel mechanism for generation was though a form of pushing textual data to stdout which would then appear in the current buffer to be interpreted in whichever way you want according to the mode.

I stumbled on to a better solution however. I had already done the hard work of constructing a list of lists, with each sublist representing a row, it turns out that I can just return this list from the babel block and if I have the babel output header parameters set as `:results table` the list will be interpreted as a table!

Well lets try this out...

Note: the following examples will all be a babel block with the following header parameters defined:

```nil
#+begin_src emacs-lisp :results table :exports both
```

```emacs-lisp
(let ((rows '()))
  (push (list "1: first row" "2: first row") rows)
  rows)
```

```nil
| 1: first row | 2: first row |
```

That is a table with a single row!

Lets expand...

```emacs-lisp
(let ((rows '()))
  (push (list "1: first row" "2: first row") rows)
  (push (list "1: second row" "2: second row") rows)
  rows)
```

```nil
| 1: second row | 2: second row |
| 1: first row  | 2: first row  |
```

So now I have a simple mechanism for adding multiple rows.

Hang on!, the rows are not the order I expected, lets reverse.

```emacs-lisp
(let ((rows '()))
  (push (list "1: first row" "2: first row") rows)
  (push (list "1: second row" "2: second row") rows)
  (reverse rows))
```

```nil
| 1: first row  | 2: first row  |
| 1: second row | 2: second row |
```

What about the table header?, this took a while to figure out, but I think I have it.

```emacs-lisp
(let ((rows '()))
  (push (list "1: first row" "2: first row") rows)
  (push (list "1: second row" "2: second row") rows)
  (setq rows (reverse rows))
  (push 'hline rows)
  rows)
```

```nil
|---------------+---------------|
| 1: first row  | 2: first row  |
| 1: second row | 2: second row |
```

Well that is the header line, but there is no header!!

```emacs-lisp
(let ((rows '())
      (header (list "col1" "col2")))
  (push (list "1: first row" "2: first row") rows)
  (push (list "1: second row" "2: second row") rows)
  (setq rows (reverse rows))
  (push 'hline rows)
  (cons header rows))
```

```nil
| col1          | col2          |
|---------------+---------------|
| 1: first row  | 2: first row  |
| 1: second row | 2: second row |
```

I think I have figured it out now, so the next aspect I need to consider is how to pick up the org elements.  It seems a common approach is to use `org-map-entries` which steps though each headline, seemingly actually within the buffer itself and some helper functions are available which can be used to extract the data.  For example, `org-element-at-point`, `org-outline-level`, `org-get-tags`, e.t.c, I am aware that there is a more formal API type of method where a syntactical tree can be made available through `org-element-parse-buffer` but that maybe is for another time.  Lets move ahead with the org babel, table output, org list construction through org-map-entries implementation.


## The Task - Collecting Headings and Outputting in a Table {#the-task-collecting-headings-and-outputting-in-a-table}

The goal is simple: outline tasks with various headings, nesting levels, TODO states, tags, and content in an Org file, and use that information to generate an Org table using an **Emacs Lisp Org Babel block**. This allows us to extract metadata from headings into a well-structured table with a corresponding header row.

Each Org heading will be converted into a table row in the following format:

-   **Title**: The full heading, including indentation to represent the level hierarchy.
-   **TODO State**: The TODO keyword (like TODO, DONE, or other custom states) associated with the heading.
-   **Tags**: Any Org tags associated with the heading.
-   **Contents**: The content that immediately follows the heading.


## The Code {#the-code}

The following Emacs Lisp code in an Org Babel block which scans the Org file, collects the required information from all headings, and formats it into a table. Let's break it down step-by-step:


### Org Babel Block: Automatic Table Generation {#org-babel-block-automatic-table-generation}

```emacs-lisp
(let ((rows)
      (header (list "Title" "TODO" "Tags" "Contents"))
      (table-rows '())
      (max-level 0))
  (org-map-entries
   (lambda ()
     (let* ((entry (org-element-at-point))
            (heading (org-get-heading t t t t))
            (level (org-outline-level))
            (tags (org-get-tags))
            (todo (org-get-todo-state))
            (contents ""))
       (org-end-of-meta-data nil)
       (when (eq (org-element-type (org-element-at-point)) 'paragraph)
         (let ((start (point)))
           (org-next-visible-heading 1)
           (setq contents (buffer-substring-no-properties start (point)))
           (dolist (pattern '("^#\\+begin.*" "^#\\+end.*" "\n+"))
             (setq contents (replace-regexp-in-string pattern
                                                      (if (string= pattern "\n+") " " "")
                                                      (string-trim contents))))))
       (setq max-level (max max-level level))
       (push (list (concat (make-string (* (1- level) 2) 45) " " heading)
                   (or todo "") (string-join tags ":") (or contents "")) rows))))
  (setq rows (reverse rows))
  (push 'hline rows)
  (cons header rows))
```


## How It Works {#how-it-works}

-   **`org-map-entries`**: This function iterates through all headings in the Org document. Each heading that it encounters produces metadata, such as the heading title, level in the hierarchy, TODO state, tags, and the content following the heading.

-   **Content Sanitization**: For headings with associated content, newline characters and code block markers (like `#+begin_src` and `#+end_src`) are removed. This ensures that the content is condensed into a single line for insertion into the Org table.

-   **`heading`**: This variable stores the full heading, formatted with indentations based on its nesting level (`level`) to visually represent its position in the Org structure.

-   **Output Format**: The information is collected into a list (`rows`). The list contains individual lists representing rows of the table. A "horizontal line" (`'hline`) is inserted to act as a row separator.

-   Finally, the first list in `rows` is a header row (`header`), which includes the column titles: "Title", "TODO", "Tags", and "Contents".

When evaluated (`C-c C-c`) within this org source for the blog post the code block outputs a table like the one below, note that it will also include the example Org Structure defined in the next section.

{{< figure src="/emacs/20241110085851-emacs--Babel-Block-Generating-Org-Table-From-Org-Headings/2024-11-10-13-54-46.jpg" width="100%" >}}


## Example Org Structure {#example-org-structure}

Here's an example Org structure that would produce part of the table above:

```org
** TODO one :tag1:tag2:
*** DOING sub

content 1

#+begin_src
>>
code start
more code >>
#+end_src
after code

**** further into the tree! :subtag:

first line, no space before!

***** even further down the hole! :hole_tag:

No escape!!

** TODO two :tag_two:

content 2

** three
SCHEDULED: <2024-11-10 Sun>
```


## Exporting the Table {#exporting-the-table}

Once this table is generated, it will correctly render within Org mode, respecting the Org table formatting rules. You can also export it to other formats (like HTML or Markdown) using standard Org export commands (such as `C-c C-e h` or `C-c C-e m`).

For example, exporting to **HTML** will produce a structured HTML table with the column headers formatted as table header (`<th>`) elements, and the data rows and horizontal lines correctly converted into formatting tags.

{{< figure src="/emacs/20241110085851-emacs--Babel-Block-Generating-Org-Table-From-Org-Headings/2024-11-10-13-53-37.jpg" width="100%" >}}


## Conclusion {#conclusion}

By leveraging Emacs Lisp and Org Babel, we've created a highly flexible way to extract information from Org headings and output it as a well-structured table. This method not only saves time when working with large or hierarchical documents, but it also provides a powerful way to export Org-based data to various formats based on your needs.

This approach can be extended further to include other metadata (like scheduled dates, deadlines, or custom properties) or more advanced formatting options for both Org and export formats.

**Next Steps**: Experiment with Org Babel to explore even more advanced use cases, for example I would like to format the table in a more flexible manner, for example to specify the width and maybe the cell / row colour based on content.
