---
title: "Generate Current Year tag in an Org Capture Template"
author: ["James Dyer"]
lastmod: 2024-11-01T08:30:00+00:00
tags: ["org-capture", "org", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241007121110-emacs--Org-Capture-Dynamically-Setting-Year.jpg"
---

A crucial aspect of maintaining organized and up-to-date notes is the use of Org Capture templates.

I have currently always hard-coded/set the current year in my org capture templates for use when exporting/constructing my web pages and as a tag for some filtering / web index generation.  Of course the main disadvantage of this is that I have to remember to update the year each year, of which I often fail miserably. ☹️

<!--more-->

{{< figure src="/emacs/20241007121110-emacs--Org-Capture-Dynamically-Setting-Year.jpg" width="100%" >}}

Can I get an org capture template to auto generate just the year as a tag like other capture template attributes?

Well yes, yes you can, and there a couple of ways to achieve this.

Firstly there is the org capture family of format strings which inserts a date when the capture is triggered.  This is a bit much for me and instead of generating just a string for the year, each specifier will generate a full timestamp:

-   **%U** — Inactive Timestamp (Full Date and Time)
-   **%T** — Active Timestamp (Full Date and Time)
-   **%t** — Active Timestamp (Current Date Only)
-   **%u** — Inactive Timestamp (Current Date Only)

However there is another format specifier, which can just insert the year, simply use **`%<%Y>`** which is part of a pretty standard common date or time string format options, for example, here are some useful format specifiers that can be used:

-   \`%Y\`: Year (e.g., \`2023\`)
-   \`%m\`: Month (e.g., \`10\` for October)
-   \`%d\`: Day (e.g., \`05\`)
-   \`%H\`: Hour (24-hour clock)
-   \`%M\`: Minute
-   \`%S\`: Seconds
-   \`%A\`: Full weekday name (e.g., \`Monday\`)
-   \`%B\`: Full month name (e.g., \`October\`)

Well that was easy, so to insert a year tag I can just wrap colons around %&lt;%Y&gt;

Just for fun, lets try something a little more advanced to achieve the same goal but this time allowing much more flexibility.

A particularly useful functional aspect of an org template is that the dynamic generation of the current year can be achieved using some elisp.  Lets use the `(format-time-string)` function.


## Understanding `(format-time-string "%Y")` {#understanding--format-time-string-y}

Before diving into the implementation, let's break down what `(format-time-string "%Y")` does:

-   **`format-time-string`**: This is an Emacs Lisp function that formats the current time and date into a string based on a specified format.

-   **`"%Y"`**: This format code specifically fetches the four-digit representation of the current year. For example, in 2023, it would insert "2023", and conforms to the date specifiers defined above for the more simple capture template format definition.

By embedding this function into my Org Capture templates, the inclusion of the current year is neatly automated, this not only reduces manual updates but also ensures consistency across my entries.


## Implementing Dynamic Year in Capture Templates {#implementing-dynamic-year-in-capture-templates}

Here's an example Org Capture template that uses `(format-time-string "%Y")` to dynamically manage the year for Emacs-related entries:

```elisp
("e" "Emacs" plain
 (file+function
  "~/DCIM/content/emacs--all.org"
  my-capture-top-level)
 "* TODO %^{title} :emacs:%(format-time-string \"%Y\"):"
 :PROPERTIES:
 :EXPORT_FILE_NAME: %<%Y%m%d%H%M%S>-emacs--%\\1
 :EXPORT_HUGO_SECTION: emacs
 :EXPORT_HUGO_LASTMOD: <%<%Y-%m-%d %H:%M>>
 :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail /emacs/%<%Y%m%d%H%M%S>-emacs--%\\1.jpg
 :END:
 %?
 " :prepend t :jump-to-captured t)
```


### Key Features of this Template {#key-features-of-this-template}

-   **Automatic Year Insertion**:
    -   The directive `:%(format-time-string \"%Y\"):`, within the headline, ensures that the current year is automatically included as a tag whenever you create a new entry. This functionality is particularly useful for organizing tasks by year, making it easier to retrieve and filter information based on time.
-   **Enhanced Organization**:
    -   By integrating the current year automatically, you maintain a systematic arrangement of notes and tasks, minimizing the need for manual adjustments every new year.
-   **Ease of Use**:
    -   This template is set up for simplicity and re-usability. Once implemented, it requires no further changes for year-specific adjustments, allowing you to focus on content generation rather than template management.


## Benefits of Dynamic Year Management {#benefits-of-dynamic-year-management}

-   **Time-Saving**: No need to modify the template annually, thus saving valuable time.
-   **Consistent Records**: Automatic year tagging provides a consistent way of tracking entries over time.
-   **Improved Searchability**: Facilitates year-based searches and statistics generation, enhancing the utility of your Org files.
