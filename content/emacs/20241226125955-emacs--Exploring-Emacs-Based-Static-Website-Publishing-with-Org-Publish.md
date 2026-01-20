---
title: "Exploring Static Website Publishing with Org Publish"
author: ["James Dyer"]
lastmod: 2024-12-27T10:10:00+00:00
tags: ["emacs", 2024, "org", "org-publish", "elisp"]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20241226125955-emacs--Exploring-Emacs-Based-Static-Website-Publishing-with-Org-Publish.jpg"
---

As someone who enjoys using Emacs for almost everything, I recently began exploring an alternative Emacs-native method for publishing static websites.

Currently, I use [ox-hugo](<https://github.com/kaushalmodi/ox-hugo>) to build a static site from a single Org file, and it works beautifully for exporting content to Hugo-compatible Markdown. However, I wanted to create a backup web generation system entirely within Emacs that doesn't rely on external tools like Hugo and might fit into my overarching **Emacs Enhancements** project concept.

My **Emacs Enhancements** project focuses on replacing significant external packages that I commonly use with a collection of simple Elisp functions and built-in functionality. This approach is ideal for situations where I run Emacs on an air-gapped system or even on Windows, where I have found package management to be somewhat unreliable. You can check out my progress on [GitHub](<https://github.com/captainflasmr/Emacs-enhanced>).

<!--more-->

So, how do I replace Hugo/ox-hugo? Well, I finally got around to experimenting with the powerful `org-publish` system. I say "finally" because I’ve had my eye on this for quite a while, but at no point in the past did I have the time to invest in learning it. Now, during the festive period, I have my laptop and potentially some downtime so maybe now is the time!

This post documents my work in progress journey of setting up an Emacs-based publishing workflow using `org-publish` to export to HTML.


## **Why Org-Publish over ox-hugo?** {#why-org-publish-over-ox-hugo}

While ox-hugo provides extensive functionality to structure and export content seamlessly for Hugo-based static sites, I found these motivations to explore org-publish:

1.  **Native Emacs Solution**: Keeping everything within the Emacs ecosystem.
2.  **Backup**: Ensures I have a non-Hugo-dependent HTML web site generation backup.
3.  **Customizability**: Leverage fine-grained control over publishing directory structures and formats from within Emacs elisp.

That said, this isn't a replacement for ox-hugo, but an experiment to see how efficient a pure Emacs approach can be.


## **Project Setup Overview** {#project-setup-overview}

1.  **Using a Monolithic Org File**: My starting file, `emacs--all.org`, contains all blog posts under separate headings.
2.  **Split the Org File into Multiple Smaller Files**: For compatibility with `org-publish`, I wrote a utility to split the file. This allows each blog post to be processed independently.
3.  **Use `org-publish-project-alist`**: Define a multi-step pipeline for splitting, exporting HTML, and processing images.

Here’s how I tackled each step in detail:


## **Step 1: Splitting the Monolithic Org File** {#step-1-splitting-the-monolithic-org-file}

The first hurdle was enabling `org-publish` to work with my single Org file. Since `org-publish` usually expects individual files to process, I created a custom function, `my-org-publish-split-headings`. This function splits the top-level headings marked as `DONE` into separate files, each prefixed with the date from a property (e.g., `:EXPORT_HUGO_LASTMOD:`).


### Splitting File Logic {#splitting-file-logic}

Hopefully, this defun is commented well enough to be almost self-documenting!

```emacs-lisp
(defun my-org-publish-split-headings (plist filename pub-dir)
  "Split an Org file into separate files, each corresponding to a top-level heading
that is marked as DONE.

Each file name is prefixed with the date in YYYYMMDD format extracted from the
:EXPORT_HUGO_LASTMOD: property. PLIST is the property list for the publishing
process, FILENAME is the input Org file, and PUB-DIR is the publishing directory."
  (with-temp-buffer
    (insert-file-contents filename) ;; Load the content of the current Org file
    (goto-char (point-min))
    (let ((heading-level 1) ;; Level of the top-level heading to split by
          prev-start heading-title sanitized-title output-file lastmod-date)
      ;; Iterate over all top-level headings
      (while (re-search-forward (format "^\\*\\{%d\\} \\(?:\\([[:upper:]]+\\) \\)?\\(.*\\)" heading-level) nil t)
        (let ((todo-keyword (match-string 1)) ;; Extract the TODO keyword (if it exists)
              (heading-title (match-string 2))) ;; Extract the title of the heading
          ;; Process only headings marked as DONE
          (when (and todo-keyword (string-equal todo-keyword "DONE"))
            (setq prev-start (match-beginning 0)) ;; Start of the current heading
            (setq sanitized-title (when heading-title
                                    (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" heading-title))) ;; Sanitize title
            ;; Extract the :EXPORT_HUGO_LASTMOD: property for the current section
            (save-excursion
              (when (re-search-forward ":EXPORT_HUGO_LASTMOD: +\\(<.+>\\)" (save-excursion (re-search-forward "^\\* " nil t) (point)) t)
                (let* ((raw-lastmod (match-string 1)) ;; Extract the timestamp string (e.g., "<2024-12-08 08:37>")
                       (date-elements (when (string-match "<\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" raw-lastmod)
                                        (list (match-string 1 raw-lastmod) ;; Year
                                              (match-string 2 raw-lastmod) ;; Month
                                              (match-string 3 raw-lastmod))))) ;; Day
                  (setq lastmod-date (when date-elements
                                       (apply #'concat date-elements))))))
            ;; Default to "00000000" if no valid lastmod-date is found
            (setq lastmod-date (or lastmod-date "00000000"))
            ;; Find the end of this section (right before the next top-level heading)
            (let ((section-end (save-excursion
                                 (or (re-search-forward (format "^\\*\\{%d\\} " heading-level) nil t)
                                     (point-max))))) ;; End of current section or end of file
              ;; Only proceed if sanitized title exists and is valid
              (when (and sanitized-title (not (string-empty-p sanitized-title)))
                ;; Create the output file name (prepend the date)
                (setq output-file (expand-file-name (format "%s-%s.org" lastmod-date sanitized-title) pub-dir))
                ;; Write the section content (from prev-start to section-end)
                (write-region prev-start section-end output-file)
                (message "Wrote %s" output-file)))))))
    ;; Return nil to indicate successful processing
    nil))
```

I will go into more detail about how I set this up in a future post and how I decided on a split org-file naming convention.


## **Step 2: Org-Publish Configuration** {#step-2-org-publish-configuration}

{{< figure src="/emacs/20241226125955-emacs--Exploring-Emacs-Based-Static-Website-Publishing-with-Org-Publish.jpg" width="100%" >}}

After splitting the file, I defined the publishing pipeline in `org-publish-project-alist`. Here’s what each part does:

1.  **`split-emacs`**:
    -   Runs the custom splitting function (`my-org-publish-split-headings`).
    -   Takes the original monolithic Org file and generates smaller Org files in a designated directory.
2.  **`blog-posts-emacs`**:
    -   Processes these generated Org files and exports them to HTML.
    -   Adds metadata such as preamble, postamble, and custom `.css` links for styling.
    -   Automatically generates a "sitemap" for blog indexing.
3.  **`images-emacs`**:
    -   Publishes related images to the target directory.
4.  **`blog`**:
    -   Meta-project to combine all the phases (`split-emacs`, `blog-posts-emacs`, `images-emacs`).


### `org-publish-project-alist` {#org-publish-project-alist}

```emacs-lisp
(require 'ox-publish)
;;
(setq org-publish-project-alist
      '(("split-emacs"
         :base-directory "~/DCIM/content"
         :base-extension "org"
         :publishing-directory "~/DCIM/content/split/emacs"
         :exclude ".*"
         :include ("emacs--all.org")
         :publishing-function my-org-publish-split-headings
         :recursive nil)
        ("blog-posts-emacs"
         :base-directory "~/DCIM/content/split/emacs"
         :base-extension "org"
         :publishing-directory "~/publish/hugo-emacs/site/static/public_html"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :html-preamble t
         :html-postamble t
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "the DyerDwelling"
         :html-head "<link rel=\"stylesheet\"
                    href=\"../assets/css//bootstrap.css\"
                    type=\"text/css\"/>\n
                    <link rel=\"stylesheet\"
                    href=\"../assets/css//style-ignore.css\"
                    type=\"text/css\"/>"
         :sitemap-function my-sitemap-format
         :sitemap-sort-files alphabetically)
        ("images-emacs"
         :base-directory "~/DCIM/content/emacs"
         :base-extension "jpg\\|gif\\|png"
         :recursive t
         :publishing-directory "~/publish/hugo-emacs/site/static/public_html/emacs"
         :publishing-function org-publish-attachment)
        ("blog" ;; Meta-project to combine phases
         :components ("split-emacs" "images-emacs" "blog-posts-emacs"))))
```


## Conclusion {#conclusion}

Here is the generated site: <https://www.emacs.dyerdwelling.family/public_html/>

This was a simple introduction to my approach to using `org-publish` to provide an alternative web publishing option for my blog, while I'm still refining this workflow, the combination of a monolithic Org file for writing and `org-publish` for exporting HTML is already proving quite effective as a pure Emacs-powered alternative.
