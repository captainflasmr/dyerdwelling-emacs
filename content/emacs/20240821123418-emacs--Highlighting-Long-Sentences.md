---
title: "Highlighting-Long-Sentences"
author: ["James Dyer"]
lastmod: 2024-08-21T12:34:00+01:00
tags: ["emacs", "elisp", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240821123418-emacs--Highlighting-Long-Sentences.jpg"
---

In Emacs, you can use a combination of packages and configurations to help control the length of your sentences. Here are a few approaches:

<!--more-->


## 1. **Flyspell Mode with Custom Configuration:** {#1-dot-flyspell-mode-with-custom-configuration}

-   While Flyspell is primarily used for spell checking, you can combine it with custom functions to highlight long sentences.
-   You could write a custom function that checks the length of sentences and highlights them if they exceed a certain limit.

Below is an example of how you can enhance `flyspell-mode`

with a custom function that checks the length of sentences in Emacs.  This function will highlight sentences that exceed a specified length.


### 1. **Define the Custom Function** {#1-dot-define-the-custom-function}

First, let's define a function that checks the length of each sentence and highlights it if it exceeds a certain limit:

```elisp
(defun highlight-long-sentences (limit)
  "Highlight sentences that are longer than LIMIT characters."
  (interactive "nSentence length limit: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[.!?]" nil t)
      (let ((sentence-end (point)))
        (backward-sentence)
        (let ((sentence-start (point)))
          (when (> (- sentence-end sentence-start) limit)
            (highlight-region sentence-start sentence-end 'hi-yellow)))))))

(defun highlight-region (start end face)
  "Highlight region from START to END with FACE."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)))
```


### 2. **Add Hook to Flyspell Mode** {#2-dot-add-hook-to-flyspell-mode}

You can automatically run this function when `flyspell-mode` is enabled by adding it to the `flyspell-mode-hook`.

```elisp
(defun flyspell-check-long-sentences ()
  "Check and highlight long sentences in the buffer."
  (highlight-long-sentences 100)) ;; You can adjust the 100 to your preferred sentence length limit.

(add-hook 'flyspell-mode-hook 'flyspell-check-long-sentences)
```


### 3. **Enable the Highlighting in Your Buffer** {#3-dot-enable-the-highlighting-in-your-buffer}

Now, whenever you enable `flyspell-mode`, the function will check the entire buffer for sentences longer than 100 characters (or whatever limit you set) and highlight them with a yellow background.


### 4. **Additional Customization** {#4-dot-additional-customization}

-   You can customize the length limit by changing the number in the `highlight-long-sentences` call.
-   You can also change the highlight color by using a different face, like `hi-pink` or `hi-green`, or define your own custom face.


### 5. **Usage Example** {#5-dot-usage-example}

To use this enhancement:

1.  Add the code above to your Emacs configuration (e.g., `.emacs` or `init.el`).
2.  Open a text buffer and enable `flyspell-mode` with `M-x flyspell-mode`.
3.  The sentences longer than the specified limit will be highlighted in yellow.

This setup gives you a simple way to visually monitor and control the
length of your sentences while writing in Emacs.


## 2. **LanguageTool Integration:** {#2-dot-languagetool-integration}

-   [LanguageTool](https://languagetool.org/) is a grammar checker that can be integrated into Emacs. It can be used to check for long sentences along with other grammar issues.
-   You can install the `langtool` package from MELPA and configure it to use LanguageTool.

Example setup:

```elisp
(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar "/path/to/languagetool-commandline.jar")
  (setq langtool-default-language "en"))
```


## 3. **Flycheck with Proselint:** {#3-dot-flycheck-with-proselint}

-   [Proselint](https://github.com/amperser/proselint) is a linter that focuses on prose style issues, including sentence length. You can integrate Proselint with Emacs using Flycheck.

<!--listend-->

```elisp
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package flycheck-proselint
  :ensure t
  :after flycheck
  :config
  (add-to-list 'flycheck-checkers 'proselint))
```

-   Proselint will underline or otherwise highlight issues, including overly long sentences, according to its style rules.


## 4. **Syntactic Complexity Check:** {#4-dot-syntactic-complexity-check}

-   The `syntactic-sugar` package can help by highlighting sentences that might be too complex or convoluted. It's not a direct sentence length checker, but it can help with sentence readability, which often correlates with length.
    ```elisp
    (use-package syntactic-sugar
    :ensure t)
    ```


## 5. **WriteGood Mode:** {#5-dot-writegood-mode}

-   The `writegood-mode` is another package designed to highlight poor writing, including long sentences and complex phrases. It's lightweight and easy to use.
    ```elisp
    (use-package writegood-mode
      :ensure t
      :config
      (add-hook 'text-mode-hook 'writegood-mode))
    ```


## Combining the Tools: {#combining-the-tools}

For the best results, you might want to combine some of these tools. For instance, using `writegood-mode` for general writing quality, `flycheck #+end_src with =proselint` for linting, and `langtool` for grammar checking can provide a comprehensive writing environment.

Each tool has its strengths, so you can choose the combination that best fits your writing needs.
