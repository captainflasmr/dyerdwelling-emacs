---
title: "Guide-to-Creating-New-Emacs-Package-for-MELPA"
author: ["James Dyer"]
lastmod: 2024-01-13T16:01:00+00:00
tags: ["package", "melpa", "magit", "emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240113160113-emacs--MELPA-Submitting.jpg"
---

The instructions below are a quick guide to creating a new Emacs package and preparing it to be successfully submitted to the MELPA repository.

<!--more-->

---

<div class="ox-hugo-toc toc local">

- [Firstly create the new package locally](#firstly-create-the-new-package-locally)
- [Test package](#test-package)
- [Check the package just running in vanilla emacs](#check-the-package-just-running-in-vanilla-emacs)
- [Ensure package follows the relevant coding conventions:](#ensure-package-follows-the-relevant-coding-conventions)
- [Alternatives](#alternatives)
- [Tag package](#tag-package)
- [Fork MELPA Repository](#fork-melpa-repository)
- [Clone Your Fork Locally](#clone-your-fork-locally)
- [Create a new branch and add MELPA Recipe](#create-a-new-branch-and-add-melpa-recipe)
- [Package Sanity check](#package-sanity-check)
- [Sanity](#sanity)
- [Commit and Push Your Changes](#commit-and-push-your-changes)
- [ERROR:](#error)
- [Create a Pull Request](#create-a-pull-request)
- [Address Feedback](#address-feedback)
- [Merge](#merge)

</div>
<!--endtoc-->

---


## Firstly create the new package locally {#firstly-create-the-new-package-locally}

-   Create a git local repo and link it to github remote repo and develop the new package.


## Test package {#test-package}

Comment / uncomment load-path / vc to check package in emacs locally / remotely, remotely when changes have been committed to git repo which will form a good idea if the package can be successfully downloaded built and integrated into emacs.

```elisp
(use-package selected-window-accent-mode
  :load-path "~/repos/selected-window-accent-mode"
  ;; :vc (:fetcher github :repo "captainflasmr/selected-window-accent-mode")
  :custom
  (selected-window-accent-fringe-thickness 10)
  (selected-window-accent-custom-color my/accent-color)
  (selected-window-accent-mode-style 'subtle))
```


## Check the package just running in vanilla emacs {#check-the-package-just-running-in-vanilla-emacs}


## Ensure package follows the relevant coding conventions: {#ensure-package-follows-the-relevant-coding-conventions}

MELPA will generally require a package to follow the following conventions:


### Use quality-checking tools {#use-quality-checking-tools}


#### byte-compile / flycheck {#byte-compile-flycheck}

When specifying package dependencies in an Emacs Lisp package, including a version number like \`((visual-fill-column "2.23"))\` indicates that your package requires at least that version of the dependency to work correctly. When your package is installed via Emacs' package manager, it will attempt to install a version of \`visual-fill-column\` that satisfies this minimum version requirement.

Since \`visual-fill-column\` is distributed on MELPA, the version number can be a bit misleading, because MELPA uses a date-based versioning system for packages created from each commit. Therefore, the actual version number \`visual-fill-column\` is assigned when built by MELPA will look something like \`20230320.1234\`, which represents the date and time of the package build.

To find out which version MELPA associates with a package, you can search for the package on the MELPA website or query MELPA from within Emacs using \`M-x package-refresh-contents\` followed by \`M-x package-list-packages\`.

In your package's header, if you're not sure about the exact version to depend upon, you may use \`"0"\` or omit the version number entirely. For example:

```elisp
;; Package-Requires: ((emacs "25.1") (visual-fill-column "0"))
```

or

```elisp
;; Package-Requires: ((emacs "25.1") (visual-fill-column))
```

If you use \`"0"\`, the package manager interprets it as no specific version requirement, and it will install the latest version available. If additional features from a specific version of \`visual-fill-column\` are needed, it might be necessary to inspect release notes or specific commits in the \`visual-fill-column\` source repository to find out the correct minimum version number to require.


#### checkdoc {#checkdoc}

Use `checkdoc` to make sure that your package follows the conventions for documentation strings, within reason.

Buffer comments and tags:  Ok
Documentation style:       Ok
Message/Query text style:  Ok
Unwanted Spaces:           Ok


#### `package-lint` to help you identify common errors in your package metadata. {#package-lint-to-help-you-identify-common-errors-in-your-package-metadata-dot}


#### font-lock elisp warning {#font-lock-elisp-warning}

Set the following custom-set-faces :

```elisp
'(font-lock-warning-face ((t (:foreground "#ff0000" :inverse-video t))))
```

so that any slightly frowned upon malformed elisp is clearly highlighted.


#### melpazoid {#melpazoid}

This is an overall check and will combine probably the above and is not essential to get things added to MELPA but run anyway

However can pick up some extra minor things.  All you need to do is to download the package and evaluate from :

```sh
https://github.com/riscy/melpazoid/blob/master/melpazoid/melpazoid.el
```

Note that you may also need to install pkg-info


## Alternatives {#alternatives}

Ensure that alternatives to your emacs package have been investigated.


## Tag package {#tag-package}

When everything is committed and has been checked then create a tag


### vc-mode {#vc-mode}

First make sure you are on main/master

run `vc-create-tag`, I think there is a better way using [b] [c] and soemthing else but I can't remember!

Enter something like 0.1.0

list tags by dropping into a terminal and running

```sh
git tag --list
```

Make sure you are switched to the tag, although should be anyway, but use [b] [s]

To push you will at the moment definitely need to be in the terminal so run :

```sh
git push origin 0.1.0
```


### magit {#magit}

magit -&gt; t -&gt; Create t (tag) -&gt; 0.4.0 -&gt; main

now commit and push tag

magit -&gt; P -&gt; T (a tag) which will formally take a snapshot of the source code at that time and I am assuming will form the basis for the MELPA build mechanism.

Note : github will create a tarball of the tags contents but the new tag will be available to inspect.


## Fork MELPA Repository {#fork-melpa-repository}

Go to the MELPA GitHub repository and fork it to your own user account.

If we are applying feedback after a period or the fork is just out-of-date then on github sync the fork to the MELPA repository.


## Clone Your Fork Locally {#clone-your-fork-locally}

Clone the forked MELPA repository to your local machine, navigate to the required top level directory:


### vc-mode {#vc-mode}

drop into a terminal and run

```sh
git clone git@github.com:captainflasmr/melpa.git
```


### magit {#magit}

```sh
M-x magit-clone
[u]rl or name
git@github.com:captainflasmr/melpa.git
```


## Create a new branch and add MELPA Recipe {#create-a-new-branch-and-add-melpa-recipe}

Using `magit` create a new branch and checkout to hold your changes:


### vc-mode {#vc-mode}

```nil
M-x vc-dir
b c <package-name> RET
```


### magit {#magit}

```nil
M-x magit-status
b c <package-name> RET
```


### notes {#notes}

Add your recipe file to the \`recipes\` directory in the MELPA repository. The recipe file should be named after your package and should contain the s-expression you created:

First open any recipe item in melpa/recipes enable `Melpa-Recipe` major mode this will activate the functions and variables below.

A nice built-in function is `package-build-create-recipe` which will prompt for the requisite information and then put the recipe into the directory pointed to by `package-build-recipes-dir` which will typically be the melpa/recipes directory.    The advantage of running this routine is that it will automatically create a valid recipe and then compile and install the package in the system as if it had come from a real online MELPA.

By default elisp files don't seem to be listed (`:files`) and would automatically pick up \*.el e.t.c according to its documentation, just the \*.el will be fine.

To manually build and install you can also run `C-c C-c (M-x package-build-current-recipe)` while in the recipe file buffer which will offer a `package-build-archive-entry` package descriptor to check everything is set up correctly.

You can now even run `list-packages` and check that your package is now present!

---

A recipe for MELPA is a small s-expression that tells MELPA how to fetch and build a package. Here's a simple recipe for \`selected-window-accent-mode\` based on the information you have provided:

```elisp
(selected-window-accent-mode
 :fetcher github
 :repo "captainflasmr/selected-window-accent-mode")
```

This tells MELPA to:

-   Use GitHub as the source (\`:fetcher github\`)
-   Clone the repository \`captainflasmr/selected-window-accent-mode\`


## Package Sanity check {#package-sanity-check}

Now the package is part of the package list, maybe now test using `use-package` and that everything works as expected.


## Sanity {#sanity}


### vc-mode {#vc-mode}

Check there is a single unregistered/untracked file, namely our new recipe and in our new branch


### magit {#magit}

Run `magit-status` and there will be a single untracked file, namely our new recipe and in our new branch


## Commit and Push Your Changes {#commit-and-push-your-changes}

Commit your new recipe file and push the branch to your GitHub fork


### vc-mode {#vc-mode}

Register the file using [i] and add a comment [v], something like

> Add recipe for &lt;package-name&gt;

Now push the changes using [P] to the main git repository.

Running "git push"...
fatal: The current branch ollama-buddy has no upstream branch.
To push the current branch and set the remote as upstream, use

git push --set-upstream origin ollama-buddy

To have this happen automatically for branches without a tracking
upstream, see 'push.autoSetupRemote' in 'git help config'.


### magit {#magit}

From `magit` stage and commit as normal and put in a comment, something like:

> Add recipe for &lt;package-name&gt;

Now push the changes for the branch to the remote git repository


## ERROR: {#error}

If you get the errors below:

```nil
Running git push -v origin refs/heads/add-selected-window-accent-mode:refs/heads/add-selected-window-accent-mode
Authentication failed for 'https://github.com/captainflasmr/melpa/' ... [Hit $ to see buffer magit-process: melpa for details]
```

then you have probably checked out the melpa repository incorrectly with cloning the following url :

`https://github.com/captainflasmr/melpa`

rather than

`git@github.com:captainflasmr/melpa.git`

basically it will be the github SSH command rather than the HTTPS one.


## Create a Pull Request {#create-a-pull-request}

When you log into your fork of MELPA on github it will prompt you to :

> &lt;package-name&gt; had recent pushes 2 minutes ago - Compare &amp; pull request

Presumably because a push request has just been made.

If you log in the following day then go to your melpa fork -&gt; switch to branch -&gt; Contribute -&gt; Open Pull Request

If feedback has been given from a previous pull request access the pull request and update any comments.

On your forked GitHub repository, create a pull request against the original MELPA repository. Make sure you detail what the package does and why it's useful to users.

Select the pull request and fill in the following:

```nil
### Brief summary of what the package does

[Please write a quick summary of the package.]

### Direct link to the package repository

https://github.com/your/awesome_package

### Your association with the package

[Are you the maintainer? A contributor? An enthusiastic user?]

### Checklist
- [x] The package is released under a GPL-Compatible Free Software License
- [x] I've read CONTRIBUTING.org
- [x] I've used the latest version of package-lint to check for packaging issues, and addressed its feedback
- [x] My elisp byte-compiles cleanly
- [x] I've used M-x checkdoc to check the package's documentation strings
- [x] I've built and installed the package using the instructions in CONTRIBUTING.org
```

If you are not sure what to enter select **contributing guidelines** from the right hand side.

Note : as far as I am aware MELPA will be built off your latest tag using a naming convention of something like v0.2 or 0.2 to pull a "stable" version.


## Address Feedback {#address-feedback}

There may be feedback from the MELPA maintainers. Be prepared to make necessary adjustments to your recipe or package according to their suggestions and update the pull request comments accordingly and resubmit.  Usually no new branches will need to be created as the melpa pull request branch would have just contained a simple recipe which will not have changed.


## Merge {#merge}

Once your pull request is approved, the maintainers will merge it into the main MELPA repository. Your package will then become available to users and will be built on a regular basis.

Remember to keep your package repository well-maintained, as MELPA builds packages directly from the source (assuming from the latest tag). Any changes in your GitHub repository will reflect in the MELPA package after the next build.
