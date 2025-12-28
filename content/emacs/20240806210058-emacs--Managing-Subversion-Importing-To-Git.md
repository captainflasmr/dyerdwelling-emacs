---
title: "Managing-Subversion-Importing-To-Git"
author: ["James Dyer"]
lastmod: 2024-08-06T21:00:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240806210058-emacs--Managing-Subversion-Importing-To-Git.jpg"
---

<!--more-->


## Create a Local Subversion Repository on Linux {#create-a-local-subversion-repository-on-linux}


### Prerequisites {#prerequisites}

Make sure you have Subversion installed on your Linux system. You can install it using the package manager for your distribution. For example, on Debian or Ubuntu, you can use:

```shell
sudo apt update
sudo apt install subversion
```

On Red Hat-based distributions like CentOS or Fedora, you might use:

```shell
sudo dnf install subversion
```


### Steps to Create a Local SVN Repository {#steps-to-create-a-local-svn-repository}


#### Create the Repository Directory {#create-the-repository-directory}

Create a directory where you want to store your SVN repository. For example:

```shell
mkdir ~/svn_repos
```


#### Create the Repository {#create-the-repository}

Use the `svnadmin` tool to create a new repository within the directory you just created:

```shell
svnadmin create ~/svn_repos/my_project
```


#### Import Initial Project Files (Optional) {#import-initial-project-files--optional}

If you already have a project you want to import into the repository, navigate to the project directory and run:

```shell
svn import . file:///home/your_username/svn_repos/my_project -m "Initial import"
```

Replace `/home/your_username` with your actual home directory path.


### Use the Repository {#use-the-repository}


#### Checkout the Repository {#checkout-the-repository}

Checkout a working copy from the repository to your local directory:

```shell
svn checkout file:///home/your_username/svn_repos/my_project ~/my_working_copy
```


#### Add Files to the Working Copy {#add-files-to-the-working-copy}

Navigate to your working copy directory and add files or directories:

```shell
cd ~/my_working_copy
echo "Hello, SVN!" > sample.txt
svn add sample.txt
```


#### Commit Changes {#commit-changes}

Commit your changes to the SVN repository:

```shell
svn commit -m "Added sample.txt"
```


#### Update Working Copy {#update-working-copy}

To update your working copy with the latest changes from the repository, use:

```shell
svn update
```


### Common SVN Commands {#common-svn-commands}

-   Add a file or directory:

<!--listend-->

```shell
svn add <file_or_directory>
```

-   Remove a file or directory:

<!--listend-->

```shell
svn delete <file_or_directory>
```

-   View the status of your working copy:

<!--listend-->

```shell
svn status
```

-   See the log of changes:

<!--listend-->

```shell
svn log
```

With these steps, you should be able to set up a local SVN repository and start using it for version control on your Linux system.


## Emacs Tools to Manage an SVN Repository {#emacs-tools-to-manage-an-svn-repository}


### \`psvn.el\` {#psvn-dot-el}

-   ****Description****: `psvn.el` is a popular and comprehensive Subversion interface for Emacs. It offers various features to manage SVN repositories directly within Emacs.
-   ****Installation****: It may come pre-installed with some Emacs distributions, but if not, you can download it from its [repository](http://www.xsteve.at/prg/emacs/psvn.el).
    To use `psvn.el`, add the following to your Emacs configuration:

<!--listend-->

```emacs-lisp
(autoload 'svn-status "psvn" "Run `svn status`." t)
```

-   ****Usage****: Use `M-x svn-status` to start interacting with your SVN repository.


### Built-in VC (Version Control) Mode {#built-in-vc--version-control--mode}

-   ****Description****: Emacs comes with a built-in version control (VC) mode that supports various version control systems, including SVN.
-   ****Configuration****: No additional configuration is needed to use `vc` mode for SVN; it should work out of the box.
-   ****Usage****: When you open a file in a directory under SVN control, you can use various VC commands:
    -   `C-x v v`: Perform the next appropriate version control operation (e.g., check-in, check-out).
    -   `C-x v i`: Register the file with the version control system.
    -   `C-x v d`: Display the status of files in the directory.
    -   `C-x v ~ <revision>`: Check out a specific revision.


### \`dsvn.el\` {#dsvn-dot-el}

-   ****Description****: `dsvn.el` is another Subversion front-end for Emacs that gives a `dired`-like interface for managing SVN repositories.
-   ****Installation****: You can install `dsvn` via MELPA by adding the following to your configuration if you're using the package manager:

<!--listend-->

```emacs-lisp
(use-package dsvn
  :ensure t)
```

-   ****Usage****: Use `M-x svn-update` to update the SVN repository, `M-x svn-status` for the status of the repository, and other SVN-related commands.


### \`magit-svn\` (if using Magit) {#magit-svn--if-using-magit}

-   ****Description****: If you are already using `magit` for Git repositories but also need to handle SVN repositories, `magit-svn` can be useful. It integrates SVN support into `magit`.
-   ****Installation****: You'll need to install both `magit` and `magit-svn`.

<!--listend-->

```emacs-lisp
(use-package magit
  :ensure t)

(use-package magit-svn
  :ensure t
  :after magit
  :config
  (magit-svn-mode 1))
```

-   ****Usage****: This package provides SVN support within the familiar Magit interface. You can initialize it within a repository with `M-x magit-svn`.


### \`ISVN\` (Interactive SVN) {#isvn--interactive-svn}

-   ****Description****: ISVN provides an interactive interface for Subversion within Emacs.
-   ****Installation****: You can install ISVN from the Emacs Wiki or its dedicated repository.
-   ****Usage****: You can interact with your repository using intuitive keybindings specific to ISVN.


### Example Configuration {#example-configuration}

Here’s an example of a minimal `.emacs` or `init.el` configuration to get started with the recommended tools:

```emacs-lisp
;; Use built-in VC Mode for SVN
(setq vc-handled-backends '(SVN Git))

;; Load PSVN
(autoload 'svn-status "psvn" "Run `svn status`." t)

;; Example keybinding for PSVN
(global-set-key (kbd "C-x s") 'svn-status)

;; Optional: Enable magit-svn if you're using Magit
(use-package magit
  :ensure t)

(use-package magit-svn
  :ensure t
  :after magit
  :config
  (magit-svn-mode 1))

;; Optional: Use dsvn if preferred
(use-package dsvn
  :ensure t)
```


## Clone a Git repository from a local Subversion repository {#clone-a-git-repository-from-a-local-subversion-repository}

Yes, you can use a locally checked out Subversion repository as the source for `git svn clone`. However, it requires setting up a temporary local Subversion server if you want to do this without re-checking out from the central repository. Here’s how you can achieve this:


### 1. Install a Subversion Server {#1-dot-install-a-subversion-server}

Ensure you have `svnserve` installed. On Ubuntu, you can install it with:

```sh
sudo apt-get update
sudo apt-get install subversion
```


### 2. Start Subversion Server {#2-dot-start-subversion-server}

Run `svnserve` to serve your local repository. You need to specify the root directory where your Subversion repository is located. For example:

```sh
svnserve -d -r /home/jdyer/source/working
```

This starts the server as a daemon (`-d`) and sets the root of the served files to ~ /home/jdyer/source/working~ (`-r`).


### 3. Using `git svn clone` {#3-dot-using-git-svn-clone}

Now, you can use `git svn clone` to clone the repository via the `svn://` protocol. Assuming your project in the Subversion repository is called `my_project`:

```sh
git svn clone svn://localhost/my_project my_project.git
```

This command clones the Subversion repository located at `svn://localhost/my_project` into a Git repository named `my_project.git`.


### Example {#example}

Here’s the complete process summarized in a series of commands:

```sh
# Step 1: Install svnserve if not already installed
sudo apt-get update
sudo apt-get install subversion

# Step 2: Start svnserve, serving your local Subversion repository
svnserve -d -r /home/jdyer/source/working

# Step 3: Clone the repository with git svn
git svn clone svn://localhost/my_project my_project.git
```


### Stopping the server {#stopping-the-server}

Once the cloning process is done, don’t forget to stop the `svnserve` process if it’s no longer needed. You can find its process ID and stop it.

```sh
pkill svnserve
```


### Note {#note}

This approach avoids re-fetching data from a remote Subversion repository and speeds up the conversion process by using the locally checked-out copy. However, be cautious: this setup is quite manual and assumes good familiarity with both SVN and Git tools.
