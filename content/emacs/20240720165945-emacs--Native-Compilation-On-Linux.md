---
title: "Native-Compilation-On-Linux"
author: ["James Dyer"]
lastmod: 2024-07-20T16:59:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240720165945-emacs--Native-Compilation-On-Linux.jpg"
---

Compiling Emacs with native compilation on Linux involves several steps, but it can be a worthwhile endeavor if you want to take advantage of the performance improvements offered by native compilation. Native compilation in Emacs uses the \`libgccjit\` library to compile Emacs Lisp code to native code, which can then be executed directly by the CPU.

<!--more-->

Here are the general steps to compile Emacs with native compilation on Linux:

As I am currently using Arch I shall use the AUR and tailor PKBUILD

Here is a good post from Prot : <https://protesilaos.com/codelog/2022-07-06-build-emacs-arch-linux/> of which most of this post was taken from.

On Arch we can build custom packages cleanly by writing a PKGBUILD. This is a shell script that fetches the source and prepares it for installation. In the case of Emacs, we do not need to write the entire script ourselves: the community-driven Arch User Repository (AUR) already includes the emacs-git package.

As we likely want to customise certain aspects of the build, emacs-git should not be installed as-is (and generally one ought to always inspect what they install from the AUR). Instead, we must fetch the PGBUILD source, edit it, and build the package from there. This requires the base-devel package group. The resulting output is a regular package as far as the pacman is concerned.

We get the build script:

git clone <https://aur.archlinux.org/emacs-git.git/> path/to/emacs-git-aur

Then we change to the emacs-git-aur directory and visit the PKGBUILD file. It is properly annotated so one must read all the comments carefully to understand what each variable pertains to.

```sh
JIT="YES"
PGTK="YES"     # Use native GTK3 build. Supports Wayland, yay!
GTK3=          # GTK3 old windowing interface.
XWIDGETS="YES" # Use GTK+ widgets pulled from webkit2gtk. Usable.
SITTER="YES"   # Use tree-sitter incremental language parsing.
```

Now to build the package outright ($ refers to regular user privileges):

/path/to/emacs-git-aur $ makepkg -siC

-   **-s** (or `--syncdeps`): This flag tells `makepkg` to install any missing dependencies required to build the package. It will use the package manager (usually `pacman`) to find and install these dependencies.

-   **-i** (or `--install`): After the package is successfully built, this flag will automatically install the newly built package using `pacman`.

-   **-C** (or `--cleanbuild`): This flag tells `makepkg` to remove the existing build directory before building the package. This ensures that the build starts from a clean state, which can help avoid issues related to remnants of previous builds.

the -C is not necessary here, but I added it just to be sure (it is required for clean builds).

As a final step, makepkg will prompt to install the package and ask to escalate privileges. In case we miss that part, we do not need to restart the whole process. The easier way is to check the directory we are in for a file named emacs-git-29.0.50.157962-1-x86_64.pkg.tar.zst or something like that and run the following (# denotes superuser privileges):

/path/to/emacs-git-aur # pacman -U emacs-git-29.0.50.157962-1-x86_64.pkg.tar.zst

The first build will take a long time, especially with native compilation enabled, but subsequent ones shall be faster (I wrote this blog post while waiting for it).

Rebuilding a package automatically fetches the updates. All we ever need is to re-run makepkg -si (occasionally with -C included). However, as time goes by, we might also have to check the PKGBUILD for updates, such as when new build options are added upstream. No problem! We just git pull from the source we cloned earlier (the <https://aur.archlinux.org/emacs-git.git/>). Again, the rest is all about scrutinising the PKGBUILD and running makepkg.
