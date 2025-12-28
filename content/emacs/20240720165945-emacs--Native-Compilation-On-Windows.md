---
title: "Native-Compilation-On-Windows"
author: ["James Dyer"]
lastmod: 2024-07-20T16:59:00+01:00
tags: ["emacs", 2024]
categories: ["emacs", "linux"]
draft: true
thumbnail: "/emacs/20240720165945-emacs--Native-Compilation-On-Windows.jpg"
---

Compiling Emacs with native compilation on Windows involves several steps, but it can be a worthwhile endeavor if you want to take advantage of the performance improvements offered by native compilation. Native compilation in Emacs uses the \`libgccjit\` library to compile Emacs Lisp code to native code, which can then be executed directly by the CPU.

<!--more-->

Here are the general steps to compile Emacs with native compilation on Windows:

1.  ****Install MSYS2****:
    -   Visit the [MSYS2 website](<https://www.msys2.org/>) and download the installer.
    -   Run the installer and update the package database and base packages by running the following commands in the MSYS2 terminal:
        \`\`\`sh
        pacman -Syu
        pacman --noconfirm -Syu # Might be needed to fully update MSYS2
        \`\`\`

2.  ****Install Required Packages****:
    Use the following command to install the necessary packages:
    \`\`\`sh
    pacman -S base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-libgccjit mingw-w64-x86_64-cmake mingw-w64-x86_64-pkg-config git mingw-w64-x86_64-make
    \`\`\`

3.  ****Clone the Emacs Repository****:
    -   Navigate to the directory where you want to download Emacs sources in the MSYS2 bash shell, then run:
        \`\`\`sh
        git clone <https://github.com/emacs-mirror/emacs.git>
        cd emacs
        \`\`\`

4.  ****Configure the Build****:
    -   Create a separate directory for building Emacs (e.g., \`build\` directory inside the cloned \`emacs\` directory):
        \`\`\`sh
        mkdir build
        cd build
        \`\`\`
    -   Run the configuration script with native compilation enabled:
        \`\`\`sh
        ../autogen.sh
        ../configure --with-native-compilation --prefix=/mingw64
        \`\`\`

5.  ****Compile and Install Emacs****:
    -   Compile Emacs by running:
        \`\`\`sh
        make -j$(nproc)
        \`\`\`
    -   Install Emacs to the prefix directory specified during the configuration (in this case, \`/mingw64\`):
        \`\`\`sh
        make install
        \`\`\`

6.  ****Set Up Native Compilation****:
    -   Ensure that the installed Emacs can find the native compiler (\`libgccjit\`). This usually involves adding the MSYS2 \`/mingw64/bin\` directory to your PATH environment variable.

7.  ****Run Emacs****:
    -   You can now run Emacs using the installed executable:
        \`\`\`sh
        /mingw64/bin/emacs
        \`\`\`

\### Additional Notes

1.  ****Using GUI Emacs****:
    If you want to use the GUI version of Emacs, make sure you have installed the necessary GTK packages:
    \`\`\`sh
    pacman -S mingw-w64-x86_64-gtk3 mingw-w64-x86_64-pango mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-xwidgets
    \`\`\`

2.  ****Troubleshooting****:
    -   Make sure your environment variables are set correctly, especially PATH.
    -   Keep an eye on the output of each step to catch any errors or missing dependencies.

\### Example Emacs Configuration
Here's an example \`.emacs.d/init.el\` configuration to ensure native compilation works correctly once you start Emacs:

\`\`\`elisp
(setq package-native-compile t)
(setq comp-speed 2)
