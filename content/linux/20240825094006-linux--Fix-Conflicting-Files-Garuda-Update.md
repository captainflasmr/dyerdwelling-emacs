---
title: "Fix-Conflicting-Files-Garuda-Update"
author: ["James Dyer"]
lastmod: 2024-08-25T09:40:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240825094006-emacs--Fix-Conflicting-Files-Garuda-Update.jpg"
---

<!--more-->

Total (234/234)                                                                    1532.4 MiB  8.33 MiB/s 03:04 [-------------------------------------------------------------------] 100%
(234/234) checking keys in keyring                                                                               [-------------------------------------------------------------------] 100%
(234/234) checking package integrity                                                                             [-------------------------------------------------------------------] 100%
(234/234) loading package files                                                                                  [-------------------------------------------------------------------] 100%
(234/234) checking for file conflicts                                                                            [-------------------------------------------------------------------] 100%
error: failed to commit transaction (conflicting files)
adw-gtk-theme: /usr/share/themes/adw-gtk3-dark/gtk-3.0/assets/bullet-symbolic.svg exists in filesystem (owned by adw-gtk3)
adw-gtk-theme: /usr/share/themes/adw-gtk3-dark/gtk-3.0/assets/bullet-symbolic.symbolic.png exists in filesystem (owned by adw-gtk3)
adw-gtk-theme: /usr/share/themes/adw-gtk3-dark/gtk-3.0/assets/bullet@2-symbolic.symbolic.png exists in filesystem (owned by adw-gtk3)
adw-gtk-theme: /usr/share/themes/adw-gtk3-dark/gtk-3.0/assets/check-symbolic.svg exists in filesystem (owned by adw-gtk3)

The error you are encountering is due to file conflicts between the
`adw-gtk-theme` and `adw-gtk3` packages. This usually happens because
both packages contain files with the same name and paths.

Here is a suggested way to resolve this issue:

1.  **Remove the conflicting package**: Since the `adw-gtk3` package is
    causing the conflict, you can remove it before installing the update.
    This can be done using the following command:
    ```sh
          sudo pacman -R adw-gtk3
    ```

2.  **Overwrite the conflicts** (if needed): If removing the conflicting
    package doesn't fully resolve the issue, you can choose to overwrite
    the existing files. This tells `pacman` to ignore file conflicts and
    proceed with the update:
    ```sh
          sudo pacman -Syu --overwrite "*"
    ```

3.  **Reinstall the adw-gtk3 package** (if necessary): If you still need
    the `adw-gtk3` package after updating, you can reinstall it:
    ```sh
          sudo pacman -S adw-gtk3
    ```


## Full Steps: {#full-steps}

1.  Remove the conflicting package: `sh     sudo pacman -R adw-gtk3`

2.  Update your system and overwrite if necessary:
    `sh     sudo pacman -Syu --overwrite "*"`

3.  Reinstall the `adw-gtk3` package if needed:
    `sh     sudo pacman -S adw-gtk3`


## Important Notes: {#important-notes}

-   Use the `--overwrite` option with caution as it forces the
    installation of the package even when there are conflicts.
-   Always ensure you have backups or restore points, especially if you
    are dealing with system-critical packages.

Following these steps should help you resolve the file conflict issue
during your system update.
