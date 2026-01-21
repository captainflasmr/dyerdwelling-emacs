---
title: "pcmanfm-add-desktop-actions"
author: ["James Dyer"]
lastmod: 2024-12-08T08:37:00+00:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20241208083711-emacs--pcmanfm-add-desktop-actions.jpg"
---

To add custom right-click actions in **PCManFM**, you create `.desktop` files (just as you've done) and place them in the appropriate directory that PCManFM checks for custom actions. Here's how you can do it step by step:


## 1. **Location for the `.desktop` files** {#1-dot-location-for-the-dot-desktop-files}

The `.desktop` files for custom right-click actions in PCManFM (and GTK-based `libfm` file managers) should be placed in the following directory:

\*+begin_src
~/.local/share/file-manager/actions/
\*+end_src

If the directory doesn’t exist, you need to create it. Run:

\*+begin_src bash
mkdir -p ~/.local/share/file-manager/actions/
\*+end_src

---


## 2. **Structure of the `.desktop` File** {#2-dot-structure-of-the-dot-desktop-file}

Here’s an example of what a typical custom `.desktop` file looks like:

\*+begin_src ini
[Desktop Entry]
Type=Action
Name=Your Custom Action
Icon=utilities-terminal
Profiles=default;

[X-Action-Profile default]
Exec=/path/to/your/script.sh %f
Name=Default profile
MimeTypes=application/pdf;inode/directory;
\*+end_src

**Explanation of Options:**

-   **Type:** Must always be `Action`.
-   **Name:** The label that will appear in the right-click context menu.
-   **Icon:** (Optional) The icon to display next to the action in the menu. Use standard icon names or paths.
-   **Profiles:** List of profiles referred to in the `[X-Action-Profile]` section.
-   **Exec:** Command to execute. Use placeholders like `%f`, `%F`, `%d`, `%D`, etc.:
    -   `%f`: Single selected file.
    -   `%F`: Multiple selected files.
    -   `%d`: Single selected directory.
    -   `%D`: Multiple selected directories.
-   **MimeTypes:** Restricts the action to specific file types (e.g., `application/pdf`, `image/*`, `inode/directory`). Omit this to allow the action for all file types.

---


## 3. **Example: Add "Open with Nano"** {#3-dot-example-add-open-with-nano}

Create the `nano.desktop` file as follows:

\*+begin_src ini
[Desktop Entry]
Type=Action
Name=Open with Nano
Icon=accessories-text-editor
Profiles=default;

[X-Action-Profile default]
Exec=gnome-terminal -- nano %f
Name=Default profile
MimeTypes=text/plain;
\*+end_src

Save this file to:

\*+begin_src
~/.local/share/file-manager/actions/nano.desktop
\*+end_src

---


## 4. **Reload PCManFM** {#4-dot-reload-pcmanfm}

To apply the changes, restart PCManFM or log out and log back in.

Alternatively, you can restart PCManFM by running:

\*+begin_src bash
pcmanfm --quit &amp;&amp; pcmanfm &amp;
\*+end_src

---


## 5. **Check the Context Menu** {#5-dot-check-the-context-menu}

Now, when you right-click on a file that matches the `MimeTypes` you've set in the `.desktop` file (in the example above, plain text files), you should see the "Open with Nano" option appear in the context menu.

---


## Debugging Tips {#debugging-tips}

-   Ensure the `.desktop` file has proper permissions:

    ==\`bash
    chmod 644 ~/.local/share/file-manager/actions/\*.desktop
