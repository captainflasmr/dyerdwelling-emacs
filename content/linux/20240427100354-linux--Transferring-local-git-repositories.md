---
title: "Transferring-local-git-repositories"
author: ["James Dyer"]
lastmod: 2024-04-27T10:03:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240427100354-emacs--Transferring-local-git-repositories.jpg"
---

Transferring local git repositories that are linked to GitHub and encountering issues like permission changes or files not being up-to-date can be a multifaceted problem. However, it can often be resolved by properly understanding the mechanisms involved when copying repositories and ensuring that all changes are correctly pushed to GitHub. Below are the steps and checks you can perform to ensure a smoother transfer.

<!--more-->

\### Pre-Transfer Checklist

Before transferring your repositories, ensure the following:

1.  ****Commit All Changes****: Make sure all your local changes have been committed.
    \`\`\`bash
    git status
    git add .
    git commit -m "Commit message"
    \`\`\`

2.  ****Push to GitHub****: Confirm all local commits are pushed to the remote GitHub repository.
    \`\`\`bash
    git push origin main
    \`\`\`
    Replace \`main\` with your default branch name if different.

\### Transfer Process

Assuming you've pushed all changes to GitHub, follow these steps for a clean transfer:

1.  ****Clone Instead of Copy****: Instead of copying the local repository folder directly, it's better to clone the repository afresh in the new location. This ensures that all remote settings and commits are correctly configured.
    \`\`\`bash
    git clone <https://github.com/username/repository.git>
    \`\`\`

2.  ****File Permissions****: The file mode changes (\`100644\` to \`100755\`) typically occur due to differences in OS file system behavior or Git configuration. To ignore file mode changes in Git, configure it as follows:
    \`\`\`bash
    git config core.fileMode false
    \`\`\`
    Apply this setting in all your repositories if file permission changes are irrelevant to your project.

\### Dealing with Already Transferred Repositories

If you've already copied your repositories and are facing issues:

1.  ****Resetting File Permissions****: To normalize file permissions back to what is stored in the remote, you can use:
    \`\`\`bash
    git diff --summary | grep 'mode change' | awk '{print $4}' | xargs chmod 644
    \`\`\`
    This command adjusts the permissions of files flagged with mode changes, assuming you want to set them all to \`644\`. Adjust accordingly.

2.  ****Fetching Latest Changes****: Ensure you have the latest changes from GitHub:
    \`\`\`bash
    git fetch origin
    git reset --hard origin/main
    \`\`\`
    Again, replace \`main\` with your relevant branch name.

3.  ****Pushing to Remote****: If you've made any adjustments or want to ensure everything is up-to-date, push to GitHub:
    \`\`\`bash
    git push origin main
    \`\`\`

\### Additional Tips

-   ****.gitignore****: Ensure your \`.gitignore\` file is configured to ignore files that should not be tracked by Git. This can prevent unwanted files from being considered changes.
-   ****Check Filesystem Types****: If transferring between different filesystems (NTFS on Windows vs. EXT4 on Linux), be aware of how each filesystem handles permissions and consider this when encountering file mode changes.

By following the above steps and ensuring your Git configuration is suited to your project's requirements in terms of handling file permissions and line endings, you can minimize issues related to file inconsistencies and permission errors during repository transfer.
