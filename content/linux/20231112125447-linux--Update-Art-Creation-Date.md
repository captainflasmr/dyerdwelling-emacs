---
title: "Update-Art-Creation-Date"
author: ["James Dyer"]
lastmod: 2023-11-12T12:54:00+00:00
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20231112125447-emacs--Update-Art-Creation-Date.jpg"
---

The timestamp may be incorrect given the tagged name after the copy to the image directory, so use T and dired to correct these the time is the TIMESTAMP but with the last two numbers (seconds) having a decimal point.

<!--more-->

I sometimes have these issues when copying to a new computer although technically all the timestamps should be preserved.

Then run dwim script PictureUpdateToCreateDate which will apply to modified date to creation date to ensure consistency even if the modification date gets messed up on a transfer.

These changes have to take place as my art typically isn't taken with a camera so an automatic createdate is not applied.

it seems however I need to be careful in it not removing the tags, but I shall fix that at a later date!

With the tags this could be more complicated and as there are few art files I will now have to deal with then reapplying these things probably isn't a big deal.

I have left exiftool to create a backup file just in case I want to copy some tags across.
