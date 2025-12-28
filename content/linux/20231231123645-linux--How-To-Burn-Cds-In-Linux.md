---
title: "How-To-Rip-Cds-In-Linux"
author: ["James Dyer"]
lastmod: 2023-12-31T12:36:00+00:00
tags: ["k3b", 2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20231231123645-linux--How-To-Burn-Cds-In-Linux/2024-01-01-15-21-44_001.jpg"
---

First plug in a CD drive and insert the disk and make sure the Audio Disc is available on the system, typically through the file explorer.

<!--more-->


## K3b {#k3b}

Run up K3b (K3b startup may require patience - ignore any errors regarding executables not present to write to CD or DVDs as we are only reading from the disk and [x] Do now show again -&gt; **Close**)

Menu -&gt; Tools -&gt; Rip Audio CD...

{{< figure src="/ox-hugo/2024-01-01-15-30-30_001.jpg" width="60%" >}}

A window showing the tracks to rip will be displayed

**Start Ripping**


### Settings: {#settings}

-   Filetype: Mp3 (Lame)
-   Target Folder: &lt;destination directory&gt;


### File Naming: {#file-naming}

-   Ripped files pattern: %T/%n_%t
-   Replace all blanks with: [x] _

**Start Ripping** (a dialog showing Ripping Audio Tracks from ... will be displayed)

{{< figure src="/ox-hugo/2024-01-01-10-21-00.jpg" width="80%" >}}

Wait for all tracks to be ripped into selected location and **Success** to be displayed

**Close K3b**


## Tidy Up {#tidy-up}

Check the files names and remove all strange characters.

If the album is one of a multiple set of disks then prepend an integer dash, for example:

```nil
1-01_Beat_It.mp3
2-01_How_Will_I_Know.mp3
```


## EasyTAG (add cover art) {#easytag--add-cover-art}

In a file explorer, Navigate to the ripped folder right click and Open With -&gt; EasyTAG

{{< figure src="/ox-hugo/2024-01-01-15-21-44_001.jpg" width="80%" >}}

You can add any additional tags at this stage but as k3b will have validly auto tagged all the main ones we just need to add some cover art.

Search for the album cover art online and screenshot the album image.

In EasyTag, select all the tracks, on the right hand side select the Images tab, drag and drop in the captured image then select the image icon at the bottom of the image tab which will copy the image to all selected tracks and select the **Save** icon to save all tracks.

{{< figure src="/ox-hugo/2024-01-01-15-52-14.jpg" width="80%" >}}


## Copy in to main Music folder {#copy-in-to-main-music-folder}

Copy the directory into your music collection folder.


## Sanity Test {#sanity-test}

Load the mp3 files into a player (for example **Elisa**) and check that title / album and cover art is correctly loaded.

For emacs run `emms-add-directory` and select the relevant directory.


## Sync to external music players {#sync-to-external-music-players}

Now sync the updated music collection to a NAS and possible SD card based music players.
