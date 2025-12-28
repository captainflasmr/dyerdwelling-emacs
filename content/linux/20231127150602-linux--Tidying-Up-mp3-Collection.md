---
title: "Tidying-Up-mp3-Collection"
author: ["James Dyer"]
lastmod: 2023-11-27T15:06:00+00:00
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20231127150602-emacs--Tidying-Up-mp3-Collection.jpg"
---

<!--more-->


## mp3tag configure {#mp3tag-configure}

Run mp3tag with MyMusicLibrary

File -&gt; Options -&gt; Tags -&gt; Mpeg
Read

-   [X] ID3v1
-   [X] ID3v2
-   [ ] APE

Write

-   [X] ID3v2.3 ISO-8859-1

Remove

-   All selected


## mp3tag tag save to 2.3 {#mp3tag-tag-save-to-2-dot-3}

Select all tracks

Save Tag

this will write to ID3v2.3 ISO-8859-1 as selected above

I had quite a few that were v2.4 which can cause some issues with some mp3 players notable SanDisk ClipJam


## mp3tag tag remove {#mp3tag-tag-remove}

Add action "remove fields except"

Filename;Path;Tag;Title;Artist;Album;AlbumArtist;Track;Discnumber;Year;Genre;Codec;Bitrate;Frequency;Length;Modified;picture

Note: the ;picture is not obvious but will preserve the cover art

This will remove the following:
Comment
and anything else presumably?


## eyeD3 command line tidy {#eyed3-command-line-tidy}

I think at this point some ID3v1 still exists so:

cd ~/MyMusicLibrary
eyeD3 -r --encoding utf8 .
eyeD3 -r --remove-v1 .


## Sanity {#sanity}

eyeD3 -r -v . &gt; ~/Music-tag-info-after-all.txt

Now the media for the mp3 player is much faster and doesn't seem to get stuck.

I might think about removing the following:

-   [X] Year
-   [ ] Album Artist (albumartist)

which actually removed something called **recording date**


## playlist {#playlist}

Trying putting in CLIP JAM/Playlists

in the form \*.m3u:

../../EOS_DIGITAL/MyMusicLibrary/1000_Forms_of_Fear_(Deluxe_Version)/01_Chandelier.mp3
../../EOS_DIGITAL/MyMusicLibrary/1000_Forms_of_Fear_(Deluxe_Version)/02_Big_Girls_Cry.mp3

There isn't one in the EOS_DIGITAL directly
