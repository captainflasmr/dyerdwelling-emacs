---
title: "Hugo No Carriage Return After Frame"
author: ["James Dyer"]
lastmod: 2022-09-01
tags: ["hugo", 2022]
categories: ["linux", "open-source"]
draft: false
thumbnail: "/linux/linux--hugo-no-carriage-return-after-frame__linux_hugo.jpg"
---

I am currently polishing up my web page and I am now focussing on the little annoying formatting / alignment issues.

<!--more-->

One of these is involving a default shortcode called `{< youtube >}`

(**Note**: I deliberately missed off second brace as I am writing this in org!)

Anyways, I have the following markdown:

```text
{< youtube IDK-4lhQ-sE >}

A gradual fading version of a portrait I did for my girlfriends
parents.

Using ArtRage and Infinite Painter on various drawing tablets
```

which produces the following displayed html:

{{< figure src="/ox-hugo/2022-09-01_11-02.jpg" width="100%" >}}

Note how the text starts right after the youtube video even though the markdown includes a carriage return.

So the first potential fix is to add a `<br>` just after the shortcode, which in fact does work, however there is one catch.  The markdown file is just one of hundreds I have generated from my youtube channel.  If I add this then I will have to apply it to all videos.

I maintain a list of my youtube videos in a single org file and technically it probably wouldn't be too difficult to emacs macro / grep / replace my way through it, however I don't like this idea too much as I don't really want to be mixing html with a markdown file.

So on to plan B.

With a little investigation I can see that the video is embedded in an iframe.  This presents the possibility of a quick and dirty fix that may have repercussions down the line.

Lets add in a little css:

```nil
iframe {
    padding: 0rem 0rem 1rem 0rem;
    margin: 0rem 0rem 0rem 0rem;
}
```

I have just applied a little padding to the bottom of the iframe!  I thought I would include `margin` also just in case I wanted to do it that way in the future.  Right lets see what this looks like now:

{{< figure src="/ox-hugo/2022-09-01_11-16.jpg" width="100%" >}}

yup that will do nicely!

for now...
