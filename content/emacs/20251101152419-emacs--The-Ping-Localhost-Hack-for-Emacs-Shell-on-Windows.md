---
title: "The Ping Localhost Hack for Emacs Shell on Windows"
author: ["James Dyer"]
lastmod: 2025-11-01T15:24:00+00:00
tags: ["ping", "emacs", 2025]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20251101152419-emacs--The-Ping-Localhost-Hack-for-Emacs-Shell-on-Windows.jpg"
---

{{< figure src="/emacs/20251101152419-emacs--The-Ping-Localhost-Hack-for-Emacs-Shell-on-Windows.jpg" width="100%" >}}

For a little while now, I've been developing software that needs to run on Windows (I know, I know!) and occasionally firing up batch scripts from within Emacs shell. Everything was working perfectly until I needed to add a simple delay between commands. That's when I discovered one of Windows batch scripting's more peculiar quirks.

<!--more-->

The obvious solution for adding a delay in a Windows batch script is the `timeout` command:

```batch
timeout /T 5
```

Simple enough, right? Well, it turns out that when you run this from Emacs shell (or any non-interactive environment), it just... doesn't work. The script either hangs indefinitely or skips the timeout entirely. Even adding the `/NOBREAK` flag doesn't reliably solve the issue.

The `timeout` command is actually an interactive program that expects to interact with a proper Windows console. It's designed to wait for user input OR timeout, whichever comes first. When running from Emacs shell:

-   There's no proper console attachment that `timeout` can latch onto
-   Input redirection gets confused
-   The TTY limitations of Emacs shell mean `timeout` can't find what it's looking for

This is particularly frustrating when you've got a perfectly good batch script that works fine from cmd.exe but fails spectacularly when launched from your beloved Emacs environment.

Here's where things get interesting (and slightly ridiculous). The solution that the Windows community by all accounts has rallied around is the `ping` command:

```batch
ping 127.0.0.1 -n 6 > nul
```

So here we are using a network diagnostic tool as a timer. But it works brilliantly! Here's why:

-   `ping` is completely non-interactive - it just runs and exits
-   Each ping takes roughly 1 second by default
-   The `-n` flag specifies the number of pings, so `-n 6` gives you approximately 5 seconds (n-1)
-   It's available on every Windows system, no exceptions
-   It doesn't care about console attachment, TTY setup, or any of that nonsense

In my batch scripts that launch from Emacs, I've settled on this pattern:

```batch
@echo off
echo Starting process...
ping 127.0.0.1 -n 3 > nul
echo Continuing after 2 second delay...
```

The `> nul` redirects all the ping output to nowhere, so you don't see the typical ping response spam in your output. Clean, simple, and most importantly - it actually works!

This whole situation is a perfect example of the kind of head-scratching workarounds you encounter when working across different environments. Emacs shell on Windows is already operating in a slightly weird space - it's not quite a native Windows console, not quite a Unix-like shell, but something in between.

After adding this to my batch scripts, everything now runs smoothly from Emacs shell on Windows. The software launches, pauses where it needs to, and continues without any console-related tantrums.

Is it elegant? Not particularly. Is it a proper use of the ping command? Absolutely not. Does it work perfectly every single time? You bet it does!

And really, isn't that the most important thing? Sometimes the best solution isn't the most theoretically pure one - it's the one that just works, regardless of how strange it might look to an outsider.
