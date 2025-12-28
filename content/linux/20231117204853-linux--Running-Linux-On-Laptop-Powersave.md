---
title: "Battery Laptop Optimisations on Garuda Linux"
author: ["James Dyer"]
lastmod: 2023-11-17T20:48:00+00:00
tags: ["laptop", "garuda", 2023]
categories: ["linux", "open-source"]
draft: false
thumbnail: "/linux/20231117204853-emacs--Running-Linux-On-Laptop-Powersave.jpg"
---

There are quite a few different ways to reduce power consumption on a laptop, most linux distros will be geared towards desktop, but I am always installing on a laptop, generally I was looking to use **tlp** but the options are quite bewildering and can be daunting so I was looking for something more lightweight and in fact **garuda** has a good starting point for this:

<!--more-->

{{< figure src="/ox-hugo/20231117204853-emacs--Running-Linux-On-Laptop-Powersave.jpg" class="emacs-img" >}}

Table Of Contents
---

<div class="ox-hugo-toc toc local">

- [auto-cpufreq](#auto-cpufreq)
- [intel_pstate=passive](#intel-pstate-passive)
- [turning of bluetooth](#turning-of-bluetooth)

</div>
<!--endtoc-->

---

<https://forum.garudalinux.org/t/guide-old-opinion-configuring-garuda-linux-for-laptop/7685>


## auto-cpufreq {#auto-cpufreq}

I chose to use `auto-cpufreq` and it encourages you to manually install, so:

```bash
git clone https://github.com/AdnanHodzic/auto-cpufreq.git
cd auto-cpufreq && sudo ./auto-cpufreq-installer
```

and then I just ran the GUI version and installed the systemd unit which will be activated now on every startup.


## intel_pstate=passive {#intel-pstate-passive}

There maybe other things I can do in the future, for example regarding:

```nil
intel_pstate=passive
```

It looks like you put this on the end of the kernel run up at GRUB time as in my Samsung brightness fix to GRUB_CMDLINE_LINUX_DEFAULT

Garuda Welcome -&gt; Garuda Boot Options

However on an initial test using emacs at rest the cpu was averaging around 10% and even the window navigation felt laggy.  I suspect it may be conflicting with `auto-cpufreq` which is often the way these power-saving tools are concerned.


## turning of bluetooth {#turning-of-bluetooth}

Bluetooth is of course very power efficient but that I am assuming this is just the transfer protocol? what about the actual device on board?, I don't ever need to use this so it is worth considering just disabling in `systemd`
