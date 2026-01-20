---
title: "Installing-Epson-Photo-1500W-On-Linux"
author: ["James Dyer"]
lastmod: 2024-05-19T21:07:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240519210710-emacs--Installing-Epson-Photo-1500W.jpg"
---

<!--more-->


## Find Model / Driver {#find-model-driver}

lpinfo -m

which will return something like :

gutenprint.5.3://escp2-1410/expert Epson Stylus Photo 1410 - CUPS+Gutenprint v5.3.4
gutenprint/5.3/Global/stp-escp2-1410.5.3.sim.ppd.gz Epson Stylus Photo 1410 - CUPS+Gutenprint v5.3.4 Simplified

with the driver files located in:

/usr/share/cups/model/gutenprint/5.3/Global

but the lpstat command which we will use to set up the printer already knows about :

/usr/share/cups/model


## Find uri {#find-uri}

This is simple:

lpinfo -v

direct usb://EPSON/Stylus%20Photo%201500?serial=4E4152593031383362

if no obvious printer or EPSON is apparent then the printer is likely not connected.


## Set Printer Up {#set-printer-up}

I think you need to be root to set it up, we now have the relevant info :

sudo lpadmin -p Epson_1500W -E -v usb://EPSON/Stylus%20Photo%201500?serial=4E4152593031383362 -m gutenprint/5.3/Global/stp-escp2-1410.5.3.sim.ppd.gz

-E enables

you will always get the following message :

lpadmin: Printer drivers are deprecated and will stop working in a future version of CUPS.


## test {#test}

lpstat -p

printer Epson_1500W is idle.  enabled since Sun 19 May 2024 21:02:00 BST


## error checking {#error-checking}

check in :

/var/log/cups/error_log


## uninstall {#uninstall}

sudo lpadmin -x Epson_1500W


## Print! {#print}

you should technically be able to print something now!
