---
title: "Read-USB-Speeds"
author: ["James Dyer"]
lastmod: 2024-05-19T16:27:00+01:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20240519162727-emacs--Read-USB-Speeds.jpg"
---

<!--more-->

sudo hdparm -t /dev/sda1


## summary of expected results {#summary-of-expected-results}


### USB Standards and Speeds {#usb-standards-and-speeds}

-   ****USB 2.0:**** Offers transfer rates up to 480 Mbps, but real-world speeds often hover around 30-40 MB/s for reads.
-   ****USB 3.0 (also known as USB 3.1 Gen 1):**** Officially supports speeds up to 5 Gbps. Typical real-world read speeds might range from 100 MB/s to 400 MB/s, depending on the device.
-   ****USB 3.1 (also known as USB 3.1 Gen 2):**** Doubles the potential throughput to 10 Gbps. Real-world speeds for high-quality drives can range from 300 MB/s to close to 1 GB/s for some external SSDs.
-   ****USB 3.2:**** Not as widespread in terms of devices at the time of writing, but it potentially offers up to 20 Gbps for devices supporting the standard.
-   ****USB 4:**** Offers speeds up to 40 Gbps under ideal conditions, but devices utilizing this standard are still not as common. Speeds will highly depend on the devices and setups involved.


### Drive Types {#drive-types}

-   ****HDD:**** External HDDs are limited by their mechanical nature, with speeds often falling in the range of 80-160 MB/s for USB 3.0 and above interfaces.
-   ****SSD:**** External SSDs can take better advantage of faster USB interfaces, with read speeds potentially reaching up to and exceeding 1 GB/s for USB 3.1 and later, assuming the drive's internal speed supports it.
-   ****Flash Drives:**** Speeds can vary widely based on the quality and design of the flash drive, from as slow as 10 MB/s for low-end USB 2.0 drives to 400 MB/s or more for high-end USB 3.1 (and beyond) flash drives.


### Other Factors {#other-factors}

-   ****File System:**** The type of file system (e.g., NTFS, FAT32, exFAT, HFS+, ext4) can affect speeds due to differences in efficiency and overhead.
-   ****File Size and Type:**** Smaller files can take longer to transfer overall than large files due to overhead. A large number of small files can significantly reduce the speed.
-   ****Computer and Port:**** The USB port's specification on your computer and how it handles data can also impact speeds. Older computers may not fully leverage the speeds offered by newer USB standards.


### Typical Ranges {#typical-ranges}

Given these variables, here’s a very broad set of typical ranges for read speeds from an attached USB drive:

-   USB 2.0 drives: ****20-40 MB/s****
-   USB 3.0/3.1 drives: ****100-400 MB/s****
-   High-end USB 3.1/3.2 SSDs: ****up to 1 GB/s or slightly more****
-   USB 4 devices: ****potentially faster than 1 GB/s****, but dependent on device capabilities and other factors.


## hard disk native {#hard-disk-native}

/dev/nvme0n1p7:
 Timing buffered disk reads: 1608 MB in  3.00 seconds = 535.66 MB/sec


## READ blue usba/usbc - exfat {#read-blue-usba-usbc-exfat}

/dev/sda1:
 Timing buffered disk reads: 140 MB in  3.07 seconds =  45.60 MB/sec


## READ red usba/usbc - exfat {#read-red-usba-usbc-exfat}

/dev/sda1:
 Timing buffered disk reads: 312 MB in  3.01 seconds = 103.54 MB/sec


## red hard drive usba {#red-hard-drive-usba}

/dev/sda1:
 Timing buffered disk reads: 348 MB in  3.01 seconds = 115.77 MB/sec


## samsung T7Touch {#samsung-t7touch}

/dev/sda1:
 Timing buffered disk reads: 1230 MB in  3.06 seconds = 401.47 MB/sec


## TV Disk {#tv-disk}

/dev/sda1:
 Timing buffered disk reads: 306 MB in  3.01 seconds = 101.52 MB/sec
