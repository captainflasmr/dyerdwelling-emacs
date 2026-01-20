---
title: "How to Display Google Calendar"
author: ["James Dyer"]
lastmod: 2023-02-04T13:20:00+00:00
tags: ["emacs", "elisp", "calendar", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/0230204115142-emacs--Displaying-A-Calendar/2023-02-03_12-59.jpg"
---

Emacs is subsuming me!  I have managed to get email up and running using **mu4e** and rss using **elfeed** and image viewing with **image-dired**

<!--more-->

Next up is some form of calendar integration!

Although I use Google Calendar I don't rely on any google apps directly; an android app called **Simple Calendar** is adequate for my needs and enables the thing I cherish most of all (except emacs of course!)  and that is the ability to produce an offline copy.  It can export to an `ics` file, and I have developed a habit of exporting to an offline file every time I update my calendar.  This means that I have some exported calendar files lying around, including of course the most recent one.

Initially I attempted to use **org-gcal** which potentially enables a two way communication with Google Calendar through their API, but I got lost in the setup and of course this is Google, at some stage it is likely they will either change their API / remove it / or charge for it.

Anyway, do I really need to modify my calendar from within emacs?, I am quite comfortable using **Simple Calendar** on my phone and if I really need to add calendar entries from my laptop then I always have **Thunderbird** as a backup.

So I may just be in a very fortunate position regarding an adequate level of emacs calendar integration :

1.  I only require read only
2.  I have an up-to-date `ics` file available

I created the following function which opens up a nicely formatted calendar using the packages **calfw** and **calfw-cal** (and they didn't require any additional setup).

```elisp
(defun my/calendar ()
  (interactive)
  (setq tmp-file (concat home-dir "/DCIM/Backup/tmp.org"))
  (delete-file tmp-file)
  (when (get-buffer (file-name-nondirectory tmp-file))
    (kill-buffer (file-name-nondirectory tmp-file)))
  (setq last-ics
        (car (directory-files
              (concat home-dir "/DCIM/Backup")
              'full "\.ics$" #'file-newer-than-file-p)))
  (icalendar-import-file last-ics tmp-file)
  (cfw:open-diary-calendar)
  (when (get-buffer (file-name-nondirectory last-ics))
    (kill-buffer (file-name-nondirectory last-ics))))
```

and produces a calendar of the form:

{{< figure src="/emacs/0230204115142-emacs--Displaying-A-Calendar/2023-02-03_12-59.jpg" width="300px" >}}

I could import everything directly into my `diary-file` but I decided to be a little more flexible and potentially allow for multiple calendars by setting an `include` directive in my diary file as thus:

```nil
#include "~/DCIM/Backup/tmp.org"
```

The key to the `my/calendar` function is to pull in the most recent `ics` file and call the built in `icalendar-import-file` to convert / import the `ics` data into my diary file.  The **cfw** packages will then take care of the rest.  The other parts of the function are just tidying up various buffers and files to make things a little cleaner.

One little wrinkle I discovered and didn't necessary solve to a satisfactory level was when I first call up `cfw:open-diary-calendar` the calendar didn't resize correctly to fit the window, a second call however seemed to fix this and with no overhead.

One final addition was the following:

```elisp
(add-to-list 'display-buffer-alist
             `(,(rx(or "Calendar"))
               display-buffer-in-direction
               (direction . right)
               (dedicated . t)
               (window . root)
               (window-width . 80)))
```

Which gives me greater control of where the calendar window is opened
