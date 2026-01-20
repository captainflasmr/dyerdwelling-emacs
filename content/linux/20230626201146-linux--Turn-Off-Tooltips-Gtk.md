---
title: "Turn-Off-Tooltips-Gtk"
author: ["James Dyer"]
lastmod: 2023-06-26
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230626201146-emacs--Turn-Off-Tooltips-Gtk.jpg"
---

try this in various directories:
~/.config/gtk-3.0/gtk.css
~/.config/gtk-4.0/gtk.css

tooltip {
    opacity: 0;
}
