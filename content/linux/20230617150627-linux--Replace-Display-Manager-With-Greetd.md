---
title: "Replace-Display-Manager-With-Greetd"
author: ["James Dyer"]
lastmod: 2023-06-17
tags: [2023]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20230617150627-emacs--Replace-Display-Manager-With-Greetd.jpg"
---

Install `greetd` which is a general simple login backend, with SDDM and GDM you can't always guarantee they will be particularly compatible with Wayland, the default install into `/etc/greetd/config.toml` will use the default installed `agreety` which is simply just a terminal login like getty and the following is fine ready to run your own command:

```bash
command = "agreety --cmd /bin/sh"
```

make sure the user is set correctly as greeter is the default.

```nil
user = "jdyer"
```

To run sway run:

```bash
zsh --login -c sway
```

note the zsh to make sure the .zprofile is executed correctly which is something a full login display manager would set automatically.

To start X11 sessions:

```nil
startx kde
startx i3
```

although currently I am having things freeze up a little

modified /etc/greetd/config.toml to the following for autologin:

```toml
[terminal]
# The VT to run the greeter on. Can be "next", "current" or a number
# designating the VT.
vt = 1

[general]

source_profile = true

# The default session, also known as the greeter.
[default_session]

# `agreety` is the bundled agetty/login-lookalike. You can replace `/bin/sh`
# with whatever you want started, such as `sway`.
command = "agreety --cmd /bin/sh"
# command = "agreety --cmd sway"
# command = "gtkgreet -c sway"

# The user to run the command as. The privileges this user must have depends
# on the greeter. A graphical greeter may for example require the user to be
# in the `video` group.
user = "jdyer"

[initial_session]

command = "zsh --login -c sway"

user = "jdyer"
```
