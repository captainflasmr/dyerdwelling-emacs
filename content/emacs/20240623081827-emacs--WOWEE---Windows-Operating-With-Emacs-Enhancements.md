---
title: "WOWEE - Windows Operating With Emacs Enhancements"
author: ["James Dyer"]
lastmod: 2024-06-24T21:00:00+01:00
tags: ["wowee", "emacs", 2024]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20240623081827-emacs--WOWEE---Windows-Operating-With-Emacs-Enhancements.jpg"
---

My Emacs usage on Windows seems to be more prevalent at the moment, and I'm having to drop into Visual Studio for C# development.

<!--more-->

Emacs keybindings are so ingrained into my muscle memory that it would be rather pleasant to leverage this digital advantage when plonking around Windows for all applications.

In Visual Studio this apparently, like most IDE's, can be accomplished via a plug-in, but I think I would like a more wholistic versatile approach, by for example, having Emacs keybindings applied at a global Windows level.

So how do I accomplish this?, I prodded the interwebs and firstly came across **XKeymacs**:

> XKeymacs provides key bindings like Emacs for applications running
> on Microsoft Windows. You can also configure bindings for each
> application.

On running the binary it looks very comprehensive and has a nice intuitive GUI.  However I would like a project I could get my teeth into and I was struggling to build it in a modern version of Visual Studio, this may be something I'll have another look at in the future.  Possibly though my quick abandonment of this idea was more related to an idea I had regarding AutoHotKey (a scripting language which can provide easy keyboard shortcut mapping) and a language that I am already familiar with.

As it turns out there is already an AutoHotKey project that suits my needs and that is **EWOW -- Emacs Way of Operating Windows**, it is defined as follows:

> -   allows Emacs-like commands and keybinds (almost) everywhere in
>     Windows
>     -   keyboard macros
>
>     -   prefix digit-argument
>         -   ex. C-3 C-n -&gt; go 3 lines down
>
>     -   Emacs-style region selection (i.e. set-mark-command)
>
>     -   ... etc

It is essentially a set of AutoHotkey scripts and my AHK familiarity could mean this to be an ideal project to take a closer look at and to have a tinker around.

This project also seems to be well thought out and comprehensive and even provides bonus functionality that I wouldn't have expected, like hooks and even macros!!

It is written however in AutoHotkey V1 and although still currently available it is deprecated and v2 is considered now the main supported version.  V2 is a significant rewrite of the the AHK syntax and to convert EWOW from v1 to v2 might take some significant effort.  I had an initial look to see what was involved and used it as an exercise to start to understand both the magnitude of the porting task and to get familiar generally with EWOW.  More than anything though this activity surfaced an idea for a new project aimed at implementing Emacs-like commands on Windows, focusing especially on the commands I find myself wanting to use most frequently.

My initial idea is to lean heavily on EWOW by taking the basic structure and design principles which could accommodate some future augmentation, such as hooks, macros e.t.c and to declare a form of AHK bankruptcy and start almost from scratch and progress from an AHK V2 base.  Initially I will try and add the most common simple commands using EWOW as a guide and then build up over a period of time to something with more bells on it.

I have created a new project with a nod to the name of EWOW, it is defined below and I will look to gradually introduce more Emacs functionality over the next few months and especially focussing on those commands I keep reaching for when faffing around with Visual Studio.

I have also added a kanban board (using org-kanban, which I will discuss more in a future post) that lists all the EWOW implemented key functions and indicates which ones have been ported. As I become more familiar with this project, I will start to define its scope more clearly. This will likely include deciding on the functionality I choose not to implement. I would like to focus as much as possible on simplicity, with an ergonomic approach in mind. For example, I'm considering options such as mapping Caps and RAlt to Control, providing the option for a Vim-style keybinding, and the ability to create presets through textual modifications of AHK files.

---


## WOWEE - Windows Operating With Emacs Enhancements {#wowee-windows-operating-with-emacs-enhancements}

{{< figure src="/emacs/20240623081827-emacs--WOWEE---Windows-Operating-With-Emacs-Enhancements.jpg" width="100%" >}}

<https://github.com/captainflasmr/wowee>

WOWEE is a set of AutoHotKey scripts designed to bring Emacs-like commands and keybindings to the Windows operating system. Based on the concept of EWOW (Emacs Way of Operating Windows), WOWEE allows you to use Emacs-style navigation and commands throughout your Windows environment.


## Features {#features}

-   Emacs-like commands and keybindings in Windows
-   Based on EWOW – Emacs Way of Operating Windows
-   Written in AutoHotKey v2
-   Includes various Emacs style navigation commands


### kanban {#kanban}

| TODO                        | DOING                   | DONE                   |
|-----------------------------|-------------------------|------------------------|
| scroll_left                 | jumping around commands | motion commands        |
| scroll_right                |                         | forward_char           |
| goto_line                   |                         | backward_char          |
| region commands             |                         | forward_word           |
| mark_word                   |                         | backward_word          |
| mark_whole_line             |                         | next_line              |
| mark_whole_buffer           |                         | previous_line          |
| kill_region                 |                         | scroll_down            |
| yank_pop                    |                         | scroll_up              |
| delete_backward_char        |                         | move_beginning_of_line |
| kill_word                   |                         | move_end_of_line       |
| backward_kill_word          |                         | beginning_of_buffer    |
| kill_whole_line             |                         | end_of_buffer          |
| newline and indent commands |                         | kill_ring_save         |
| newline                     |                         | yank                   |
| open_line                   |                         | delete_char            |
| indent_for_tab_command      |                         | kill_line              |
| delete_indentation          |                         | undo_only              |
| edit commands               |                         | set_mark_command       |
| redo                        |                         | set_cx_command         |
| transpose_chars             |                         | keyboard_quit          |
| transpose_words             |                         | save_buffer            |
| transpose_lines             |                         |                        |
| query_replace               |                         |                        |
| search_forward              |                         |                        |
| overwrite_mode              |                         |                        |
| case conversion commands    |                         |                        |
| upcase_region               |                         |                        |
| downcase_region             |                         |                        |
| upcase_word                 |                         |                        |
| downcase_word               |                         |                        |
| capitalize_word             |                         |                        |
| insert pairs commands       |                         |                        |
| insert_parentheses          |                         |                        |
| insert_comment              |                         |                        |
| indent_new_comment_line     |                         |                        |
| other commands              |                         |                        |
| shell                       |                         |                        |
| shell_command               |                         |                        |
| facemenu                    |                         |                        |
| help                        |                         |                        |
| system commands             |                         |                        |
| ignore                      |                         |                        |
| repeat                      |                         |                        |
| digit argument commands     |                         |                        |
| macro recording commands    |                         |                        |
| files commands              |                         |                        |
| write_file                  |                         |                        |
| find_file                   |                         |                        |
| dired                       |                         |                        |
| windows frames commands     |                         |                        |
| kill_frame                  |                         |                        |
| delete_window               |                         |                        |
| split_window                |                         |                        |
| next_window                 |                         |                        |
| previous_window             |                         |                        |
| suspend_frame               |                         |                        |
| add hooks                   |                         |                        |
| add ignore frames           |                         |                        |
| add goto line               |                         |                        |
| add kill ring               |                         |                        |
| add mouse events            |                         |                        |
| add C-x                     |                         |                        |


## Usage {#usage}

1.  ****Install AutoHotKey****: Download and install AutoHotKey from [AutoHotKey's official website](<https://www.autohotkey.com/>).
2.  ****Run WOWEE****: Double-click on the \`wowee.ahk\` script to start WOWEE. Once running, Emacs commands will be available in your Windows environment.
3.  ****Quit WOWEE****: To quit WOWEE, right-click the AutoHotKey icon in the task tray and select "Exit."


## Configuration {#configuration}

WOWEE is composed of several AutoHotKey scripts, each serving a specific purpose to replicate Emacs functionalities:


### fundamental.ahk {#fundamental-dot-ahk}

This script provides a set of fundamental functions and variables that are used to implement Emacs-like commands and keybindings.


### commands.ahk {#commands-dot-ahk}

This script includes the basic implementation of Emacs commands.


### commands_util.ahk {#commands-util-dot-ahk}

This script contains simple utility functions used by the command scripts.


### keybinds.ahk {#keybinds-dot-ahk}

This script defines the default keybindings for Emacs-like commands.


## Installation and Setup {#installation-and-setup}

1.  ****Download WOWEE****: Download the WOWEE scripts from the repository.
2.  ****Extract Files****: Extract the files to a directory of your choice.
3.  ****Run the Script****: Double-click \`wowee.ahk\` to start using WOWEE.


## Customization {#customization}


### Editing Keybindings {#editing-keybindings}

You can customize the keybindings by editing the \`keybinds.ahk\` file. Open the file in any text editor and modify the keybindings according to your preferences. Refer to the AutoHotKey documentation for the syntax and available key options.


### Adding New Commands {#adding-new-commands}

To add new commands, you can edit the \`commands.ahk\` and \`commands_util.ahk\` files. Define your new commands and utility functions, and then bind them to keys in \`keybinds.ahk\`.


## Troubleshooting {#troubleshooting}

If you encounter any issues while using WOWEE, try the following steps:

1.  ****Check AutoHotKey Version****: Ensure you have the latest version of AutoHotKey installed.
2.  ****Script Errors****: If there are errors in the script, AutoHotKey will usually display a message with details. Use this information to debug and fix the issue.
3.  ****Conflicting Programs****: Some programs might have conflicting keybindings. Try closing other programs to see if the issue is resolved.


## Contributing {#contributing}

Contributions to WOWEE are welcome! If you have suggestions for improvements or want to add new features, feel free to submit a pull request.

1.  Fork the repository
2.  Create your feature branch (\`git checkout -b feature/YourFeature\`)
3.  Commit your changes (\`git commit -am 'Add your feature'\`)
4.  Push to the branch (\`git push origin feature/YourFeature\`)
5.  Create a new pull request


## License {#license}

WOWEE is licensed under the MIT License. See the LICENSE file for more details.


## Acknowledgements {#acknowledgements}

Special thanks to the creator of EWOW, from whom I have derived significant inspiration: <https://github.com/zk-phi/ewow>

Special thanks to the creators of AutoHotKey and the Emacs community for their inspiration and contributions to keyboard efficiency.


## Contact {#contact}

For any questions or issues, please open an issue on the GitHub repository or contact the maintainer at captainflasmr@gmail.com

---

Enjoy using WOWEE and bring the power of Emacs navigation to your Windows experience!
