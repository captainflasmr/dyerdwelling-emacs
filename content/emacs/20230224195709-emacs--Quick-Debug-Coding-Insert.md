---
title: "Using Macros to Help Code Debugging"
author: ["James Dyer"]
lastmod: 2023-03-02T20:08:00+00:00
tags: ["macros", "emacs", 2023]
categories: ["emacs", "linux"]
draft: false
thumbnail: "/emacs/20230224195709-emacs--Quick-Debug-Coding-Insert.jpg"
---

Often I find myself requiring the most rudimentary of methods of debugging and that is to push text to standard output from within a running executable to indicate the logical structure of a program.

<!--more-->

{{< figure src="/emacs/20230224195709-emacs--Quick-Debug-Coding-Insert.jpg" class="emacs-img" >}}

This situation may come about if I can't debug a dll or generally actually stopping a program to debug creates an **"observer effect"**

Sometimes the logical structure can be somewhat complex consisting of a plethora of `if` statements and nested `for` loops, e.t.c and may take me a tedious while to apply the necessary debug statements.  But emacs macros can speed up this process drastically!

I often code in Ada and the text to `stdout` mechanism is generally something like:

`ada.text_io.put_line("Yes Ada is a language!");`

My macro starts with a simple search for blank lines using `C-M-s` `(isearch-forward-regexp)` for the regex **`^$`**

Next I would like to have a unique string for each debug statement and this can be accomplished by using `(kmacro-insert-counter)` via `C-x C-k C-i`

For example to debug the following dummy code:

```nil
with Ada.Text_Io;

procedure Ada_Main is
   Ftt : Boolean := True;
   Count : Integer := 0;

   procedure Call_Routine is
   begin
      null;
   end Call_Routine;

   procedure Call_Another_Routine is
   begin
      null;
   end Call_Another_Routine;

begin
   Ada.Text_Io.Put_Line("Program Starting");

   if Ftt then
      Call_Routine;
   else
      Call_Another_Routine;
   end if;

   while Count < 3 loop

      Call_Routine;

      Count := Count + 1;

   end loop;

   Ada.Text_Io.Put_Line("Program Finished");

end Ada_Main;
```

which just outputs the following:

```nil
Program Starting
Program Finished
```

To debugify this simple program, I can record a macro and perform the following steps:

1.  perform **(isearch-forward-regexp)** for `^$`
2.  type in my debug output statement - in this case an Ada one
3.  insert an incremented counter
4.  complete the debug statement.

On a macro repeat I will now have a unique text_io for each blank line giving me an indication of the code path.  I can kick off my macro from any point in the program and stop at any time hence below:

```nil
with Ada.Text_Io;

procedure Ada_Main is
   Ftt : Boolean := True;
   Count : Integer := 0;

   procedure Call_Routine is
   begin
      null;
   end Call_Routine;

   procedure Call_Another_Routine is
   begin
      null;
   end Call_Another_Routine;

begin
   Ada.Text_Io.Put_Line("Program Starting");
Ada.Text_Io.Put_Line("##1");
   if Ftt then
      Call_Routine;
   else
      Call_Another_Routine;
   end if;
Ada.Text_Io.Put_Line("##2");
   while Count < 3 loop
Ada.Text_Io.Put_Line("##3");
      Call_Routine;
Ada.Text_Io.Put_Line("##4");
      Count := Count + 1;
Ada.Text_Io.Put_Line("##5");
   end loop;
Ada.Text_Io.Put_Line("##6");
   Ada.Text_Io.Put_Line("Program Finished");
Ada.Text_Io.Put_Line("##7");
end Ada_Main;
```

and running now gives me:

```nil
Program Starting
##1
##2
##3
##4
##5
##3
##4
##5
##3
##4
##5
##6
Program Finished
##7
```

Which may help me to figure out what is going wrong with my program (although in this case nothing was really going wrong!)

For large complicated source code to have such a fast mechanism such as this can be very useful and it is amazing at just how quickly a program fault can be identified by using this simple and almost primordial method.

This of course could be improved in many different ways, for example to not rely on blank lines and possibly force insert debug statements based on semi colons or maybe to decipher the syntactical structure (tree sitter?)  and insert the debug statements in a more coherent and comprehensive manner.  But this is only a rudimentary quick method to roughly locate a programs issues and it is good enough for me.
