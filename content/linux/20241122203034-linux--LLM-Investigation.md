---
title: "LLM-Investigation"
author: ["James Dyer"]
lastmod: 2024-11-22T20:30:00+00:00
tags: [2024]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20241122203034-emacs--LLM-Investigation.jpg"
---

## Useful resources {#useful-resources}

<https://medium.com/@genebernardin/a-new-easy-desktop-ui-for-running-open-source-llms-jan-18edc9f3f2c1>


## Models {#models}

Generally for the moment the file size is based on the maximum file size that can be downloaded on GLK.


### qwen2.5-coder-7b-instruct-q5_k_m.gguf {#qwen2-dot-5-coder-7b-instruct-q5-k-m-dot-gguf}

<https://huggingface.co/Qwen/Qwen2.5-Coder-7B-Instruct-GGUF/tree/main>

5.1 G

The best model I have found so far, balancing accuracy and speed, is quite comparable to chatgpt regarding coding


### codellama-7b.Q5_K_M.gguf {#codellama-7b-dot-q5-k-m-dot-gguf}

<https://huggingface.co/TheBloke/CodeLlama-7B-GGUF/tree/main>

4.5 G

Generally recommended but haven't quite managed to get any decent accuracy or performance as of yet.


### Llama-3.2-1B-Instruct-Q8_0.gguf {#llama-3-dot-2-1b-instruct-q8-0-dot-gguf}

<https://huggingface.co/hugging-quants/Llama-3.2-1B-Instruct-Q8_0-GGUF/tree/main>

1.3 G

Very fast heavily quantized version for testing, very inaccurate for coding tasks.


### Notes {#notes}

Several things to note:

1.  The “Q” indicates the amount of compression the version has (#1 below). Think of this like a RAW digital photograph that has been compressed into a .jpg file. The resulting image lacks some detail, but you can hardly notice, and the compression saves a lot of space. The “Q” number indicates the amount of quantization that has occurred (i.e. 2-bit, 3-bit, 4-bit, etc.). The lower the bit number, the higher the quantization. This results in a smaller model with faster inference but potentially reduced accuracy. The higher the bit number, the larger the model and better the accuracy but slower inference (time from prompt to answer).

2.  K and 0 represent quantization methods and are beyond the scope of this article.

3.  The “S”, “M”, and “L”, letters before the .gguf file format designation (#2), stand for small, medium and large. You can see after the file name the amount of space the file will require on your computer (#3).

Generally, my experience is that Q4 or Q5 models perform well for me. They provide a good balance between speed and accuracy. To download one of the versions, just click on the download icon to the right of the model you intend to download (#4).


## prompts {#prompts}


### In Ada can you write a hello world program? {#in-ada-can-you-write-a-hello-world-program}


#### expeected response {#expeected-response}

```ada
with Ada.Text_IO;

procedure main is
begin
   Ada.Text_IO.Put_Line("Hello World!");
end main;
```


### In elisp can you write a hello world program? {#in-elisp-can-you-write-a-hello-world-program}


#### expeected response {#expeected-response}

```elisp
(message "Hello World!")
```


### ﻿In Ada can you generate a template specification file for pragma importing c++ routines? {#in-ada-can-you-generate-a-template-specification-file-for-pragma-importing-c-plus-plus-routines}


#### expected response (advanced) {#expected-response--advanced}

```nil
with Interfaces.C; use Interfaces.C;
with System.OS_Lib; use System.OS_Lib;

procedure Template_Specification is

   -- Define the type for the C++ function parameters and return value
   type Cpp_Function_Type is access function (Param1 : in chars_ptr) return int;

   pragma Import(C, cpp_function, "cpp_function");

   -- Call the imported C++ function from Ada
   procedure Call_Cpp_Function(Param1 : String) is
      Param1_C : constant chars_ptr := New_String(Param1);
      Result : Integer;
   begin
      Result := cpp_function(Param1_C);
      Free(Param1_C);  -- Don't forget to free the allocated string

      Put_Line("C++ function returned: " & Integer'Image(Result));
   end Call_Cpp_Function;

begin
   -- Example usage of the imported C++ function
   Call_Cpp_Function("Hello from Ada!");
end Template_Specification;
```


#### expected response (standard) {#expected-response--standard}

1.  **Create a C++ header file (`example.h`)**:
    ```cpp
          #ifndef EXAMPLE_H
          #define EXAMPLE_H

          extern "C" {
              void print_hello_world();
          }

          #endif // EXAMPLE_H
    ```

2.  **Implement the C++ function (`example.cpp`)**:
    ```cpp
          #include <iostream>

          extern "C" {
              void print/hello/world() {
                  std::cout << "Hello, World!" << std::endl;
              }
          }
    ```

3.  **Create an Ada binding file (`example.ads`)**:
    ```ada
          with Interfaces.C; use Interfaces.C;

          package Example is
              procedure Print_Hello_World; pragma Import(C, Print_Hello_World, "print_hello_world");
          end Example;
    ```

4.  **Write the main Ada program (`main.adb`)**:
    ```ada
          with Example;
          procedure Main is
          begin
              Example.Print_Hello_World;
          end Main;
    ```

5.  **Compile and link the C++ and Ada files**:
    -   First, compile the C++ file to create an object file:
        ```sh
                  g++ -c example.cpp -o example.o
        ```
    -   Then, compile the Ada file and link it with the C++ object file:
        ```sh
                  gnatmake main.adb example.o
        ```

<!--list-separator-->

-  ﻿Can you demonstrate the mapping of an Ada data structure to that of C++ when going over interop?

    <!--list-separator-->

    -  expected response

        **Ada Code**

        ```ada

        with Interfaces.C; use Interfaces.C;
        with System.OS_Lib; use System.OS_Lib;

        procedure Data_Structure_Mapping is

           -- Define an Ada record type
           type Person_Ada is record
              Name  : chars_ptr := New_String("Unknown");
              Age   : Integer := 0;
           end record;

           pragma Import(C, Person_Cpp, "Person");

           -- C++ struct definition (assuming this is in a header file)
           -- struct Person {
           --     char* name;
           --     int age;
           -- };

           type Person_Ada access all Person_Ada;

           procedure Call_Cpp_Function(P : in out Person_Ada) is
              pragma Import(C, cpp_function, "cpp_function");
           begin
              cpp_function(P);
           end Call_Cpp_Function;

        begin
           -- Create an instance of the Ada record
           declare
              P : aliased Person_Ada := (Name => New_String("Alice"), Age => 30);
           begin
              -- Call the C++ function with the Ada record
              Call_Cpp_Function(P'Access);

              -- Free allocated memory for the name string
              Free(P.Name);
           end;
        end Data_Structure_Mapping;
        ```

        **cpp**

        ```cpp

        #include <iostream>
        #include <cstring>

        // Define the C++ struct
        struct Person {
            char* name;
            int age;
        };

        extern "C" void cpp_function(Person* p) {
            // Print the contents of the Person structure
            std::cout << "Name: " << p->name << ", Age: " << p->age << std::endl;

            // Modify the contents (optional)
            strcpy(p->name, "Bob");
            p->age = 25;
        }
        ```

<!--list-separator-->

-  In C# how would I read in a json file?

    In C#, reading a JSON file can be done using the `System.Text.Json` library, which provides classes like `JsonSerializer` to work with JSON. Here's a step-by-step guide on how to do it:

    <!--list-separator-->

    -  expected response

        ```csharp

        using System;
        using System.IO;
        using System.Text.Json;

        class Program
        {
            static void Main(string[] args)
            {
                string filePath = "data.json"; // Replace with your file path

                try
                {
                    // Read the JSON file into a string
                    string jsonString = File.ReadAllText(filePath);

                    // Deserialize the JSON into a C# object
                    var data = JsonSerializer.Deserialize<MyData>(jsonString);

                    // Use the data
                    Console.WriteLine($"Name: {data.Name}");
                    Console.WriteLine($"Age: {data.Age}");
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"An error occurred: {ex.Message}");
                }
            }
        }

        // Define a class that matches the JSON structure
        public class MyData
        {
            public string Name { get; set; }
            public int Age { get; set; }
        }
        ```

<!--list-separator-->

-  Can you tell me about swig?

    <!--list-separator-->

    -  expected response

        SWIG (Simplified Wrapper and Interface Generator) is a powerful tool for connecting C or C++ code with various high-level programming languages. It is primarily used to generate the "glue" code that allows you to call native C/C++ functions and use native C/C++ data structures in other languages like Python, Java, C#, Ruby, and more.

        <!--list-separator-->

        -  Key Features of SWIG:

            1.  ****Language Binding****: It creates bindings between C/C++ code and high-level languages, making native code accessible without rewriting it in the target language.

            2.  ****Cross-Platform Support****: SWIG works on many platforms and supports multiple target languages, allowing developers to create bindings for different ecosystems from a single codebase.

            3.  ****Automatic Wrapper Generation****: By parsing C/C++ header files, SWIG can automatically generate the necessary wrapper code to expose the functions and classes.

            4.  ****Customizability****: SWIG provides directives and configuration options to customize how the bindings are generated, allowing you to fine-tune the integration.

            ---

        <!--list-separator-->

        -  How SWIG Works:

            1.  ****Interface File****: You create an interface file (e.g., `example.i`) that describes the C/C++ code to expose.
            2.  ****Processing****: SWIG processes this file and generates wrapper code (in C or C++) for the target language.
            3.  ****Compilation****: The generated wrapper code is compiled into a shared library (or equivalent for the target language).
            4.  ****Usage****: The target language can then load the library and interact with the C/C++ code.

        <!--list-separator-->

        -  Supported Languages:

            SWIG supports many languages, including:

            -   ****Scripting Languages****: Python, Perl, Tcl, Ruby, PHP, Lua.
            -   ****System/Statically Typed Languages****: Java, C#, Go, D, R.
            -   ****Mathematical/Scientific Platforms****: Octave, Scilab, MATLAB.

        <!--list-separator-->

        -  Benefits:

            -   Simplifies the process of integrating C/C++ with other languages.
            -   Saves time by automating wrapper generation.
            -   Offers robust support for object-oriented C++ features like classes and inheritance.

        <!--list-separator-->

        -  Challenges:

            -   Generated wrappers can sometimes be complex and hard to debug.
            -   Requires careful management of memory (e.g., when passing pointers between languages).
            -   Performance overhead due to wrappers.

        <!--list-separator-->

        -  When to Use SWIG:

            -   When you need to reuse existing C/C++ libraries in high-level languages.
            -   For projects requiring cross-language compatibility without rewriting code.

        <!--list-separator-->

        -  Alternatives:

            -   ****Manually writing bindings**** (more tedious).
            -   Other tools like ****Boost.Python**** (for Python) or ****pybind11**** (specific to Python bindings). These can be more Python-focused but lack the multi-language support of SWIG.

        <!--list-separator-->

        -  Conclusion:

            SWIG is a versatile and powerful tool for creating language bindings for C/C++ projects. It is widely used in both industry and academia for extending functionality, leveraging native performance, and integrating legacy codebases.

<!--list-separator-->

-  Can you refactor the following code:

    <!--list-separator-->

    -  expected response

        ```csharp

          static void ReadConfig()
                {
                    XmlDocument doc = new XmlDocument();
                    doc.Load("MiniHost.def");
                    XmlNode bnode = doc.SelectSingleNode("MiniHostInitialization");

                    // Default values
                    Port_H2IG = 8000;
                    Port_IG2H = 8001;
                    IGAddr = "127.0.0.1";

                    if (bnode != null)
                    {
                        XmlNode configNode = bnode.SelectSingleNode("Config");
                        if (configNode != null)
                        {
                            IGAddr = configNode["IG_Addr"]?.InnerText ?? IGAddr;
                            Port_H2IG = int.Parse(configNode["Port_To_IG"]?.InnerText ?? "8003");
                            Port_IG2H = int.Parse(configNode["Port_From_IG"]?.InnerText ?? "8004");
                        }

                        foreach (XmlNode dbNode in bnode.SelectNodes("DBase"))
                        {
                            DbInfo dbInfo = new DbInfo
                            {
                                Id = int.Parse(dbNode["ID"]?.InnerText ?? "0"),
                                Lat = double.Parse(dbNode["Lat"]?.InnerText ?? "0"),
                                Lon = double.Parse(dbNode["Lon"]?.InnerText ?? "0"),
                                Alt = double.Parse(dbNode["Alt"]?.InnerText ?? "0")
                            };

                            dbList.Add(dbInfo);
                        }
                    }
                }
        ```

    <!--list-separator-->

    -  optimizations

        ollama :

        ```bash
        export CUDA_VISIBLE_DEVICES=-1
        export OLLAMA_FLASH_ATTENTION=1
        ```

<!--list-separator-->

-  Installation

    <!--list-separator-->

    -  Jan

        <https://jan.ai/>

        <!--list-separator-->

        -  Windows

            jan-win-x64-0.5.9.exe

        <!--list-separator-->

        -  Linux

            jan-linux-x86_64-0.5.9.AppImage

    <!--list-separator-->

    -  Ollama

        <https://ollama.com/download>

        <!--list-separator-->

        -  Windows

            OllamaSetup.exe

        <!--list-separator-->

        -  Linux

            <https://www.rahuljuliato.com/posts/ellama>
            <https://github.com/ollama/ollama/blob/main/docs/linux.md>
            <https://github.com/ollama/ollama/releases/tag/v0.4.4>

            ollama-linux-amd64.tgz

            sudo tar -C /usr -xzf ollama-linux-amd64.tgz

            <!--list-separator-->

            -  Running

                First always run the following:

                ```nil
                ollama serve &
                ```

            <!--list-separator-->

            -  Create a Modelfile

                Modelfile is the blueprint that Ollama uses to create and run models. Since we're working with a LLaMa 3.1 variant, we can base our Modelfile on an existing one:

                ollama show --modelfile llama3.1:70b-instruct-q4_0 &gt;&gt;  Modelfile

                This command generates a Modelfile based on the llama3.1 model specifications which I already had locally pulled.

                If you don't have example of existing Modelfile to reuse then you would need to figure it out from the Hugging-Face page for the model and then create one.

                To import a GGUF model, create a Modelfile containing:

                ```nil
                FROM /path/to/file.gguf
                ```

                Once you have created your Modelfile, use the ollama create command to build the model.

                The Modelfile can be created anywhere.

                ```nil
                ollama create <your-model-name-here> -f <full path to Modelfile>
                ```

            <!--list-separator-->

            -  Import Model

                ```nil
                ~/DCIM/content $ ollama create james-test -f /home/jdyer/.config/ollama/Modelfile
                ```

                This command processes the Modelfile and copies the model to Ollama's storage, typically located at _usr/share/ollama_.ollama or ~/.ollama

                Mainly copies the gguf into the blobs directory.

                How do we just simple symbolic link to it?

            <!--list-separator-->

            -  Query

                ```nil
                ollama list
                NAME                 ID              SIZE      MODIFIED
                qwen:latest          1dd4f9a26d56    12 GB     4 minutes ago
                james-test:latest    6bd474fa5b3b    1.3 GB    13 minutes ago
                ```

            <!--list-separator-->

            -  Run

                ```nil
                ollama run <name>
                ```

    <!--list-separator-->

    -  gpt4all

        <https://www.nomic.ai/gpt4all>

        <!--list-separator-->

        -  Windows

            gpt4all-installer-win64.exe

        <!--list-separator-->

        -  Linux

            QT_QPA_PLATFORM=xcb ./gpt4all-installer-linux.run

<!--list-separator-->

-  IDE Setup

    <!--list-separator-->

    -  Emacs

        <!--list-separator-->

        -  Ellama

        <!--list-separator-->

        -  chatgpt-shell

        <!--list-separator-->

        -  gpt.el

    <!--list-separator-->

    -  VSCode
