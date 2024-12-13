![Logo](http://software.rochus-keller.ch/luon_logo_200x130.png)

## Welcome to the Luon Programming Language

Luon is a high-level programming language with a syntax similar to [Oberon+](https://github.com/rochus-keller/oberon/), Oberon-07 and Oberon-2, integrating concepts from Lua, and targeting the LuaJIT VM. Luon can be regarded as a statically typed version of Lua. The name is thus a combination of "Lua" and "Oberon". 

Luon procedures can be declared "external" and be implemented in Lua. This allows the re-use of libraries written in Lua, and also C libraries via 
the LuaJIT foreign function interface.

The language specification can be found in the [specification subdirectory](https://github.com/rochus-keller/Luon/tree/master/specification).
See also the code examples below.

The project is the result of the author's experience with the [Smalltalk-80](https://github.com/rochus-keller/Smalltalk/) and [SOM VM](https://github.com/rochus-keller/Som/) implementation, and the prospect to build a [LuaJIT based Interlisp VM](https://github.com/rochus-keller/Gingko/). 

In contrast to Oberon+, Luon doesn't have pointers, but instead all structured datatypes have reference semantics and are dynamically created. In addition to the ARRAY type, there is also a HASHMAP type for compatibility with Lua. There is also a STRING data type which - like Lua - is immutable and follows a value semantics. 

In particular, Luon solves the following Lua shortcomings recognized during the aforementioned projects:

- conditional compilation to enable or disable statements only used for debugging and to avoid wasting calculation time 
- constants not requiring local slots or hashed element access
- explicit inline declaration, so the code can be better structured without additional context 
  switches and slot consumption
- no implicit global declarations, and other means to detect as many errors as possible during compile time
- locals can no longer be used before declaration
- switch/case control statement to avoid writing the relation expression all over again

This project implements a compiler and IDE for the Luon programming language. Representative example projects demonstrating the
capabilities of the language and the IDE can be found in the [testcases subdirectory](https://github.com/rochus-keller/Luon/tree/master/testcases/). 

Here is a screenshot of the IDE:

![IDE Screenshot 1](http://software.rochus-keller.ch/luon-ide-screenshot-0.7.0-1.png)

And here is the source-level debugger of the IDE:

![IDE Screenshot 2](http://software.rochus-keller.ch/luon-ide-screenshot-0.7.0-2.png)

### Code examples

##### Procedural Programming
```
module Fibonacci
  proc calc*(n : integer): integer
    var a, b, i, next: integer // comma is optional
  begin
   <* if use_recursion then *>
    if n > 1 then 
      a := calc(n - 1)
      b := calc(n - 2)
      return a + b
    elsif n = 0 then return 0
    else return 1
    end
   <* else *>
    b := 1
    for i := 0 to n-1 do
      next := a + b
      a := b
      b := next
    end
    return a
   <* end *>
  end calc
  var res: integer
begin
  res := calc(21)
  assert(res = 10946)
  print("Fibonaccy result: ")
  println(res)
end Fibonacci

```

##### Generic Programming
```
module Collections(T) 
  type Deque* = record data: array of T
                      size: integer end
                      
  proc createDeque*(): Deque 
    const initial_len = 1_000 // separators
    var this: Deque  // this is initialized to nil
  begin 
    new(this); 
    new(this.data,initial_len) 
    return this 
    // this and data will be garbage collected
  end createDeque
  
  proc (this: Deque) append*(element: T)
  begin 
    if this.size = len(this.data) then assert(false) end
    this.data[this.size] := element inc(this.size) 
  end append
  
  type Iterator* = record end
  proc (this: Iterator) apply*(element: T) end
  
  proc (this: Deque) forEach*(iter: Iterator)
    var i: integer; val: T
  begin 
    for i := 0 to this.size-1 do 
      iter.apply(this.data[i]) 
    end
  end forEach
end Collections
```

##### Object Oriented Programming
```
module Drawing
  import F := Fibonacci
         C := Collections(Figure)
  
  type Figure* = record position: record 
                     x,y: integer end end  
  proc (this: Figure) draw*() end
    
  type
    Circle* = record (Figure) diameter: integer end
    Square* = record (Figure) width: integer end 
    proc (this: Circle) draw*() end
    proc (this: Square) draw*() end
        
  var figures: C.Deque
      circle: Circle
      square: Square
    
  proc drawAll()
    type I = record(C.Iterator) count: integer end
    proc (this: I) apply( figure: Figure ) 
    begin 
      figure.draw(); inc(this.count) 
    end apply
    var i: I // count is initialized to zero
  begin
    new(i)
    figures.forEach(i)
    assert(i.count = 2)
    println("drawing done")
  end drawAll
  
begin 
  figures := C.createDeque()
  // use constructors instead of new:
  circle := { { F.calc(3), F.calc(4) }, diameter: 3 }
  figures.append(circle)
  square := { { x: F.calc(5), y: F.calc(6) }, 4 }
  figures.append(square)
  drawAll()
end Drawing  
```
### Planned features

- [x] Implement lexer with directive support
- [x] Implement parser and AST
- [x] Implement semantic validator 
- [x] Implement a LuaJIT backend based on LjTools
- [x] Implement an IDE similar to the Oberon+ IDE
- [x] Document the language (specification is available)
- [x] Migrate the Smalltalk-80 VM to Luon as a proof-of-concept and to optimize the language
- [x] BUSY build and precompiled versions for some platforms
- [ ] Migrate PAL2 from C to Lua, fix BitBlt, separate deployment
- [ ] Complete the language implementation according to the specification (inline, invar, pcall, visibility, etc.)

### Development history

##### Status on October 14, 2024

Derived the syntax from Oberon+ and Micron and generated a parser using [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Programming_Language.ebnf) and adopted the lexer from [the Micron project](https://github.com/rochus-keller/Micron). The parser is able to successfully read the ongoing Luon migration of the 
[Are-we-fast-yet benchmark suite](https://github.com/rochus-keller/Are-we-fast-yet/tree/main/Luon) (not yet commited).

##### Status on October 15, 2024

Adopted the AST infrastructure from Micron and extended the AST for full program coverage.
Implemented a recursive descent parser (modified the one generated  by [EbnfStudio](https://github.com/rochus-keller/EbnfStudio)) which generates the AST.

##### Status on October 16, 2024

Improved language: constructors generate object, extended for arrays and hashmaps; extra set constructor no longer needed. Can generate AST for ongoing AWFY with no memory leaks.

##### Status on October 25, 2024

The semantic validator is complete and tested. I developed it in parallel with [the Luon version of the Are-we-fast-yet benchmark suite](https://github.com/rochus-keller/Are-we-fast-yet/tree/main/Luon). Some refactorings and language changes were necessary. As in Oberon-07, there is now only one INTEGER and REAL type, the former with 53 bits of precision. The validator was feature complete on October 22, so development took six, and testing and debugging three calendar days.

##### Status on October 27, 2024

The IDE is ready for testing and debugging. I derived it from the LuaJIT version of the Oberon IDE, but many changes were necessary because the AST and code model are completely different. This included an optional cross-referencing infrastructure integrated with the validator. Also the import logic had to be extended for compatibility with the project file concept, which required refactoring of AST, parser and validator.

##### Status on November 05, 2024

The LuaJIT bytecode generator including most built-in procedures is complete and ready for testing and debugging. All functions of the LuonFfi.c file 
were migrated to LUON.lua to avoid C dependencies. Added Oakwood implementations in Lua (Files is no longer Oakwood compatible).

##### Status on November 09, 2024

After a lot of testing, debugging and fixes the generated code seems to be sufficiently correct for the moment to continue with other stuff. The testcases
in the all.lnpro project, which I migrated from Oberon+ ("features" subfolder), work, which covers most aspect of the language. I also 
implemented delegates and the copy() builtin, and made some other language changes (e.g. := no longer copies char arrays by value). Next goal is
to run the Luon Awfy suite.

##### Status on November 13, 2024

The Luon Are-we-fast-yet suite works up to Json; a significant refactoring of the code generator was necessary for this to properly intitialize all
class objects up-front; the performance is currently slightly (1%) better than Lua, and there is room for improvement. Now after a calendar month 
in the project, the compiler and IDE seem ready for implementing a Smalltalk VM, so let's go for it.

##### Status on November 26, 2024

All benchmarks of the Luon Are-we-fast-yet suite now work and show a performance equal to the Lua on LuaJIT version of the benchmark. The debugger has been significantly improved. The BYTE type and TOSTRING builtin have been added to the language.
The Project Oberon System has been migrated to Luon; the code works so far, but for correct display output many changes would be required (maybe in future).
The Smalltalk VM is work in progress and expected to be complete in a week. Both compiler and IDE are used to implement the Smalltalk VM and have proved to be useful so far.

##### Status on December 1. 2024

The migration of [the Luon implementation of the Smalltalk VM](https://github.com/rochus-keller/Smalltalk/blob/master/Smalltalk.lnpro) (see for *.luon and *.lnpro) is complete. I migrated the C++ version with a few architectural changes. In contrast to the C++ version which uses Qt, this version uses SDL2 via LuaJIT FFI. It is also an example how the EXTERN keyword works, i.e. how the external implementations are provided. Now the debugging starts; wish me luck ;-)

Meanwhile also [the Luon specification](https://github.com/rochus-keller/Luon/tree/master/specification) is available, though not all features are yet implemented.

##### Status on December 8. 2024

The ST-80 display representation works, but only with the C version of the BitBlt so far. Also added a Qt version of the PAL to ease debugging. Extended the language (KEYS function) and made other hasmap fixes. The ST VM requires still more debugging.

##### Status on December 10. 2024

The Smalltalk-80 VM implemented in Luon works (with a few issues WIP), with both the SDL2 and Qt based PAL. Added the source code of the VM as
a local testcase and demonstration to the Luon repository (as a copy of the corresponding files in the Smalltalk repository). 
The same applies to the Are-we-fast-yet implementation. The project is close to an MVP release.

##### Status on December 12. 2024

With only a few changes, the IDE and demo projects also work on Windows. Provided precompiled packages for a few platforms. LuaJIT is statically linked
on all platforms. MVP release.

### Precompiled versions

The following precompiled versions are available at this time:

- [Linux x86](http://software.rochus-keller.ch/LuonIDE_linux_i386.tar.gz)
- [Linux x86_64](http://software.rochus-keller.ch/LuonIDE_linux_x64.tar.gz)
- [Windows x86](http://software.rochus-keller.ch/LuonIDE_win32.zip)

Just download and unpack the compressed file to a directory. Start the IDE by double clicking on the LuonIDE executable. The specification and some demonstration projects are included.

### How to build

Follow these steps if you want to build the Luon IDE yourself. The build is using LeanQt and the BUSY build system. Note that compiling on Linux requires the build essentials, xcb, libxcb1-dev and libx11-dev packages. On Mac and Windows there are no additional requirements than a toolchain.

1. Create a new directory; we call it the root directory here
1. Download https://github.com/rochus-keller/Luon/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "Oberon".
1. Download https://github.com/rochus-keller/LuaJIT/archive/refs/heads/LjTools.zip and unpack it to the root directory; rename the resulting directory to "LuaJIT".
1. Download https://github.com/rochus-keller/LjTools/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "MonoTools".
1. Download https://github.com/rochus-keller/GuiTools/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "GuiTools".
1. Download https://github.com/rochus-keller/LeanQt/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "LeanQt".
1. Download https://github.com/rochus-keller/BUSY/archive/refs/heads/master.zip and unpack it to the root directory; rename the resulting directory to "build".
1. Open a command line in the "LuaJIT" directory and follow the instructions there to build LuaJIT for your platform.
1. Open a command line in the "build" directory and type `cc *.c -O2 -lm -o lua` or `cl /O2 /MD /Fe:lua.exe *.c` depending on whether you are on a Unix or Windows machine; wait a few seconds until the Lua executable is built.
1. Now type `./lua build.lua ../Luon` (or `lua build.lua ../Luon` on Windows); wait until the LuonIDE executable is built; you find it in the output subdirectory.

Instead of the command line you can run the build using [LeanCreator](https://github.com/rochus-keller/LeanCreator) which uses multiple cores and thus builds faster.

It is still possible to build the IDE using Qt 5 with qmake; use LnIde.pro for this purpose and proceed as usual when building with Qt.

### Support

If you need support or would like to post issues or feature requests please use the Github issue list at https://github.com/rochus-keller/Luon/issues or send an email to the author.

### License

Luon is available under GPL 2 or 3. The runtime libraries in the runtime subfolder, in addition, are available under MPL or LGPL.

### Additional Credits

- [LuaJIT](http://luajit.org) is Copyright © 2005-2023 Mike Pall, released under the MIT open source license. 
- [LeanQt](https://github.com/rochus-keller/LeanQt) is based on Qt, which is Copyright (C) 2016 by The Qt Company Ltd, 2008 by Nokia Corporation and/or its subsidiary(-ies), 1992-2005 by Trolltech AS, and many individual contributors around the world.




