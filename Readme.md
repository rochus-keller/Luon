This project implements a compiler and IDE for the Luon programming language.

Luon is a high-level programming language with a syntax similar to Oberon+, aspiring to run on the LuaJIT VM and to be compatible with the Lua programming language. Luon can be regarded as a statically typed version of Lua. The name is thus a combination of "Lua" and "Oberon". 

The project is the result of the author's experience with the [Smalltalk-80](https://github.com/rochus-keller/Smalltalk/) and [SOM VM](https://github.com/rochus-keller/Som/) implementation, and the prospect to build a [LuaJIT based Interlisp VM](https://github.com/rochus-keller/Gingko/). 

In contrast to Oberon+, Luon doesn't have pointers, but instead all structured datatypes have reference semantics and are dynamically created. In addition to the ARRAY type, there is also a HASHMAP type for compatibility with Lua. There is also a STRING data type which - like Lua - is immutable and follows a value semantics. 

In particular, Luon solves the following shortcomings recognized during the aforementioned projects:

- conditional compilation to enable or disable statements only used for debugging and to avoid wasting calculation time 
- constants not requiring local slots or hashed element access
- explicit inline declaration, so the code can be better structured without additional context 
  switches and slot consumption
- no implicit global declarations, and other means to detect as many errors as possible during compile time
- locals can no longer be used before declaration
- switch/case control statement to avoid writing the relation expressionall over again


NOTE that this project is in an early stage and work-in-progress.

#### Planned features

- [x] Implement lexer with directive support
- [ ] Implement parser, validator and IR generator
- [ ] Implement a LuaJIT backend based on LjTools
- [ ] Implement an IDE similar to the Oberon+ IDE
- [ ] Migrate the Smalltalk-80 VM to Luon as a proof-of-concept and to optimize the language

#### Status on October 14, 2024

Derived the syntax from Oberon+ and Micron and generated a parser using [the grammar](https://github.com/micron-language/specification/blob/master/Micron_Programming_Language.ebnf) and adopted the lexer from [the Micron project](https://github.com/rochus-keller/Micron). The parser is able to successfully read the ongoing Luon migration of the 
[Are-we-fast-yet benchmark suite](https://github.com/rochus-keller/Are-we-fast-yet/tree/main/Luon) (not yet commited).

#### Precompiled versions

Not available at this time.

#### How to build

Qt Creator 3.x is currently used for development. There are *.pro files which are compatible with qmake, but there is no Qt dependency otherwise.
