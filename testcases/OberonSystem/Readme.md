This is the Oberon System originally from http://www.projectoberon.net/ and originally written in Oberon-07.
I used the version from https://github.com/rochus-keller/OberonSystem, particlularly the one with the
SDL backend, and made the code Luon compatible. It currently compiles and runs without errors, but 
the display doesn't look correct and more debugging is necessary.
Luon - in contrast to Oberon+ - doesn't have an FFI, but procedures can be declared extern and implemented
in Lua. Currently the PAL.luon module makes use of this; the implementation in _PAL.lua then calls native
C code via the LuaJIT FFI.

I'm currently using this project to check and improve the Luon implementation. Since Luon only has reference
semantics and no explicit pointers, parts of the code are not yet properly initialized. The goal is to fix 
that and eventually get rid of the C SDL adapter, and implementing the complete PAL in Lua.
