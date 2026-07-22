This is the Oberon System originally from http://www.projectoberon.net/ and originally written in Oberon-07.
I used the version from https://github.com/rochus-keller/OberonSystem, particlularly the one with the
SDL backend, and made the code Luon compatible. It compiles and runs without errors, and the display
look and feel is now correct. Though note that some applications like Hilbert and Sierpinski are not
yet migrated.

Luon - in contrast to Oberon+ - doesn't have an FFI, but procedures can be declared extern and implemented
in Lua. Currently the PAL.luon module makes use of this; the implementation in _PAL.lua then calls native
C code via the LuaJIT FFI.

A future version will get rid of the C SDL adapter and implement the complete PAL in Lua.
