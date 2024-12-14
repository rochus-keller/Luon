This is a Smalltalk-80 implementation written in Luon, following the famous [Smalltalk "Bluebook"](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf). 

The original files are in my [Smalltalk repository](https://github.com/rochus-keller/Smalltalk) and copied here for convenience.

The VM supports an SDL2 and Qt based PAL (platform abstraction layer) which can be selected by changing the "PAL2" prefix in _Display.lua to "PAL3" and vice versa.

Both a C and Luon based BitBlt are supported (the latter still has issues).

This project is meant as a proof-of-concept for the Luon language, compiler and IDE, and to demonstrate the fitness for purpose.
