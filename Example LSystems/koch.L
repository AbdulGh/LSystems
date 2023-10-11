This file defines the koch curve and triangle!

With these files, everything up to the 'variables:' line is ignored, as are any lines that are empty or that begin with //.
Then:
variables are parsed as: type symbol info etc, where
-type is from a list of predefined types
-symbol is any identifier
-info is some type specific thing
-etc is anything after the variable construction has consumed all that it expects (does not really need to begin with //)

this is followed by a 'rules:' line, which is followed by a newline separated list of rules that just look like
symbol -> space separated symbols that it becomes
(deterministic for now)

this is followed by 'axioms:', then a list of
name space separated symbols
which serve as possible starting points for your fractal.

variables:
linear F translation 0 1 //Go 'up'
linear + rotation -1.0471976 //Rotate left 60 degrees
linear - rotation 1.0471976 //Rotate right 60 degrees

rules:
//line becomes a spike of four lines
F -> F + F - - F + F

axioms:
curve F
weird F + + F + + F + + F