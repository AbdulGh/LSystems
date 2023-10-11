This file defines the koch curve and triangle!

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