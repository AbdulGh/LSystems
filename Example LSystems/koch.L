This file defines the koch curve and triangle!

variables:
linear F //Go forwards
linear + //Rotate left 45 degrees
linear - //Rotate right 45 degrees

rules:
//line becomes a spike of four lines
F -> F + F - - F + F

axioms:
curve F
square F + + F + + F + + F
//triangle will require a 60 degree rotation