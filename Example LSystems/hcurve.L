// https://kruzenshtern.org/25-h-curve.html

variables:
linear F translation 1 0
linear + rotation 1.5708 //pi/2 rads
linear - rotation -1.5708 

rules:
F -> F - F - F F F - F - F + F + F - F - F F F - F - F

axioms:
start F