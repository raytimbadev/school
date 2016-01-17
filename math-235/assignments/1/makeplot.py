#!/usr/bin/env python

from sympy import *

eqs = []

x = Symbol('x')

for n in range(1, 11):
    eqs.append(x**n)

p = plot(*eqs, xlim=[0,2.5], ylim=[0,5], show=False)

p.save("polynomials.pdf")
