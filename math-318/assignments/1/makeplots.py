#!/usr/bin/env python

import sympy as sp

x, y = sp.symbols("x y")

#p = sp.plot(x - 1, show=False)
#p2 = sp.plot(x**2, show=False)
#p2[0].style = "dotted"

p = sp.plot(x - 1, show=False)

p2 = sp.plot(x**2)
p2.style = "dotted"
p[2] = sp.And(x > y + 1, x**2 > y)

p.save("diagram1.pdf")


