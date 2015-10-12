#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import operator as op
from scipy.spatial import ConvexHull

from sympy import *
from matplotlib.patches import Polygon

if __name__ == '__main__':
    x1, x2 = symbols('x1 x2')

    lp = [
            (x1 + x2, 0),
            (2 * x1 + x2, 7),
            (x1 - x2, -2),
            (x1 - 3 * x2, 0),
            (- x1 + 2 * x2, 4),
    ]

    equations = [
            Eq(*t)
            for t
            in lp
    ]

    constraints = [
            o(*t)
            for t, o
            in zip(
                lp,
                [op.ge, op.le, op.ge, op.le, op.le]
            )
    ]

    class P:
        def __init__(self, x, y, eq1, eq2):
            self.i = eq1
            self.j = eq2
            self.v = (x, y)

        def __getitem__(self, i):
            return self.v[i]

        def __hash__(self):
            return hash(self.v) + hash(self.i) + hash(self.j)

        def __eq__(self, other):
            return (
                    self.v == other.v
                    and
                    self.i == other.i
                    and
                    self.j == other.j
            )

        def __repr__(self):
            return "P(x=%(x)s, y=%(y)s, eq1=%(i)d, eq2=%(j)d)" % dict(
                    x=str(self.v[0]),
                    y=str(self.v[1]),
                    i=self.i,
                    j=self.j,
            )

    verts = set()

    for i, e1 in enumerate(equations[:-1]):
        for j, e2 in enumerate(equations[i+1:], i+1):
            if i == j:
                continue
            soln = solve([e1, e2], [x1, x2])
            if soln:
                verts.add( P(soln[x1], soln[x2], i, j) )

    verts = list(verts)

    print("The intersection points of the contraints lines are:")
    for p in verts:
        print(p)

    p = plot_implicit(
            And(*constraints),
            x_var=x1,
            y_var=x2
    )

    p.title = "The feasible region of the linear program"

    p.save('plot.pdf')
