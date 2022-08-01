import interpolation
import sympy
from sympy.abc import x, c
import math
import numpy
import re


def chebyshev_nodes(n: int, a: float, b: float) -> list[float]:
    nodes = []
    for i in range(1, n + 1):
        xi = (b + a) / 2 + (b - a) / 2 * sympy.cos((2*i - 1)*sympy.pi / (2*n))
        nodes.append(xi)

    return [x.evalf() for x in nodes]


def chebyshev_interpolation_errors(n: int, a: float, b: float, f: sympy.Expr) -> sympy.Expr:
    return ((b - a) / 2) ** n / 2**(n - 1) / sympy.factorial(n) * f.diff(x, n).subs({x: c})


def e1():
    sympy.pprint(chebyshev_nodes(6, -1, 1))
    sympy.pprint(chebyshev_nodes(4, -2, 2))
    sympy.pprint(chebyshev_nodes(6, 4, 12))
    sympy.pprint(chebyshev_nodes(5, -0.3, 0.7))


def e3():
    sympy.pprint(chebyshev_interpolation_errors(
        6, -1, 1, sympy.exp(x)).subs({c: 1}).evalf())


def e5():
    sympy.pprint(chebyshev_interpolation_errors(
        4, 0, 2, sympy.sin(x)).subs({c: sympy.pi/2}))
    xs = chebyshev_nodes(4, 0, 2)
    ys = [math.sin(x) for x in xs]
    print(interpolation.divided_difference(*zip(xs, ys)))


def c5():
    n = 10
    # xs = numpy.linspace(-1, 1, n)
    xs = chebyshev_nodes(n, -1, 1)
    ys = [math.exp(-x**2) for x in xs]
    # ys = [math.exp(abs(x)) for x in xs]

    code = sympy.printing.mathematica.mathematica_code(
        interpolation.lagrange_interpolation(*zip(xs, ys)).expand())
    print(re.sub(r'e(\-?\d+)', r'*10^(\1)', code))


c5()
