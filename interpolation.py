import sympy
import math
from sympy.abc import x, c
from sympy import pi, pprint, prod, product
import numpy

# sympy.init_printing(use_unicode=True)


def lagrange_interpolation(*points: tuple[float, float]) -> sympy.Expr:
    expr = sympy.poly(0, x, domain='RR')
    for x0, y0 in points:
        term = sympy.poly(y0, x, domain='RR')
        for x1, _ in points:
            if x0 == x1:
                continue

            term *= (x - x1) / (x0 - x1)

        expr += term

    return expr.expr


def divided_difference(*points: tuple[float, float]) -> sympy.Expr:
    def difference(*points: tuple[float, float]) -> float:
        if len(points) == 1:
            _, fx = points[0]
            return fx
        else:
            num = difference(*points[1:]) - difference(*points[:-1])
            den = points[-1][0] - points[0][0]
            return num / den

    sums = 0
    xs, ys = [*zip(*points)]
    for i in range(len(points)):
        prod = 1
        for j in range(i):
            prod *= (x - xs[j])
        sums += prod * difference(*points[:i + 1])

    return sums


def example():
    sympy.pprint(lagrange_interpolation((0, 1), (2, 2), (3, 4)))
    sympy.pprint(lagrange_interpolation((0, 2), (1, 1), (2, 0), (3, -1)))
    sympy.pprint(divided_difference((0, 1), (2, 2), (3, 4)).expand())
    sympy.pprint(divided_difference((0, 2), (1, 1), (2, 0), (3, -1)))

    xs = [*map(float, [0, sympy.pi/6, 2*sympy.pi/6, 3*sympy.pi/6])]
    ys = [*map(float, [sympy.sin(x) for x in xs])]
    sympy.pprint(divided_difference(*zip(xs, ys)).expand())


def interpolation_error(xs: list[float], f) -> sympy.Expr:
    n = len(xs)
    prod = 1
    for xi in xs:
        prod *= x - xi

    prod /= sympy.factorial(n)
    prod *= f.diff(x, n).subs({x: c})

    return prod


def additional_examples():
   #  print(divided_difference((-2, -9), (-1, -1), (0, -9), (3, -9), (4, 9)).expand())

    xs = [0, 0.2, 0.4, 0.8, 1.0]
    ys = [*map(lambda x: 2**x, xs)]
    print(divided_difference(*zip(xs, ys)).expand())
    errf = interpolation_error([0, 0.2, 0.4, 0.6, 0.8, 1.0], 2**x)
    pprint(errf.subs({x: 0.5, c: 1}).evalf())
    pprint(errf.subs({x: 0.9, c: 1}).evalf())


def e1():
    xs = [sympy.Float(0), pi/2, pi]
    ys = [*map(math.sin, xs)]

    # (a)
    p4 = divided_difference(*zip(xs, ys)).expand()
    pprint(p4)

    # (b)
    pprint(p4.subs({x: pi/4}))

    # (c)
    errf = interpolation_error(xs, sympy.sin(x))
    pprint(errf.subs({x: pi/4, c: 0}))


def e3():
    xs = numpy.linspace(0, 1, 10)
    ys = [*map(math.sin, xs)]

    errf = interpolation_error(xs, sympy.exp(-2*x))
    # (a)
    pprint(errf)
    pprint(errf.subs({x: 1/2, c: 0}))


def e5():
    xs = numpy.linspace(0.1, 0.6, 6)
    err_faction = sympy.prod([(x - xi) for xi in xs])
    pprint(err_faction.subs({x: 0.35}) / err_faction.subs({x: 0.55}))  # ~ 5/21


def c1():
    xs = [0.6, 0.7, 0.8, 0.9, 1.0]
    ys = [1.433329, 1.63216, 1.896481, 2.247908, 2.718282]

    p4 = divided_difference(*zip(xs, ys)).expand()
    pprint(p4)
    pprint(p4.subs({x: 0.82}))
    pprint(p4.subs({x: 0.98}))

    errf = interpolation_error(xs, sympy.exp(x**2))
    pprint(errf.subs({x: 0.82, c: 1}).evalf())
    pprint(errf.subs({x: 0.98, c: 1}).evalf())

    print(p4 - sympy.exp(x**2))


def c3():
    xs = numpy.linspace(1994, 2003, 10)
    ys = [67.052e6, 68.008e6, 69.803e6, 72.024e6, 73.4e6,
          62.024e6, 72.063e6, 74.669e6, 74.487e6, 74.065e6, 76.777e6]
    print(divided_difference(*zip(xs, ys)))


c3()
