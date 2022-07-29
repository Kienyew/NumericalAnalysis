import sympy
import functools
from sympy.abc import x

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


def additional_examples():
    print(divided_difference((-2, -9), (-1, -1),
                             (1, -9), (3, -9), (4, 9)).expand())


# example()
additional_examples()
