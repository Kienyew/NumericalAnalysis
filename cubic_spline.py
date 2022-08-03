import sympy
from sympy.abc import x
import pprint

# given n points, return n - 1 cubic polynomials


def cubic_spline(xs: list[float], ys: list[float]) -> list[sympy.Expr]:
    n = len(xs)
    assert len(ys) == n

    def dx(i: int) -> float:
        return xs[i + 1] - xs[i]

    def dy(i: int) -> float:
        return ys[i + 1] - ys[i]

    cmatrix = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            if i == 0 and j == 0:
                cmatrix[i][j] = 1
            elif i == 0:
                cmatrix[i][j] = 0
            elif i == n - 1 and j == n - 1:
                cmatrix[i][j] = 1
            elif i == n - 1:
                cmatrix[i][j] = 0
            elif j == i - 1:
                cmatrix[i][j] = dx(i - 1)
            elif j == i + 1:
                cmatrix[i][j] = dx(i)
            elif i == j:
                cmatrix[i][j] = 2 * (dx(i - 1) + dx(i))
            else:
                cmatrix[i][j] = 0

    cn = [0] + [3 * (dy(i) / dx(i) - dy(i - 1) / dx(i - 1))
                for i in range(1, n - 1)] + [0]
    cn = sympy.Matrix(cn)
    cs = sympy.Matrix(cmatrix).inv() @ cn

    def c(i):
        return cs[i]

    splines = []
    for i in range(n - 1):
        a = ys[i]
        d = (c(i + 1) - c(i)) / (3 * dx(i))
        b = dy(i)/dx(i) - dx(i)/3 * (2*c(i) + c(i + 1))
        s = a + b*(x - xs[i]) + c(i) * (x - xs[i])**2 + d*(x - xs[i])**3

        splines.append(s)

    return splines


def example():
    xs = [0, 1, 2, 5]
    ys = [3, -2, 1, 10]
    pprint.pprint(cubic_spline(xs, ys))


example()
