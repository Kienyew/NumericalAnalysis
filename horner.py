import random

# horner(polynomial, x0) = (P(x0), P'(x0))


def horner(a, x0):
    if len(a) == 1:
        return (a[0], 0, [])

    n = len(a) - 1

    def b(k):
        if k == n:
            return a[n]
        else:
            return a[k] + b(k + 1) * x0

    Q = [b(k) for k in range(1, n + 1)]
    return (b(0), horner(Q, x0)[0], Q)


def evaluate(a, x0):
    return horner(a, x0)[0]


def newton(a, x0, tol):
    fx, fprimex, _ = horner(a, x0)
    if abs(fx) <= tol:
        return x0
    else:
        return newton(a, x0 - fx/fprimex, tol)


def complex_roots(a, tol=0.001):
    if len(a) <= 1:
        return []

    x = 1 + 1j
    fx, fprimex, Q = horner(a, x)
    while abs(fx) > tol:
        x = x - fx/fprimex
        fx, fprimex, Q = horner(a, x)

    return [x] + complex_roots(Q, tol)


def real_roots(a, tol=0.001):
    if len(a) <= 1:
        return []

    prev_x = 0
    x = 0.28
    fx, fprimex, Q = horner(a, x)
    iterations = 0
    while abs(prev_x / x - 1) > tol:
        if iterations == 100:
            return []

        prev_x = x
        x = x - fx/fprimex
        fx, fprimex, Q = horner(a, x)

        iterations += 1

    return [x] + real_roots(Q, tol)


p = [-4, 3, -3, 0, 2.0]
# print(horner(p, -2))
# print(newton(p, -2))
# print(abs(evaluate(p, newton(p, -2))))
for p in [[-5, 0, -2, 1],
          [-1, 0, 3, 1],
          [-1, -1, 0, 1],
          [-3, -1, 2, 0, 1],
          [1.101, 4.002, 4.001, 1],
          [-4, 1, -3, 2, -1, 1]]:

    print(*map(lambda root: '(x - {})'.format(root), real_roots(p)))


print(real_roots([-0.21141, 2.295, -8.3, 10]))
