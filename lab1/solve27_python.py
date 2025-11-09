def f(n, a, b):
    return n * n + a * n + b
    
def is_prime(n):
    if n <= 1:
        return False
    i = 2
    while i * i <= n:
        if n % i == 0:
            return False
        i += 1
    return True
    
result = 0
n_max = 0
for a in range(-1000, 1001):
    for b in range(-1000, 1001):
        n = 0
        while is_prime(f(n, a, b)):
            n += 1
        if n_max < (n - 1):
            n_max = n - 1
            result = a * b
print(result)
print(n_max)
