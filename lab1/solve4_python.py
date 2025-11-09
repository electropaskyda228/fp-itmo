def is_palindrome(n):
    s = str(n)
    return s == s[::-1]
    
maximum = 0
for a in range (100, 1000):
    for b in range(100, 1000):
        if is_palindrome(a * b):
            maximum = max(a * b, maximum)
print(maximum)
