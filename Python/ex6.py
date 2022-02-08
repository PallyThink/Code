def update(n):
	if n <= 1:
		return 1
	s = 0
	a = 1
	b = 1
	for i in range( 2, n+1):
		s = a + b
		c = a
		a = s
		b = c
	return s
n=int(input())
k=update(n)%(1000000000+7)
print(k)