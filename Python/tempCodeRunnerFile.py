n=int(input())
s=0
d=1
for i in range(1,n):
    s=s+d
    d=s+2

print(4*n -4,s)