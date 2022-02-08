n=int(input())
s=0
x=0
if 5<=n<=10**9:
    for i in range(1,n+1):
        if s<=n:
            s=s+i
            x=x+1
        else:
            print(x-1)
            break