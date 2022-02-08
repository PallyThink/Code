
##n=9
# d=0
#     print(A)
# while n>0 :
#     i=1
#     while i<n : 
#         print(end=' ')
#         i=i+1
#     j=0
#     while j<=d: #d=2 , d=4
#         print(end='*')
#         j=j+1
#     n=n-1  ## n=2 , n=1 , n=0
#     d=d+2  # d=2 , d=4 , d=6
#     print('\n')
print("Nhập giá trị n: ",end='')
n=int(input())
if n >= 0:
    giaithua=1
    for i in range(1,n+1):
        giaithua=giaithua*i
    print(n,"giai thừa bằng:",giaithua)
else:
    print("Vui lòng nhập n > 0")