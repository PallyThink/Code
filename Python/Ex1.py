a=int(input('nhap chieu cao hinh chu nhat :  '))
n=int(input('nhap chieu rong cua hinh chu nhat: '))
s=0
print('dien tich hinh chu nhat la : ',a*n)

for i in range(1,n+1):
    s+=i
print(s)
