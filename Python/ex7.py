l=[]
for i in range(3):
    row=list(int(z) for z in input().split())
    l.append(row)
x=l[0][0] * l[1][1] * l[2][2] + l[0][1]*l[1][2]*l[2][0] + l[0][2]*l[1][0]*l[2][1]
y=l[2][0]*l[1][1]*l[0][2] + l[2][1]*l[1][2]*l[0][0] + l[2][2]*l[1][0]*l[0][1]
print(x-y)