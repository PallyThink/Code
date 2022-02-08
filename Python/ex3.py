j=1
while j<=9 :
    i=1
    while i<=9 :
        a=i*j
        if a<10 : 
            print(" ",a,end='   ')
        else :
            print(" ",a,end='  ')
        i=i+1
    print("\n")
    j=j+1