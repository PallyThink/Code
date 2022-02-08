#bai6
#a
year=c(1970:1979)
snow.cover=c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2)
#b
plot(year,snow.cover,type='l')
#c
hist(snow.cover,freq = 1)
#d
d=log(snow.cover)
#b
plot(year,d,type='l')
#c
hist(d,freq = 1)
#bai7
df=edit(data.frame())
plot(df$F,df$T,type='h')
#bai8
#a
lamphat1=edit(data.frame())
lamphat2=edit(data.frame())
#b
lamphat=data.frame(lamphat1,lamphat2[2:3])
#c
countlp=function(lamphat)
{
  d=0
  for (i in 1:length(lamphat$Nam)) 
    {
    if( (lamphat$US[i] >5) & (lamphat$Anh[i] >5) & (lamphat$Nhat[i] >5) & (lamphat$Duc[i] >5))
    {
      d=d+1
    }
  }
  return(d)
}
countlp(lamphat)

length(lamphat$Nam[lamphat$US>5])
length(lamphat$Nam[lamphat$Anh>5])
length(lamphat$Nam[lamphat$Nhat>5])
length(lamphat$Nam[lamphat$Duc>5])
#d
plot(lamphat$Nam,lamphat$US,type = 'l')
plot(lamphat$Nam,lamphat$Anh,type = 'l')
plot(lamphat$Nam,lamphat$Nhat,type = 'l')
plot(lamphat$Nam,lamphat$Duc,type = 'l')
#e
#trung binh
mean(lamphat$US)
mean(lamphat$Anh)
mean(lamphat$Nhat)
mean(lamphat$Duc)
#phan vi
median(lamphat$US)
median(lamphat$Anh)
median(lamphat$Nhat)
median(lamphat$Duc)
#max
max(lamphat$US)
max(lamphat$Anh)
max(lamphat$Nhat)
max(lamphat$Duc)
#min
min(lamphat$US)
min(lamphat$Anh)
min(lamphat$Nhat)
min(lamphat$Duc)
# do lech chuan
sd(lamphat$US)
sd(lamphat$Anh)
sd(lamphat$Nhat)
sd(lamphat$Duc)

#f range()
us=max(lamphat$US) - min(lamphat$US)
anh=max(lamphat$Anh) - min(lamphat$Anh)
nhat=max(lamphat$Nhat) - min(lamphat$Nhat)
duc=max(lamphat$Duc) - min(lamphat$Duc)

#g
Nam=lamphat$Nam[1:(length(lamphat$Nam)-1)]
US=lamphat$US[1:(length(lamphat$Nam)-1)]
Anh=lamphat$Anh[1:(length(lamphat$Nam)-1)]
Duc=lamphat$Duc[1:(length(lamphat$Nam)-1)]
Nhat=lamphat$Nhat[1:(length(lamphat$Nam)-1)]
lamphat1=data.frame(Nam,US,Anh,Duc,Nhat)
lamphat1=lamphat[lamphat$Nam!=1980,]
lamphat1
