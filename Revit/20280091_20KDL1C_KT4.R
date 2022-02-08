#cau1 
#H0 : thoi gian 1 lan cung cap dvu <= 12 phut
#H1 : thoi gian 1 lan cung cap dvu > 12 phut
n=25
mu=12
x.bar=13.25
s=3.2
t_0 = (x.bar - mu)*sqrt(n)/s
# tinh p - value
p.value = 1- pnorm(t_0)
ifelse(p.value < 0.05,'Bac bo H0','Chap nhan H0')

#cau2
#H0:ham luong thach tin trong nuoc tai area1 = area2
#H1: ham luong thach tin trong nuoc tai area1 != area2
area1=c(3,7,25,10,15,6,12,25,15,7)
area2=c(48,44,40,38,33,21,20,1,18)
alpha = 0.05
result = t.test(area1, area2, conf.level = 0.95)
result$p.value
ifelse(re$p.value < alpha, 'Bac bo H0', 'Chap nhan H0')
#voi p.value =0.0012 < 0.05 -> ham luong thach tin o khu vuc 1 khac khu vuc 2

#cau3
#H0: Hai may co ti le giam tho may bang nhau
#H1: Hai may co ti le giam tho may khac nhau
alpha = 0.05
n = c(100, 100)
y = c(3, 13)
re <- prop.test(y, n, conf.level = 1 - alpha)
re
ifelse(re$p.value < 0.05,'Bac bo H0','Khong co so bac bo H0')
#p.value=0.01899 < 0.05 -> may 1 co ti le giam may tho khac voi may 2

#cau4
data=read.csv(file.choose())
names(data)
tuoi=data$tuoi[data$gioi_tinh=='nu'];
cannang=data$can_nang[data$gioi_tinh=='nu'];

result=lm((cannang)~tuoi)
res=resid(result)
b1=(coef(result))[['tuoi']]

plot(cannang,tuoi)
abline(lm(tuoi~cannang),col='blue')

