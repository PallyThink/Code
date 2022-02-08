setwd("C:/Users/Administrator/Desktop/Math/Xac suat thong ke/TH XSTK/data/Data cho cac bai thuc hanh")
data=read.csv("chieucao_cannang.csv",header = TRUE)
#cau1
gioitinh=data$gioi_tinh
sl_nam=function(gioitinh)
{
  sl=0
  giotinh=data$gioi_tinh
  for(i in gioitinh)
  {
    if(i=="nam")
    {
      sl=sl+1
    }
  }
  return(sl)
}
#sl nam
sl_nam(gioitinh)
#sl nu
length(gioitinh)-sl_nam(gioitinh)
#cau2
can_nang=function(data)
{
  chiso=0
  for(i in 1:length(data$stt))
  {
    if(data$gioi_tinh[i]=="nu" & chiso<data$can_nang[i])
    {
      chiso=data$can_nang[i]
    }
  }
  return(chiso)
}
#can nang lon nhat
can_nang(data)

stt=function(data)
{
  for(i in 1:length(data$stt))
  {
    if (data$gioi_tinh[i]=="nu" & can_nang(data)==data$can_nang[i])
    {
      return(i)
    }
  }
}
#stt cua doi tuong
stt(data)
#cau3
#trung binh chieu cao
tb_chieucao=function(data)
{
  tong_CC=0
  for(i in 1:length(data$stt))
  {
    if(data$gioi_tinh[i]=="nam")
    {
      tong_CC=tong_CC+data$chieu_cao[i]
    }
  }
  return(tong_CC/sl_nam(gioitinh))
}
tb_chieucao(data)
#phuong sai chieu cao
phuongsai=function(data)
{
   
}
#cau4
BMI=function(chieucao,cannang)
{
  return(cannang/(chieucao^2))
}
BMI(1.7,55)
#cau5
checkBMI=function(chieucao,cannang)
{
  if(BMI(chieucao,cannang)<18.5) return("Thieu can")
  else if(BMI(chieucao,cannang)>=18.5 & BMI(chieucao,cannang)<25)
  {
    return("Binh Thuong")
  }
  else if(BMI(chieucao,cannang)>=25 & BMI(chieucao,cannang) <30)
  {
    return("Thua can")
  }
  else
  {
    return("beo phi")
  }
}
checkBMI(1.7,55)

#bai1
df=read.csv(file.choose())
table(df$gioi_tinh)
#bai2
df(df$gioi_tinh=='nu',)
df(df$gioi_tinh=='nu','can_nang')
can_nang_nu_max=max(df(df$gioi_tinh=='nu','can_nang'))




