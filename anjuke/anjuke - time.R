##���ù����ռ�
#�ѡ����ݼ������ļ��п�����F���£�����setwd���ù����ռ�
setwd("C:/Users/leishen/Documents/R/win-library/practice/anjuke")
#���ݶ�ȡ
anjukefile1=read.csv('./anjkzzfx-2.csv',he=T)
rows=dim(anjukefile1)[1]
anjukefile2<-anjukefile1[0,]
for (i in 1:rows)
  for(j in 1:anjukefile1[i,12])
    anjukefile2<-rbind(anjukefile2,anjukefile1[i,])


#�����ϲ��ĳɽ��۸���ܶ�ͼ
mydensity2<-function(anjukefile2){
  par(mfrow=c(2,1))
  d <- density(anjukefile2$ckjg1)
  plot(d)
  d <- density(anjukefile2$ckjg1)
  plot(d, main="Kernel Density of Miles Per Gallon")
  polygon(d, col="red", border="blue")
  rug(anjukefile2$ckjg1, col="brown")
}
mydensity2(anjukefile2)

#���ݽ���ʱ��Ϊx����Ʋο��۸��С����ͼ
myviotime1<-function(anjukefile2){
  library(vioplot)
  anjukefile<-anjukefile2
  y1 <- anjukefile$ckjg1[anjukefile$jfsj1==2011]
  y2 <- anjukefile$ckjg1[anjukefile$jfsj1==2013]
  y3 <- anjukefile$ckjg1[anjukefile$jfsj1==2014]
  y4 <- anjukefile$ckjg1[anjukefile$jfsj1==2015]
  y5 <- anjukefile$ckjg1[anjukefile$jfsj1==2016]
  y6 <- anjukefile$ckjg1[anjukefile$jfsj1==2017]
  y7 <- anjukefile$ckjg1[anjukefile$jfsj1==2018]
  y8 <- anjukefile$ckjg1[anjukefile$jfsj1==2019]
  y9 <- anjukefile$ckjg1[anjukefile$jfsj1==2020]
  if (length(y1)==0) y1<-c(y1,15000) else y1<-c(y1,mean(y1)+100)
  if (length(y2)==0) y2<-c(y2,15000) else y2<-c(y2,mean(y2)+100)
  if (length(y3)==0) y3<-c(y3,15000) else y3<-c(y3,mean(y3)+100)
  if (length(y4)==0) y4<-c(y4,15000) else y4<-c(y4,mean(y4)+100)
  if (length(y5)==0) y5<-c(y5,15000) else y5<-c(y5,mean(y5)+100)
  if (length(y6)==0) y6<-c(y6,15000) else y6<-c(y6,mean(y6)+100)
  if (length(y7)==0) y7<-c(y7,15000) else y7<-c(y7,mean(y7)+100)
  if (length(y8)==0) y8<-c(y8,15000) else y8<-c(y8,mean(y8)+100)
  if (length(y9)==0) y9<-c(y9,15000) else y9<-c(y9,mean(y9)+100)
  
  
  vioplot(y1, y2, y3, y4, y5, y6, y7, y8, y9,
          names=c("2011", "2013", "2014","2015", "2016", "2017","2018", "2019", "2020"), 
          col="gold")
  #title("��ʱ������")
}
myviotime1(anjukefile2)
#����ʱ��Ϊx�����ÿ�������Ĳο��۸�С����ͼ
myviotime<-function(anjukefile2,qyjc){
  library(vioplot)
  anjukefile<-anjukefile2
  #anjukefile<-anjukefile2[anjukefile2$qywz1==qyjc,]
  y1 <- anjukefile$ckjg1[anjukefile$jfsj1==2011]
  y2 <- anjukefile$ckjg1[anjukefile$jfsj1==2013]
  y3 <- anjukefile$ckjg1[anjukefile$jfsj1==2014]
  y4 <- anjukefile$ckjg1[anjukefile$jfsj1==2015]
  y5 <- anjukefile$ckjg1[anjukefile$jfsj1==2016]
  y6 <- anjukefile$ckjg1[anjukefile$jfsj1==2017]
  y7 <- anjukefile$ckjg1[anjukefile$jfsj1==2018]
  y8 <- anjukefile$ckjg1[anjukefile$jfsj1==2019]
  y9 <- anjukefile$ckjg1[anjukefile$jfsj1==2020]
  if (length(y1)==0) y1<-c(y1,15000) else y1<-c(y1,mean(y1)+100)
  if (length(y2)==0) y2<-c(y2,15000) else y2<-c(y2,mean(y2)+100)
  if (length(y3)==0) y3<-c(y3,15000) else y3<-c(y3,mean(y3)+100)
  if (length(y4)==0) y4<-c(y4,15000) else y4<-c(y4,mean(y4)+100)
  if (length(y5)==0) y5<-c(y5,15000) else y5<-c(y5,mean(y5)+100)
  if (length(y6)==0) y6<-c(y6,15000) else y6<-c(y6,mean(y6)+100)
  if (length(y7)==0) y7<-c(y7,15000) else y7<-c(y7,mean(y7)+100)
  if (length(y8)==0) y8<-c(y8,15000) else y8<-c(y8,mean(y8)+100)
  if (length(y9)==0) y9<-c(y9,15000) else y9<-c(y9,mean(y9)+100)
  
  vioplot(y1, y2, y3, y4, y5, y6, y7, y8, y9,
          names=c("2011", "2013", "2014","2015", "2016", "2017","2018", "2019", "2020"), 
          col="gold")
  #title(paste("��ʱ������",qyjc))
}
myviotime(anjukefile2,'DD')
#���ݽ���ʱ����Ʋο��۸�ĺ��ܶ��ص�ͼ
mydensity<-function(anjukefile2){
  # Listing 6.8 - Comparing kernel density plots
  par(mfrow=c(1,1))
  par(lwd=2) 
  library(sm)
  attach(anjukefile2)
  
  # create value labels 
  jfsj1.f <- factor(jfsj1, levels= c(2011,2013,2014,2015,2016,2017,2018,2019,2020),                               
                    labels = c("2011","2013", "2014","2015", "2016", "2017","2018", "2019", "2020")) 
  
  # plot densities 
  sm.density.compare(ckjg1, jfsj1, xlab="��λƽ�׼۸�")                
  title(main="�ϲ�������������ݺ��ܶ�ͼ")
  
  # add legend via mouse click
  colfill<-c(2:(1+length(levels(jfsj1.f)))) 
  cat("Use mouse to place legend...","\n\n")
  legend(locator(1), levels(jfsj1.f), fill=colfill) 
  detach(anjukefile2)
  par(lwd=1)
}
mydensity(anjukefile2)
#�������¿���ʱ����Ʋο��۸�ĺ��ܶ��ص�ͼ��ʧ���ˣ���֪Ϊ�β��ܻ���
mydensity1<-function(anjukefile2){
  # Listing 6.8 - Comparing kernel density plots
  par(mfrow=c(1,1))
  par(lwd=2) 
  library(sm)
  attach(anjukefile2)
  
  
  # create value labels 
  zxkp1.f <- factor(zxkp1, levels= c(2011,2012,2013,2014,2015,2016,2017,2018),                               
                    labels = c("2011","2012","2013", "2014","2015", "2016", "2017","2018")) 
  
  # plot densities 
  sm.density.compare(ckjg1, zxkp1, xlab="��λƽ�׼۸�")                
  title(main="�ϲ������¿�����ݺ��ܶ�ͼ")
  
  # add legend via mouse click
  colfill<-c(2:(1+length(levels(zxkp1.f)))) 
  cat("Use mouse to place legend...","\n\n")
  legend(locator(1), levels(zxkp1.f), fill=colfill) 
  detach(anjukefile2)
  par(lwd=1)
}
mydensity1(anjukefile2)
#�������¿���ʱ��ʱ����Ʋο����ۺ��ܶ�ͼ
mydensity3<-function(anjukefile,year){
  anjukefile2<-anjukefile[anjukefile$zxkp1==year,]
  #d <- density(anjukefile2$ckjg1)
  #plot(d)
  d <- density(anjukefile2$ckjg1)
  plot(d,xlim=c(0,40000),main=paste("���¿���ʱ��",year))
  polygon(d, col="red", border="blue")
  rug(anjukefile2$ckjg1, col="brown")
}
par(mfrow=c(4,1))
for (year in 2011:2018)
  mydensity3(anjukefile2,year)

#��������λ��Ϊx����Ʋο�����С����ͼ
par(mfrow=c(1,1))
myvioqywz<-function(anjukefile2){
  anjukefile<-anjukefile2
  y1<- c(anjukefile$ckjg1[anjukefile$qywz1=='DD'])
  y2<- c(anjukefile$ckjg1[anjukefile$qywz1=='GAGA'])
  y3<- c(anjukefile$ckjg1[anjukefile$qywz1=='GG'])
  y4<- c(anjukefile$ckjg1[anjukefile$qywz1=='GJ'])
  y5<- c(anjukefile$ckjg1[anjukefile$qywz1=='HF'])
  y6<- c(anjukefile$ckjg1[anjukefile$qywz1=='HH'])
  y7<- c(anjukefile$ckjg1[anjukefile$qywz1=='HGHJ'])
  y8<- c(anjukefile$ckjg1[anjukefile$qywz1=='HJ'])
  y9<- c(anjukefile$ckjg1[anjukefile$qywz1=='JX'])
  y10<- c(anjukefile$ckjg1[anjukefile$qywz1=='JN'])
  y11<- c(anjukefile$ckjg1[anjukefile$qywz1=='NL'])
  y12<- c(anjukefile$ckjg1[anjukefile$qywz1=='NN'])
  y13<- c(anjukefile$ckjg1[anjukefile$qywz1=='NX'])
  y14<- c(anjukefile$ckjg1[anjukefile$qywz1=='QC'])
  y15<- c(anjukefile$ckjg1[anjukefile$qywz1=='QSQS'])
  y16<- c(anjukefile$ckjg1[anjukefile$qywz1=='QQ'])
  y17<- c(anjukefile$ckjg1[anjukefile$qywz1=='SS'])
  y18<- c(anjukefile$ckjg1[anjukefile$qywz1=='WW'])
  y19<- c(anjukefile$ckjg1[anjukefile$qywz1=='XC'])
  y20<- c(anjukefile$ckjg1[anjukefile$qywz1=='XHXH'])
  y21<- c(anjukefile$ckjg1[anjukefile$qywz1=='XX'])
  
  
  if (length(y1)==0) y1<-c(y1,15000) else y1<-c(y1,mean(y1)+100)
  if (length(y2)==0) y2<-c(y2,15000) else y2<-c(y2,mean(y2)+100)
  if (length(y3)==0) y3<-c(y3,15000) else y3<-c(y3,mean(y3)+100)
  if (length(y4)==0) y4<-c(y4,15000) else y4<-c(y4,mean(y4)+100)
  if (length(y5)==0) y5<-c(y5,15000) else y5<-c(y5,mean(y5)+100)
  if (length(y6)==0) y6<-c(y6,15000) else y6<-c(y6,mean(y6)+100)
  if (length(y7)==0) y7<-c(y7,15000) else y7<-c(y7,mean(y7)+100)
  if (length(y8)==0) y8<-c(y8,15000) else y8<-c(y8,mean(y8)+100)
  if (length(y9)==0) y9<-c(y9,15000) else y9<-c(y9,mean(y9)+100)
  if (length(y10)==0) y10<-c(y10,15000) else y10<-c(y10,mean(y10)+100)
  if (length(y11)==0) y11<-c(y11,15000) else y11<-c(y11,mean(y11)+100)
  if (length(y12)==0) y12<-c(y12,15000) else y12<-c(y12,mean(y12)+100)
  if (length(y13)==0) y13<-c(y13,15000) else y13<-c(y13,mean(y13)+100)
  if (length(y14)==0) y14<-c(y14,15000) else y14<-c(y14,mean(y14)+100)
  if (length(y15)==0) y15<-c(y15,15000) else y15<-c(y15,mean(y15)+100)
  if (length(y16)==0) y16<-c(y16,15000) else y16<-c(y16,mean(y16)+100)
  if (length(y17)==0) y17<-c(y17,15000) else y17<-c(y17,mean(y17)+100)
  if (length(y18)==0) y18<-c(y18,15000) else y18<-c(y18,mean(y18)+100)
  if (length(y19)==0) y19<-c(y19,15000) else y19<-c(y19,mean(y19)+100)
  if (length(y20)==0) y20<-c(y20,15000) else y20<-c(y20,mean(y20)+100)
  if (length(y21)==0) y21<-c(y21,15000) else y21<-c(y21,mean(y21)+100)
  
  library(vioplot)
  vioplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,
          names=c("DD","GAGA","GG","GJ","HF","HH","HGHJ","HJ","JX","JN","NL","NN","NX","QC","QSQS","QQ","SS","WW","XC","XHXH","XX"), 
          col="gold")
  
  title("�ϲ���ͬ����Ĳο��۸�С����ͼ")
}
myvioqywz(anjukefile2)
#��������λ��Ϊx����Ʋ�ͬ�Ľ���ʱ��Ĳο�����С����ͼ
myvioqywz1<-function(anjukefile2,year){
  anjukefile<-anjukefile2[anjukefile2$jfsj1==year,]
  
  y1<- c(anjukefile$ckjg1[anjukefile$qywz1=='DD'])
  y2<- c(anjukefile$ckjg1[anjukefile$qywz1=='GAGA'])
  y3<- c(anjukefile$ckjg1[anjukefile$qywz1=='GG'])
  y4<- c(anjukefile$ckjg1[anjukefile$qywz1=='GJ'])
  y5<- c(anjukefile$ckjg1[anjukefile$qywz1=='HF'])
  y6<- c(anjukefile$ckjg1[anjukefile$qywz1=='HH'])
  y7<- c(anjukefile$ckjg1[anjukefile$qywz1=='HGHJ'])
  y8<- c(anjukefile$ckjg1[anjukefile$qywz1=='HJ'])
  y9<- c(anjukefile$ckjg1[anjukefile$qywz1=='JX'])
  y10<- c(anjukefile$ckjg1[anjukefile$qywz1=='JN'])
  y11<- c(anjukefile$ckjg1[anjukefile$qywz1=='NL'])
  y12<- c(anjukefile$ckjg1[anjukefile$qywz1=='NN'])
  y13<- c(anjukefile$ckjg1[anjukefile$qywz1=='NX'])
  y14<- c(anjukefile$ckjg1[anjukefile$qywz1=='QC'])
  y15<- c(anjukefile$ckjg1[anjukefile$qywz1=='QSQS'])
  y16<- c(anjukefile$ckjg1[anjukefile$qywz1=='QQ'])
  y17<- c(anjukefile$ckjg1[anjukefile$qywz1=='SS'])
  y18<- c(anjukefile$ckjg1[anjukefile$qywz1=='WW'])
  y19<- c(anjukefile$ckjg1[anjukefile$qywz1=='XC'])
  y20<- c(anjukefile$ckjg1[anjukefile$qywz1=='XHXH'])
  y21<- c(anjukefile$ckjg1[anjukefile$qywz1=='XX'])
  
  
  if (length(y1)==0) y1<-c(y1,15000) else y1<-c(y1,mean(y1)+100)
  if (length(y2)==0) y2<-c(y2,15000) else y2<-c(y2,mean(y2)+100)
  if (length(y3)==0) y3<-c(y3,15000) else y3<-c(y3,mean(y3)+100)
  if (length(y4)==0) y4<-c(y4,15000) else y4<-c(y4,mean(y4)+100)
  if (length(y5)==0) y5<-c(y5,15000) else y5<-c(y5,mean(y5)+100)
  if (length(y6)==0) y6<-c(y6,15000) else y6<-c(y6,mean(y6)+100)
  if (length(y7)==0) y7<-c(y7,15000) else y7<-c(y7,mean(y7)+100)
  if (length(y8)==0) y8<-c(y8,15000) else y8<-c(y8,mean(y8)+100)
  if (length(y9)==0) y9<-c(y9,15000) else y9<-c(y9,mean(y9)+100)
  if (length(y10)==0) y10<-c(y10,15000) else y10<-c(y10,mean(y10)+100)
  if (length(y11)==0) y11<-c(y11,15000) else y11<-c(y11,mean(y11)+100)
  if (length(y12)==0) y12<-c(y12,15000) else y12<-c(y12,mean(y12)+100)
  if (length(y13)==0) y13<-c(y13,15000) else y13<-c(y13,mean(y13)+100)
  if (length(y14)==0) y14<-c(y14,15000) else y14<-c(y14,mean(y14)+100)
  if (length(y15)==0) y15<-c(y15,15000) else y15<-c(y15,mean(y15)+100)
  if (length(y16)==0) y16<-c(y16,15000) else y16<-c(y16,mean(y16)+100)
  if (length(y17)==0) y17<-c(y17,15000) else y17<-c(y17,mean(y17)+100)
  if (length(y18)==0) y18<-c(y18,15000) else y18<-c(y18,mean(y18)+100)
  if (length(y19)==0) y19<-c(y19,15000) else y19<-c(y19,mean(y19)+100)
  if (length(y20)==0) y20<-c(y20,15000) else y20<-c(y20,mean(y20)+100)
  if (length(y21)==0) y21<-c(y21,15000) else y21<-c(y21,mean(y21)+100)
  
  library(vioplot)
  vioplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,
          names=c("DD","GAGA","GG","GJ","HF","HH","HGHJ","HJ","JX","JN","NL","NN","NX","QC","QSQS","QQ","SS","WW","XC","XHXH","XX"), 
          col="gold")
  
  title(paste("����ʱ��==",year))
}
years<-c(2011,2013,2014,2015,2016,2017,2018,2019,2020)
par(mfrow=c(3,1))
for (year in years)
  myvioqywz1(anjukefile2,year)
#��������λ��Ϊx����Ʋ�ͬ�����¿���ʱ��Ĳο�����С����ͼ
myvioqywz2<-function(anjukefile2,year){
  anjukefile<-anjukefile2[anjukefile2$zxkp1==year,]
  
  y1<- c(anjukefile$ckjg1[anjukefile$qywz1=='DD'])
  y2<- c(anjukefile$ckjg1[anjukefile$qywz1=='GAGA'])
  y3<- c(anjukefile$ckjg1[anjukefile$qywz1=='GG'])
  y4<- c(anjukefile$ckjg1[anjukefile$qywz1=='GJ'])
  y5<- c(anjukefile$ckjg1[anjukefile$qywz1=='HF'])
  y6<- c(anjukefile$ckjg1[anjukefile$qywz1=='HH'])
  y7<- c(anjukefile$ckjg1[anjukefile$qywz1=='HGHJ'])
  y8<- c(anjukefile$ckjg1[anjukefile$qywz1=='HJ'])
  y9<- c(anjukefile$ckjg1[anjukefile$qywz1=='JX'])
  y10<- c(anjukefile$ckjg1[anjukefile$qywz1=='JN'])
  y11<- c(anjukefile$ckjg1[anjukefile$qywz1=='NL'])
  y12<- c(anjukefile$ckjg1[anjukefile$qywz1=='NN'])
  y13<- c(anjukefile$ckjg1[anjukefile$qywz1=='NX'])
  y14<- c(anjukefile$ckjg1[anjukefile$qywz1=='QC'])
  y15<- c(anjukefile$ckjg1[anjukefile$qywz1=='QSQS'])
  y16<- c(anjukefile$ckjg1[anjukefile$qywz1=='QQ'])
  y17<- c(anjukefile$ckjg1[anjukefile$qywz1=='SS'])
  y18<- c(anjukefile$ckjg1[anjukefile$qywz1=='WW'])
  y19<- c(anjukefile$ckjg1[anjukefile$qywz1=='XC'])
  y20<- c(anjukefile$ckjg1[anjukefile$qywz1=='XHXH'])
  y21<- c(anjukefile$ckjg1[anjukefile$qywz1=='XX'])
  
  
  if (length(y1)==0) y1<-c(y1,15000) else y1<-c(y1,mean(y1)+100)
  if (length(y2)==0) y2<-c(y2,15000) else y2<-c(y2,mean(y2)+100)
  if (length(y3)==0) y3<-c(y3,15000) else y3<-c(y3,mean(y3)+100)
  if (length(y4)==0) y4<-c(y4,15000) else y4<-c(y4,mean(y4)+100)
  if (length(y5)==0) y5<-c(y5,15000) else y5<-c(y5,mean(y5)+100)
  if (length(y6)==0) y6<-c(y6,15000) else y6<-c(y6,mean(y6)+100)
  if (length(y7)==0) y7<-c(y7,15000) else y7<-c(y7,mean(y7)+100)
  if (length(y8)==0) y8<-c(y8,15000) else y8<-c(y8,mean(y8)+100)
  if (length(y9)==0) y9<-c(y9,15000) else y9<-c(y9,mean(y9)+100)
  if (length(y10)==0) y10<-c(y10,15000) else y10<-c(y10,mean(y10)+100)
  if (length(y11)==0) y11<-c(y11,15000) else y11<-c(y11,mean(y11)+100)
  if (length(y12)==0) y12<-c(y12,15000) else y12<-c(y12,mean(y12)+100)
  if (length(y13)==0) y13<-c(y13,15000) else y13<-c(y13,mean(y13)+100)
  if (length(y14)==0) y14<-c(y14,15000) else y14<-c(y14,mean(y14)+100)
  if (length(y15)==0) y15<-c(y15,15000) else y15<-c(y15,mean(y15)+100)
  if (length(y16)==0) y16<-c(y16,15000) else y16<-c(y16,mean(y16)+100)
  if (length(y17)==0) y17<-c(y17,15000) else y17<-c(y17,mean(y17)+100)
  if (length(y18)==0) y18<-c(y18,15000) else y18<-c(y18,mean(y18)+100)
  if (length(y19)==0) y19<-c(y19,15000) else y19<-c(y19,mean(y19)+100)
  if (length(y20)==0) y20<-c(y20,15000) else y20<-c(y20,mean(y20)+100)
  if (length(y21)==0) y21<-c(y21,15000) else y21<-c(y21,mean(y21)+100)
  
  library(vioplot)
  vioplot(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,
          names=c("DD","GAGA","GG","GJ","HF","HH","HGHJ","HJ","JX","JN","NL","NN","NX","QC","QSQS","QQ","SS","WW","XC","XHXH","XX"), 
          col="gold")
  
  title(paste("���¿���ʱ��==",year))
}
years<-c(2011,2012,2013,2014,2015,2016,2017,2018)
par(mfrow=c(4,1))
for (year in years)
  myvioqywz2(anjukefile2,year)
#��������λ�û��Ʋ�ͬ�ο��۸�ĺ��ܶ�ͼ
mydensity3<-function(anjukefile,qywz1){
  anjukefile3<-anjukefile[anjukefile$qywz1==qywz1,]
  #d <- density(anjukefile2$ckjg1)
  #plot(d)
  d <- density(anjukefile3$ckjg1)
  plot(d,xlim=c(0,40000),main=paste("����",qywz1))
  polygon(d, col="red", border="blue")
  rug(anjukefile3$ckjg1, col="brown")
}
par(mfrow=c(5,1))
#qywz1s<-c("DD","GAGA","GG","GJ","HF","HH","HGHJ","HJ","JX","JN","NL","NN","NX","QC","QSQS","QQ","SS","WW","XC","XHXH","XX")
#qywz1s<-c("DD","GAGA","GG","GJ","HF","HH","HGHJ","HJ","JX","JN","NL","NN","NX","QSQS","QQ","SS","WW","XC","XHXH","XX")
for (qywz1 in qywz1s)
  mydensity3(anjukefile2,qywz1)

#���ݽ���ʱ�����С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(jfsj1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.05)

#�������¿���ʱ�����С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(zxkp1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.05)

#��������λ�û���С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(qywz1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)


#�����׸���������С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(sfbl), y=lhl1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)

#���׸�����������ݽ���ʱ�����С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(zxkp1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.05)+
  facet_grid(sfbl~.)

#���׸����������������λ�û���С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(qywz1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)+
  facet_grid(sfbl~.)

#������ʱ������������λ�û���С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(qywz1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)+
  facet_grid(factor(jfsj1)~.)
#������λ�÷�����ݽ���ʱ�����С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(jfsj1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)+
  facet_grid(factor(qywz1)~.)
#�����¿���ʱ������������λ�û���С����ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=factor(qywz1), y=ckjg1)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)+
  facet_grid(factor(zxkp1)~.)
##���ݽ���ʱ����Ʋο����ۺ��ܶ�ͼ
library(ggplot2)
ggplot(data=anjukefile2, aes(x=ckjg1, fill=factor(jfsj1))) +
  geom_density() +
  facet_grid(factor(jfsj1)~.)

#���ݻ���Ϊx�ᣬ�ο��۸�Ϊy�ᣬ�滮����Ϊ���ݴ�С��������ͼ
library(ggplot2)
ggplot(anjukefile1, aes(x=rjl1, y=ckjg1, size=ghhs1)) +
  geom_point(shape=21, color="black", fill="cornsilk") +
  labs(x="rjl1", y="ckjg1",
       title="Bubble Chart", size="ghhs1")
#���ݻ���Ϊx�ᣬ�ο��۸�Ϊy�ᣬΪ����ʱ��Ϊ��ɫ���Ƶ�ͼ
library(ggplot2)
ggplot(anjukefile1, aes(x=rjl1, y=ckjg1, color=factor(jfsj1))) +
  scale_color_manual(values=c(2:10)) +
  geom_point(size=2)
#���ݻ���Ϊx�ᣬ�ο��۸�Ϊy�ᣬ���ƴ��⻬��������ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=rjl1, y=ckjg1)) +
  geom_smooth(method=glm, formula=y~poly(x,3), size=1) +
  geom_point(size=2)
#���ݻ���Ϊx�ᣬ�ο��۸�Ϊy�ᣬ���׸��������ࣨ����ס��סլ���ࣩ���ƴ��⻬��������ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=rjl1, y=ckjg1,color=sfbl)) +
  scale_color_manual(values=c(2:3)) +
  geom_smooth(method=glm, formula=y~poly(x,3), size=1) +
  geom_point(size=2)

#���̻���Ϊx�ᣬ�ο��۸�Ϊy�ᣬ���׸��������ࣨ����ס��סլ���ࣩ���ƴ��⻬��������ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=lhl1, y=ckjg1,color=sfbl)) +
  scale_color_manual(values=c(2:3)) +
  geom_smooth(method=glm, formula=y~poly(x,3), size=1) +
  geom_point(size=2)
#��λ����Ϊx�ᣬ�ο��۸�Ϊy�ᣬ���׸��������ࣨ����ס��סլ���ࣩ���ƴ��⻬��������ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=wcb1, y=ckjg1,color=sfbl)) +
  scale_color_manual(values=c(2:3)) +
  geom_smooth(method=glm, formula=y~poly(x,3), size=1) +
  geom_point(size=2)
#��ҵ������x�ᣬ�ο��۸�Ϊy�ᣬ���׸��������ࣨ����ס��סլ���ࣩ���ƴ��⻬��������ͼ
library(ggplot2)
ggplot(anjukefile2, aes(x=wyglf1, y=ckjg1,color=sfbl)) +
  scale_color_manual(values=c(2:3)) +
  scale_x_continuous(limits=c(0,5)) +
  geom_smooth(method=glm, formula=y~poly(x,3), size=1) +
  geom_point(size=2)



                  









