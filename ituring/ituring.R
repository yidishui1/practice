##设置工作空间
#把“数据及程序”文件夹拷贝到F盘下，再用setwd设置工作空间
setwd("C:/Users/leishen/Documents/R/win-library/practice/ituring")
#数据读取
ituringfire=read.csv('./ituring.csv',he=T)
itur=ituringfire[c("recommend", "reader", "date", "year", "price", "pages", "printing", "state")]
itur1=ituringfire[c("recommend", "reader", "year", "price", "pages")]
myformat <- "%Y/%m/%d"
itur$date <- as.Date(itur$date, myformat)
head(itur)
summary(itur)

library(Hmisc)
describe(itur1)

library(pastecs)
stat.desc(itur1)

library(psych)
describe(itur1)

mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
dstats <- function(x)sapply(x, mystats)
by(itur1, itur1$printing, dstats)
#使用psych包中的describeBy()分组计算概述统计量
library(psych)
describeBy(itur, list(printing=itur$printing))
describeBy(itur, list(state=itur$state))

library(gmodels)
CrossTable(itur$printing, itur$state)
CrossTable(itur$printing, itur$year)


library(ggplot2)
ggplot(itur, aes(x=factor(year), y=reader)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.01)

library(ggplot2)
ggplot(data=itur, aes(x=reader, fill=factor(year))) +
  geom_density() +
  facet_grid(factor(year)~.)


library(ggplot2)
ggplot(itur, aes(x=pages, y=price,color=printing)) +
  scale_color_manual(values=c(2:7)) +
  geom_smooth(method=glm, formula=y~poly(x,2), size=1) +
  geom_point(size=2)



library(ggplot2)
ggplot(itur, aes(x=date, y=reader,color=printing)) +
  scale_color_manual(values=c(2:7)) +
  geom_point(size=2)
#相关分析
itur1=ituringfire[c("recommend", "reader", "year", "price", "pages")]
cov(itur1)
cov(itur1,use="pairwise.complete.obs")
cor(itur1,use="pairwise.complete.obs")
library(corrgram)
corrgram(itur1, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of itur1")
library(corrgram)
corrgram(itur1, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Corrgram of itur1")

library(vcd)
library(grid)
mosaic(~state+printing, data=itur, shade=TRUE, legend=TRUE)
