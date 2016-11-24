files<-dir("C:/寻优结果")
data<-read.csv(paste("C:/寻优结果/",files[1],sep=''))
head(data)



connection<-function(data.new){
k<-c(108,117,122,131,140,150,161,167,175,185,196,207,219,229)
d<-c(451,536,609,690,797,860,956)

for(i in 1:(length(d)-1)){
if(data.new$主汽流量>=d[i]&data.new$主汽流量<=d[i+1]){
d.d<-i
}
}
d.k<-1
for(j in 1:(length(k)-1)){
if(data.new$总煤量>=k[j]&data.new$总煤量<=k[j+1]){
d.k<-j
}
}

zuhe<-NULL
for(z in LETTERS[1:5]){
if(data.new[z]>=25){
zuhe<-paste(zuhe,z,sep='')
}
}

result<-c(d.k,d.d,zuhe)
return(result)
}

data_test<-read.csv(file.choose())##测试数据
data_test<-na.omit(data_test)
last<-c()
test<-c()
head(data_test)
for(r in 1:25000){
data.new<-data_test[r,]
cund<-connection(data.new)
if(length(intersect(paste(cund[3],".csvbest.csv",sep=''),dir("D:/寻优结果")))==1){
data<-read.csv(paste("D:/寻优结果/",cund[3],".csvbest.csv",sep=''))

k<-c(108,117,122,131,140,150,161,167,175,185,196,207,219,229)
d<-c(451,536,609,690,797,860,956)
if(length(intersect(paste(k[as.numeric(cund[1])],"-",k[as.numeric(cund[1])+1],",",d[as.numeric(cund[2])],"-",d[as.numeric(cund[1])+1],cund[3]," .csv",sep=''),dir("D:/珲春工况")))==1){
DA<-read.csv(paste("D:/珲春工况/",k[as.numeric(cund[1])],"-",k[as.numeric(cund[1])+1],",",d[as.numeric(cund[2])],"-",d[as.numeric(cund[2])+1],cund[3]," .csv",sep=''))
##jia,max,min
nox.max<-max((DA[,"nox进口A"]+DA[,"NOX.B"])/2)
nox.min<-min((DA[,"nox进口A"]+DA[,"NOX.B"])/2)
wwt.max<-max(DA[,"再热器温度"])
wwt.min<-min(DA[,"再热器温度"])
rt.max<-max(DA[,"排烟温度"])
rt.min<-min(DA[,"排烟温度"])
temp<-subset(data,V1==as.numeric(cund[1])&V2==as.numeric(cund[2]))
names(temp)<-c(names(temp)[1:19],names(data.new)[29:41])
if(nrow(temp)>0){
count<-rep(0,nrow(temp))
for(i in seq(nrow(temp))){
if(data.new["YL"]>=temp[i,"YL"]&data.new["YL"]<=(temp[i,"YL"]+0.2)) count[i]<-count[i]+1
 

for(j in names(data.new)[29:41]){
if(data.new[j]>=temp[i,j]&data.new[j]<=(temp[i,j]+2)) count[i]<-count[i]+1
}
}
best<-temp[which(count==max(count))[1],]
last<-rbind(last,best)
test<-rbind(test,cbind(data.new,nox.max,nox.min,wwt.max,wwt.min,rt.max,rt.min))
}
}
}
}
dim(test)
dim(last)
write.csv(test,"D:/TEST.csv",row.names=F)
write.csv(last,"D:/LAST.csv",row.names=F)
X.nox<-((test[,"nox进口A"]+test[,"NOX.B"])/2-test$nox.min)/(test$nox.max-test$nox.min)
X.wwt<-(test[,"再热器温度"]-test$wwt.min)/(test$wwt.max-test$wwt.min)
X.rt<-(test[,"排烟温度"]-test$rt.min)/(test$rt.max-test$rt.min)
X.test<-0.5*X.wwt-0.4*(1-X.nox)-0.1*(1-X.rt)
plot(X.test,type="l",col="green")
X.last<-0.5*((0.000001+last$mean_wt-last$MI_wt)/(0.000001+last$MA_wt-last$MI_wt))+0.4*(1-((0.000001+last$mean_nox-last$MI_nox)/(0.000001+last$MA_nox-last$MI_nox)))+0.1*(1-((0.000001+last$mean_rt-last$MI_rt)/(0.000001+last$MA_rt-last$MI_rt)))
lines(X.last,type="l",col="red")
title("测试数据打分方法寻优结果对比")
