setwd("/Users/yuekang/Documents/GitHub/STA-260-Consulting-Project/")
library(ggplot2)
library(dplyr)
library(stringr)
library(lme4)

rdata = read.csv("Compost_StatsClass_021521.csv")
rdf <- rdata %>% 
  mutate(IDD = group_indices(.,Curve_ID,Amendment,Soil,Temperature,WHC)) 
rdf <- rdf[-which(rdf$IDD == 7),]
for(i in c(93,70,90,91)){
  rdf_i = rdf[which(rdf$IDD ==i),]
  rdf_i = rdf_i[order(rdf_i$Time),]
  Nim = rdf_i$N.min.amd.pct
  t = rdf_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  N0k = rbind(N0k,opt$par)
  pp <- ggplot() + 
    xlim(0, max(t))+ 
    geom_function(fun = fn(opt$par)) + 
    geom_point(aes(x=t,y=Nim),col = "blue") + 
    ggtitle(paste("IDD",i,(rdf_i$Amendment),rdf_i$Soil,"TEMP",rdf_i$Temperature,"WHC",rdf_i$WHC))+
    ylab("Nmin")
  print(pp)
}



data = rdata[-which(data$N.min.amd.pct < 0),]

for(i in c(55,47)){
  df_i = df[which(df$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  N0k = rbind(N0k,opt$par)
  pp <- ggplot() + 
    xlim(0, max(t))+ 
    geom_function(fun = fn(opt$par)) + 
    geom_point(aes(x=t,y=Nim),col = "blue") + 
    ggtitle(paste("IDD",i,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
    ylab("Nmin")
  print(pp)
}


#Each curve is specified by Curve_ID, Amendment, Soil, Temperature and WHC
#New curve ID is IDD

df <- data %>% 
  mutate(IDD = group_indices(.,Curve_ID,Amendment,Soil,Temperature,WHC)) 
table(df$IDD)
ind <- as.data.frame(table(df$IDD))$Var1[as.data.frame(table(df$IDD))$Freq < 4]
#delete IDD #7 because there is one one sample for the curve
df = df[-which(df$IDD %in% ind),]
View(df)

#delete outliers for each curve
#model function and loss function
fn = function(par)function(t){
  par[1]*(1-exp(-par[2]*t))
}

loss = function(x){
  loss = 0
  for (i in seq(t)) {
    
    loss = (fn(x)(t[i])-Nim[i])^2+loss
  }
  return(loss)
}

#plot for all curves and calculate N0, k for each curve
N0k = data.frame()
for(i in unique(df$IDD)){
  #for(i in c(75,72,64,50)){
  df_i = df[which(df$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  N0k = rbind(N0k,opt$par)
  pp <- ggplot() + 
    xlim(0, max(t))+ 
    geom_function(fun = fn(opt$par)) + 
    geom_point(aes(x=t,y=Nim),col = "blue") + 
    ggtitle(paste("IDD",i,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
    ylab("Nmin")
  print(pp)
}

#organized data(N0 & k included): nSamples is number of samples in the curve
fullN0k <- N0k
aggregate(numeric(nrow(df)),df[c("IDD")],length)
colnames(fullN0k) = c("N0","k")
df_Nk = cbind(df[!duplicated(df[c("Temperature","WHC","IDD")]),][c(3:5,8:9)],fullN0k)
df_NK = df_Nk[c("IDD","Temperature","WHC","Amendment","Soil","N0","k")]
df_Nk$nSamples = aggregate(numeric(nrow(df)),df[c("IDD")],length)[,2]
rownames(df_Nk) = df_Nk$IDD
df_Nk
#cbind(df_Nk[,seq(4)],round(df_Nk[,c(7)],3))
plot(df_Nk$Temperature,df_Nk$k)
plot(df_Nk$Temperature,log(df_Nk$k))

plot(df_Nk$WHC,df_Nk$k)
plot(df_Nk$WHC,log(df_Nk$k))

##### Data Cleaning

df_clean = df %>%
  filter(  !((IDD == 3 & Time %in% c(2,3)) |
               (IDD == 4 & Time %in% c(3,8))|
               (IDD == 42 & round(Time,2) %in% c(7.14))|
               (IDD == 43 & round(Time,1) %in% c(13.9))|
               (IDD == 44 & round(Time,2) %in% c(1.43))|
               (IDD == 45 & round(Time,2) %in% c(13.86))|
               (IDD == 58 & Time %in% c(8))|
               (IDD == 80 & Time %in% c(12)) |
               (IDD == 81 & Time %in% c(12)) |
               (IDD == 89 & Time %in% c(3)) | 
               (IDD == 90 & Time %in% c(1)) |
               (IDD == 91 & Time %in% c(14))) )

#discuss?
c(16, 41, 50, 64, 72,75)


N0k_new = data.frame()
for(i in unique(df_clean$IDD)){
  df_i = df_clean[which(df_clean$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  N0k_new = rbind(N0k_new,opt$par)
  #pp <- ggplot() + 
  #  xlim(0, max(t))+ 
  #  geom_function(fun = fn(opt$par)) + 
  #  geom_point(aes(x=t,y=Nim),col = "blue") + 
  #  ggtitle(paste("IDD",df_i$IDD,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
  #  ylab("Nmin")
  #print(pp)
}


#organized data(N0 & k included): nSamples is number of samples in the curve
aggregate(numeric(nrow(df_clean)),df_clean[c("IDD")],length)
colnames(N0k_new) = c("N0","k")
df_Nk_new = cbind(df_clean[!duplicated(df_clean[c("Temperature","WHC","IDD")]),][c(1:5,8:9)],N0k_new)
df_NK_new = df_Nk_new[c("ID","Curve_ID","IDD","Temperature","WHC","Amendment","Soil","N0","k")]
df_Nk_new$nSamples = aggregate(numeric(nrow(df_clean)),df_clean[c("IDD")],length)[,2]
df_Nk_new

plot(df_Nk_new$Temperature,df_Nk_new$k)
plot(df_Nk_new$Temperature,log(df_Nk_new$k))

plot(df_Nk_new$WHC,df_Nk_new$k)
plot(df_Nk_new$WHC,log(df_Nk_new$k))


df_Nk_new$IDD[df_Nk_new$Temperature > 15]


for(i in c(16,44,64,72,75)){
  df_i = df_clean[which(df_clean$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  pp <- ggplot() +
    xlim(0, max(t))+
    geom_function(fun = fn(opt$par)) +
    geom_point(aes(x=t,y=Nim),col = "blue") +
    ggtitle(paste("IDD",df_i$IDD,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
    ylab("Nmin")
  print(pp)
}


boxplot(log(df_Nk$k)~df_Nk$Temperature)


#plot for certain group
for(i in unique(df$IDD[which(df$Temperature == 23)])){
  df_i = df[which(df$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  pp <- ggplot() + 
    xlim(0, max(t))+ 
    geom_function(fun = fn(opt$par)) + 
    geom_point(aes(x=t,y=Nim),col = "blue") + 
    ggtitle(paste("IDD",i,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))
  print(pp)
}
# T:23 c(75, 72, 64)


test = filter(df_Nk_new, !IDD %in% c(16,44,64,72,75))
test$CNratio = as.numeric(str_extract_all(test$Soil,"[0-9.]+[0-9]",simplify = TRUE))

plot(test$Temperature,log(test$k))
boxplot(log(test$k)~test$Amendment)

plot(test$WHC,log(test$k))
boxplot(log(test$k)~test$WHC)

plot(test$CNratio,log(test$k))
boxplot(log(test$k)~test$CNratio)

test <- test[-which(is.na(test$WHC)),]


aov.fit1 <- aov(data = test, log(k) ~ Temperature+WHC+Temperature:WHC)
summary(aov.fit1)


test$Temperature = as.factor(test$Temperature)
test$WHC = as.factor(test$WHC)
test$Soil = as.factor(test$Soil)
glm.fit1 = lmer(log(k)~ WHC + Temperature + WHC:Temperature -1+(1|Soil), test)
summary(glm.fit1)





