setwd("E:/E-BOOK/STA 260/")
library(ggplot2)
library(dplyr)
library(stringr)
library(lme4)
library(ggthemes)

data = read.csv("Compost_StatsClass_021521[26705].csv")
data = filter(data, N.min.amd.pct >= 0)
data$CNratio = as.numeric(str_extract_all(data$Soil,"[0-9.]+[0-9]",simplify = TRUE))

#Each curve is specified by Curve_ID, Amendment, Soil, Temperature and WHC
#New curve ID is IDD
df <- data %>% 
  mutate(IDD = group_indices(.,Curve_ID,Amendment,Soil,Temperature,WHC)) %>%
  group_by(IDD) %>%
  arrange(Time,.by_group = TRUE) %>%
  filter(n()>3) #%>%
#  filter(N.min.amd.pct[length(N.min.amd.pct)]>=N.min.amd.pct[length(N.min.amd.pct)-1])

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
    ggtitle(paste("IDD",df_i$IDD,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
    ylab("Nmin")
  print(pp)
}
#delete outliers for each curve
df_clean = df %>%
  filter(  !((IDD == 3 & Time %in% c(2,3)) |
           (IDD == 4 & Time %in% c(3,8))|
           (IDD == 42 & round(Time,2) %in% c(7.14))|
           (IDD == 43 & round(Time,1) %in% c(13.9))|
           (IDD == 44 & round(Time,2) %in% c(1.43))|
           (IDD == 45 & round(Time,2) %in% c(13.86))|
           (IDD == 55 & Time %in% c(16))|
           (IDD == 58 & Time %in% c(8))|
           (IDD == 80 & Time %in% c(12)) |
           (IDD == 81 & Time %in% c(12)) |
           (IDD == 89 & Time %in% c(3)) | 
           (IDD == 90 & Time %in% c(1)) |
           (IDD == 91 & Time %in% c(14))| 
           (IDD %in% c(16,50,41,42,44,64,72,75)))) %>%
  filter(n()>3)

#discuss?
c(16, 41, 50, 64, 72,75)


#calculate N0 and k for cleaned data
N0k = data.frame()
for(i in unique(df_clean$IDD)){
  df_i = df_clean[which(df_clean$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(max(Nim),1),loss)
  N0k = rbind(N0k,opt$par)
}
colnames(N0k) = c("N0","k")
#organized data(N0 & k included): nSamples is number of samples in the curve
df_Nk = cbind(df_clean[!duplicated(df_clean[c("Temperature","WHC","IDD")]),][c(1:5,8:9)],N0k)
df_Nk$nSamples = aggregate(numeric(nrow(df_clean)),df_clean[c("IDD")],length)[,2]
#merge df_clean and df_Nk
df_merge = df_clean %>%
  merge(df_Nk) %>%
  mutate(Nmin.Nmax = N.min.amd.pct/(max(N.min.amd.pct)+0.05)) %>%
  mutate(across(is.character, as.factor))
df_merge$Temperature =factor(df_merge$Temperature)
df_merge$WHC =factor(df_merge$WHC)
df_merge$ID = factor(df_merge$ID)
df_merge$Curve_ID = factor(df_merge$Curve_ID)
df_merge$CNratio = factor(df_merge$CNratio)

#Time VS log(1-Nmin/max(Nmin)) for each IDD and grouped by Temprature/WHC/CNratio/Soil/Amendments/ID/Curve_ID
ggplot(data = df_merge,aes(Time,log(1-Nmin.Nmax)))+
  geom_line(aes(colour = ID,group=IDD),size = 1)+
  geom_point(aes(colour = ID,group=IDD),size = 1)
#Temperature 23, 25, 30
df_merge_23.25.30 = filter(df_merge,Temperature %in% c(23,25,30))
ggplot(data = df_merge_23.25.30,aes(Time,log(1-Nmin.Nmax)))+
  geom_line(aes(colour = Temperature,group=IDD),size = 1,alpha = 0.5)+
  geom_point(aes(colour = Temperature,group=IDD),size = 1)

#Scatter plot of Temperature VS log(k_hat)
plot((df_Nk$Temperature),log(df_Nk$k),xlab = "Temp",ylab = "k_hat")
#Scatter plot of CNratio VS log(k_hat)
plot(round(df_Nk$CNratio,1),log(df_Nk$k),xlab = "CNratio",ylab = "k_hat")
#Scatter plot of WHC VS log(k_hat)
plot(df_Nk$WHC,log(df_Nk$k),xlab = "WHC",ylab = "k_hat")
#Scatter plot of CNratio VS N0 
plot(round(df_Nk$CNratio,1),log(df_Nk$N0),xlab = "CNratio",ylab = "N0")
boxplot(df_Nk$N0~df_Nk$CNratio)


#anova model: k_hat ~ Temperature*WHC + error:
fit_aov.TW = aov(log(k)~Temperature*WHC,df_Nk)
summary(fit_aov.TW)

#fit lmm model: k_hat ~ Temperature + WHC + 1|Soil + error
fit_lmm = lmer(log(k)~ WHC + Temperature-1+(1|Soil), df_Nk)
summary(fit_lmm)




#plot for certain group
for(i in unique(df_clean$IDD)){
  df_i = df_clean[which(df_clean$IDD ==i),]
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





