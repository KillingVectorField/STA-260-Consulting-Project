setwd("E:/E-BOOK/STA 260/")
library(ggplot2)
library(dplyr)
library(stringr)

data = read.csv("Compost_StatsClass_021521[26705].csv")
data = filter(data, N.min.amd.pct >= 0)

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
           (IDD == 58 & Time %in% c(8))|
           (IDD == 80 & Time %in% c(12)) |
           (IDD == 81 & Time %in% c(12)) |
           (IDD == 89 & Time %in% c(3)) | 
           (IDD == 90 & Time %in% c(1)) |
           (IDD == 91 & Time %in% c(14))) )

#discuss?
c(16, 41, 50, 64, 72,75)


#calculate N0 and k for cleaned data
N0k = data.frame()
for(i in unique(df_clean$IDD)){
  df_i = df_clean[which(df_clean$IDD ==i),]
  df_i = df_i[order(df_i$Time),]
  Nim = df_i$N.min.amd.pct
  t = df_i$Time
  opt = optim(c(Nim[length(t)],1),loss)
  N0k = rbind(N0k,opt$par)
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
colnames(N0k) = c("N0","k")
df_Nk = cbind(df_clean[!duplicated(df_clean[c("Temperature","WHC","IDD")]),][c(1:5,8:9)],N0k)
df_NK = df_Nk[c("ID","Curve_ID","IDD","Temperature","WHC","Amendment","Soil","N0","k")]
df_Nk$nSamples = aggregate(numeric(nrow(df_clean)),df_clean[c("IDD")],length)[,2]

#Relationships between T and k
plot(df_Nk$Temperature,log(df_Nk$k))
plot(df_Nk$Temperature,(df_Nk$k))
boxplot(log(df_Nk$k)~df_Nk$Temperature)

#Relationships between WHC and k
plot(df_Nk$WHC,log(df_Nk$k))
plot(df_Nk$WHC,(df_Nk$k))


#plot for certain group
for(i in unique(df_clean$IDD[which(df_clean$Temperature == 25)])){
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

#Temp 15: IDD 44 seems missing points between time 0 and time 5 --> extreme large k

#Temp 23: IDD 64 72 75 ? 

test = filter(df_Nk, !IDD %in% c(44,64,72,75))
test$CNratio = as.numeric(str_extract_all(test$Soil,"[0-9.]+[0-9]",simplify = TRUE))

plot(test$Temperature,log(test$k))
boxplot(log(test$k)~test$Amendment)
plot(test$CNratio,log(test$k))
boxplot(log(test$k)~test$CNratio)



