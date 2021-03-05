setwd("E:/E-BOOK/STA 260/")
library(ggplot2)
library(dplyr)

data = read.csv("Compost_StatsClass_021521[26705].csv")
data = data[-which(data$N.min.amd.pct < 0),]

#Each curve is specified by Curve_ID, Amendment, Soil, Temperature and WHC
#New curve ID is IDD

df <- data %>% 
  mutate(IDD = group_indices(.,Curve_ID,Amendment,Soil,Temperature,WHC)) 
table(df$IDD)

#delete IDD #7 because there is one one sample for the curve
df = df[-which(df$IDD %in% c(7,17,18,19,20:39,46,49,59,63,65:67,83:88)),]
View(df)


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
    ggtitle(paste("IDD",i,(df_i$Amendment),df_i$Soil,"TEMP",df_i$Temperature,"WHC",df_i$WHC))+
    ylab("Nmin")
  print(pp)
}
#outliers
c(89,88,58,45,44,44)
#discussion
c(75,72,64,50)

#organized data(N0 & k included): nSamples is number of samples in the curve
aggregate(numeric(nrow(df)),df[c("IDD")],length)
colnames(N0k) = c("N0","k")
df_Nk = cbind(df[!duplicated(df[c("Temperature","WHC","IDD")]),][c(3:5,8:9)],N0k)
df_NK = df_Nk[c("IDD","Temperature","WHC","Amendment","Soil","N0","k")]
df_Nk$nSamples = aggregate(numeric(nrow(df)),df[c("IDD")],length)[,2]
rownames(df_Nk) = df_Nk$IDD
df_Nk
#cbind(df_Nk[,seq(4)],round(df_Nk[,c(7)],3))
plot(df_Nk$Temperature,log(df_Nk$k))

boxplot(log(df_Nk$k)~df_Nk$Temperature)

#?????
#negative ones:
neg_IDD = c(17,19,24,26,27,28,29,30,31,32,44,48,52,53,60,61,65,67,68,69,72,74,70,87,91,93)
length(neg_IDD)
#strange ones:
stra_IDD = c(3,4,19,20,22,23,39,40,41,46,34,66,77,80,88,89,92)
length(sta_IDD)
df[which(df$IDD %in% sta_IDD),]
#normal ones:outliers are contained
norm_IDD = c(1,2,5,6,8,9,10,11,12,13,14,15,33,34,35,36,38,42,43,45,49,50,54,55,56,57,58,59,62,63,64,71,73,75,76,78,79,81,82,83,84,85,90,94,95)
length(norm_IDD)
#bad or okay?
notsure_IDD = c(16,18,21,25,37,47,86,96)

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
T:23 c(75, 72, 64)
df_Nk
