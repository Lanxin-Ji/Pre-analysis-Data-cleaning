library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("reshape2")
library("R.matlab")
library("reshape")
library("ggalt")

data1 <- readMat("E:/fetal_brain/Norm_Evaluation/32wk_merged_ref/raw_std_wk26.mat")
data2 <- readMat("E:/fetal_brain/Norm_Evaluation/Wkly_merged_ref/raw_std_wk26.mat")

data1 <- lapply(data1, unlist)
data2 <- lapply(data2, unlist)

df <- data.frame(data1,data2)

names(df)[1] <- 'average_tmp'
names(df)[2] <- 'wkly_tmp'
df[df<25]<-NA

df2 <- melt(df, variable.names = 'data',na.rm = TRUE)

names(df2)[1] <- 'tmp_label'

ggbetweenstats(
  data = df2,
  x = tmp_label,
  y = value,
  plot.type = "violin",
  )

gghistostats(
  data = df,
  x = average_tmp,
  binwidth = 1,
  type = "nonparametric")

ggplot(df2, aes(x=value, fill=tmp_label)) +
  geom_histogram(alpha=.6, position="identity") + theme_bw()

plot(density(df2$value, bw = 4), lwd = 2)
  

########################################################################

datafiles <- lapply(Sys.glob("E:/fetal_brain/Norm_Evaluation/32wk_merged_ref/raw_std_wk*.mat"), readMat)
df_32wk <- data.frame(datafiles[[i]]$Data)
names(df_32wk)[1] <- 'V1'
for (i in 1:12){
  datafiles[[i]]$Data[datafiles[[i]]$Data<25] <- NA
  print(mean(datafiles[[i]]$Data, na.rm = TRUE))
  df_32wk[,i] <-datafiles[[i]]$Data
  names(df_32wk)[i] <- paste('wk',i+25)
}
df_32wk_melt <- melt(df_32wk, variable.names = 'data',na.rm = TRUE)
#names(df_32wk_melt)[2] <- '32wk_tmp'

rm(df_32wk)


datafiles <- lapply(Sys.glob("E:/fetal_brain/Norm_Evaluation/wkly_merged_ref/raw_std_wk*.mat"), readMat)
df_wkly <- data.frame(datafiles[[1]]$Data)
for (i in 1:12){
  datafiles[[i]]$Data[datafiles[[i]]$Data<25] <- NA
  print(mean(datafiles[[i]]$Data, na.rm = TRUE))
  df_wkly[,i] <-datafiles[[i]]$Data
  names(df_wkly)[i] <- paste('wk',i+25)
}
df_wkly_melt <- melt(df_wkly, variable.names = 'data',na.rm = TRUE)
#names(df_wkly_melt)[2] <- 'wkly_tmp'
rm(df_wkly)

ggplot(df_32wk_melt, aes(x = variable, y = value)) + 
  geom_point() + theme_bw() + ylim(25, 400)
ggplot(df_wkly_melt, aes(x = variable, y = value)) + 
  geom_point() + theme_bw() + ylim(25, 400)

ggplot() +
  geom_point(data = df_32wk_melt, aes(x = variable, y = value), color = 'green') +
  geom_point(data = df_wkly_melt, aes(x = variable, y = value), color = 'red')
  