library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("reshape2")
library("plyr")

setwd("E:/fetal_brain")
dir()
data<-read.xlsx("Lanxin_subject_selection_file_Preproc.xlsx",sheetIndex = 1)

data$Target.template[data$Target.template==31] <- 30

#plotdata <- data[data$Target.template == 26, ]
df2 <- melt(data, measure.vars = c("PYR.mean_32wk","PYR.mean_wkly"), variable.names = 'data')
names(df2)[13] <- 'tmp_label'

df2$Target.template <- factor(df2$Target.template)
ggplot(df2, aes(fill=tmp_label, y=value, x=Target.template)) +
         geom_violin(position="dodge", alpha=0.5) + theme_bw()

