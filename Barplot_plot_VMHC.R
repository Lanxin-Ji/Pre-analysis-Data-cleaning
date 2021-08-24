library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("reshape2")

setwd("E:/fetal_brain")
dir()

data<-read.xlsx("Lanxin_subject_selection_file_Preproc.xlsx",sheetIndex = 3)
View(data)
class(data)   #Data Frame
dim(data) # [179,57], but the actual size = 171. 8 NAs at the end
summary(data)

df2 <- melt(data, measure.vars = c("Mean.VMHC.con","Mean.VMHC.agg"), variable.names = 'data')
names(df2)[5] <- 'label'
names(df2)[6] <- 'VMHCmean'

df2$Target.template[df2$Target.template==31] <- 30
df2$Target.template <- factor(df2$Target.template)
ggplot(df2, aes(y=VMHCmean, x=Target.template, fill=label)) +
  geom_violin(position="dodge", alpha=0.5) + theme_bw()

df3 <- melt(data, measure.vars = c("Mean.VMHC.agg","Mean.VMHC.agg.2mm","Mean.VMHC.agg.4mm"), variable.names = 'VMHCmean')
names(df3)[4] <- 'label'
names(df3)[5] <- 'VMHCmean'

names(df3)[4] <- 'label'
df3$Target.template[df3$Target.template==31] <- 30
df3$Target.template <- factor(df3$Target.template)
ggplot(df3, aes(y=value, x=Target.template, fill=label)) +
  geom_violin(position="dodge", alpha=0.5) + theme_bw()

# res_df <- aggregate(Mean.VMHC.con ~ Target.template, df, mean)
data<-read.xlsx("Lanxin_subject_selection_file_Preproc.xlsx",sheetIndex = 5)
df2 <- melt(data, id.vars = "Target.template", measure.vars = c("Mean.VMHC.con","Mean.VMHC.agg"), variable.names = 'data')
df3 <- melt(data, id.vars = "Target.template", measure.vars = c("Sd.VMHC.con","Sd.VMHC.agg"), variable.names = 'data')
df2$sd <- df3$value
rm(df3)

names(df2)[2] <- 'label'
ggplot(data=df2, aes(fill=label, y=value, x=Target.template)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.6, alpha = 1) + 
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks =c(26:37)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd),width=.4, position = position_dodge(.6))

df2 <- melt(data, id.vars = "Target.template", measure.vars = c("Mean.VMHC.agg", "Mean.VMHC.agg.2mm", "Mean.VMHC.agg.4mm"), variable.names = 'data')
df3 <- melt(data, id.vars = "Target.template", measure.vars = c("Sd.VMHC.agg", "Sd.VMHC.agg.2mm", "Sd.VMHC.agg.4mm"), variable.names = 'data')
df2$sd <- df3$value
rm(df3)

names(df2)[2] <- 'label'
ggplot(data=df2, aes(fill=label, y=value, x=Target.template)) +
  geom_bar(stat="identity", position = position_dodge(), width = 0.8, alpha = 1) + 
  theme_bw() +
  scale_fill_brewer(palette =4) +
  scale_x_continuous(breaks =c(26:37)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd),width=.4, position = position_dodge(.8))


############################################
data<-read.xlsx("Lanxin_subject_selection_file_Preproc.xlsx",sheetIndex = 3)
ggscatterstats(
  data = data,
  x = Mean.VMHC.agg,
  y = PYR.max_32wk,
  xlab = "Mean.VMHC.agg",
  ylab = "PYR.max",
)
