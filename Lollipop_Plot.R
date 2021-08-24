library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("dplyr")
library("hrbrthemes")


setwd("E:/fetal_brain")
dir()
data<-read.xlsx("Lanxin_subject_selection_file_Preproc.xlsx",sheetIndex = 3)

data1 <- data.frame(
  x=1:121,
  value1 = data$No.IC.raw,
  value2 = data$No.IC.mas
)

data1 <- data1 %>%
  rowwise() %>%
  mutate(mymean = mean(c(value1, value2))) %>%
  arrange(mymean) %>%
  mutate(x=factor(x,x))
 
ggplot(data1) +
  geom_segment(aes(y=x, yend=x, x=value1, xend=value2), color="grey") +
  geom_point(aes(y=x, x=value1), color=rgb(0.2,0.7,0.1,0.5), size=3) +
  geom_point(aes(y=x, x=value2), color=rgb(0.7,0.2,0.1,0.5), size=3) +
  coord_flip() +
  theme_ipsum() +
  theme(
    legend.position="none",
  ) + theme_bw()
