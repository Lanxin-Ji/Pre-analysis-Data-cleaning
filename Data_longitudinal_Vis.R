#install.packages("xlsx")
#install.packages("ggplot2")
#install.packages("ggstatsplot")
#install.packages("rmcorr")
library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("reshape2")
library("rmcorr")

setwd("C:/Users/tnice/Desktop/ncrc_desktop/Dongning_PD")
dir()

data<-read.xlsx("Cognition.xlsx",sheetIndex = 1)
View(data)
class(data)   #Data Frame
dim(data) # [179,57], but the actual size = 171. 8 NAs at the end
summary(data)

tmp2 <- data[c("PATNO","moca_BL","moca_1y","moca_2y","moca_3y","moca_4y","moca_5y")]
tmp2 <- melt(tmp2, id.vars = 'PATNO', variable.names = 'series')
unique(tmp2$variable)
########### "variable" as vol name is not allowed
names(tmp2)[names(tmp2) == "variable"] <- "time"

tmp2$time <- factor(tmp2$time,
                    levels = c("moca_BL","moca_1y","moca_2y","moca_3y","moca_4y","moca_5y"),
                    labels = c(0,1,2,3,4,5))

ggbetweenstats(
  data = tmp2,
  x = time,
  y = value,
  title = "MoCA Changes"
)

tmp2$time <- as.numeric(tmp2$time)
my_rmc <- rmcorr(participant = "PATNO",measure1 = "time",measure2 = "value",dataset = tmp2)
plot(my_rmc, overall = TRUE)

################################################################################
#Benton Visual Spatial
data<-read.xlsx("Cognition.xlsx",sheetIndex = 2)
colnames(data)

tmp2 <- data[c("PATNO","BL_Benton.Judgment.of.Line.Orientation","X1y_Benton.Judgment.of.Line.Orientation",
               "X2y_Benton.Judgment.of.Line.Orientation","X3y_Benton.Judgment.of.Line.Orientation",
               "X4y_Benton.Judgment.of.Line.Orientation","X5y_Benton.Judgment.of.Line.Orientation")]
tmp2 <- melt(tmp2, id.vars = 'PATNO', variable.names = 'series')
unique(tmp2$variable)
names(tmp2)[names(tmp2) == "variable"] <- "time"

tmp2$time <- factor(tmp2$time,
                    levels = c("BL_Benton.Judgment.of.Line.Orientation","X1y_Benton.Judgment.of.Line.Orientation",
                               "X2y_Benton.Judgment.of.Line.Orientation","X3y_Benton.Judgment.of.Line.Orientation",
                               "X4y_Benton.Judgment.of.Line.Orientation","X5y_Benton.Judgment.of.Line.Orientation"),
                    labels = c(0,1,2,3,4,5))


ggbetweenstats(
  data = tmp2,
  x = time,
  y = value,
  title = "Visual_Spatial Changes"
)

tmp2$time <- as.numeric(tmp2$time)
my_rmc <- rmcorr(participant = "PATNO",measure1 = "time",measure2 = "value",dataset = tmp2)
plot(my_rmc, overall = TRUE)

################################################################################
#Benton Visual Spatial
data<-read.xlsx("Cognition.xlsx",sheetIndex = 3)
colnames(data)

tmp2 <- data[c("PATNO","BL_Semantic.Fluency","X1y_Semantic.Fluency",
               "X2y_Semantic.Fluency","X3y_Semantic.Fluency",
               "X4y_Semantic.Fluency","X5y_Semantic.Fluency")]
tmp2 <- melt(tmp2, id.vars = 'PATNO', variable.names = 'series')
unique(tmp2$variable)
names(tmp2)[names(tmp2) == "variable"] <- "time"

ggbetweenstats(
  data = tmp2,
  x = time,
  y = value,
  title = "Fluency Changes"
)

################################################################################
#Symbol_Digit
data<-read.xlsx("Cognition.xlsx",sheetIndex = 4)
colnames(data)

tmp2 <- data[c("PATNO","BL_Symbol.Digit.Modalities.Test","X1y_Symbol.Digit.Modalities.Test",
               "X2y_Symbol.Digit.Modalities.Test","X3y_Symbol.Digit.Modalities.Test",
               "X4y_Symbol.Digit.Modalities.Test","X5y_Symbol.Digit.Modalities.Test")]
tmp2 <- melt(tmp2, id.vars = 'PATNO', variable.names = 'series')
unique(tmp2$variable)
names(tmp2)[names(tmp2) == "variable"] <- "time"

ggbetweenstats(
  data = tmp2,
  x = time,
  y = value,
  title = "Symbol_Digit"
)


################################################################################
#HVLT_Imm
data<-read.xlsx("Cognition.xlsx",sheetIndex = 5)
colnames(data)

tmp2 <- data[c("PATNO","BL_HVLT.Immediate.Total.Recall","X1y_HVLT.Immediate.Total.Recall",
               "X2y_HVLT.Immediate.Total.Recall","X3y_HVLT.Immediate.Total.Recall",
               "X4y_HVLT.Immediate.Total.Recall","X5y_HVLT.Immediate.Total.Recall")]
tmp2 <- melt(tmp2, id.vars = 'PATNO', variable.names = 'series')
unique(tmp2$variable)
names(tmp2)[names(tmp2) == "variable"] <- "time"

ggbetweenstats(
  data = tmp2,
  x = time,
  y = value,
  title = "HVLT_Imm"
)


