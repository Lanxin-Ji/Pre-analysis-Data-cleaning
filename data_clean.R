#install.packages("xlsx")
#install.packages("ggplot2")
#install.packages("ggstatsplot")
library("xlsx")
library("ggplot2")
library("ggstatsplot")
library("reshape2")

setwd("C:/Users/tnice/Desktop/ncrc_desktop/NYU/fetal_dFNC")
dir()

data<-read.xlsx("Lanxin_subject_selection_file.xlsx",sheetIndex = 1)
View(data)
class(data)   #Data Frame
dim(data) # [179,57], but the actual size = 171. 8 NAs at the end
summary(data)

data[data==999]<-NA

# Code 1 = special note; missing values on variables of interest
data$LJ_inclusion_exclusion_code[!is.na(data$X.special_health_or_other_note)] <- 1
data$LJ_inclusion_exclusion_code[is.na(data$sex)] <- 1
summary(data$LJ_inclusion_exclusion_code) #n = 166

# Code 2 = health concern (early age birth; low birth weight)
data$LJ_inclusion_exclusion_code[data$X.Gestational_Age_birth<33|is.na(data$X.Gestational_Age_birth)|
                                   data$X.Birth_weight<1800|is.na(data$X.Birth_weight)] <-2
summary(data$LJ_inclusion_exclusion_code) #n = 152

# Code 3 = too young at scan

# code 4 = high motion
data$LJ_inclusion_exclusion_code[data$X.Weighted_Avg_XYZ_drift>1.5|
                                   data$X.Weighted_Avg_PYR_drift>2|
                                   data$X.Weighted_Avg_PYR_mean>1] <- 4
data$LJ_inclusion_exclusion_code[is.na(data$X.Weighted_Avg_XYZ_drift)] <- 4
summary(data$LJ_inclusion_exclusion_code) #n = 135

## set LJ_code = 0 for included subject
data$LJ_inclusion_exclusion_code[is.na(data$LJ_inclusion_exclusion_code)]<-0
data$LJ_inclusion_exclusion_code_binary[data$LJ_inclusion_exclusion_code==0] <- 0
data$LJ_inclusion_exclusion_code_binary[data$LJ_inclusion_exclusion_code!=0] <- 1

#write.xlsx(data,"Lanxin_subject_selection_file_0325.xlsx")

################################################################## Plot 
data <- subset(data, LJ_inclusion_exclusion_code==0)
dim(data) # [135,57]

ggbetweenstats(
  data = data,
  x = sex,
  y = X.Gestational_Age_scan,
  title = "scan age by sex"
)

ggbetweenstats(
  data = data,
  x = income_recoded,
  y = X.Gestational_Age_scan,
  title = "scan age by income"
)

ggscatterstats(
  data = data,
  x = X.Gestational_Age_scan,
  y = Maternal_Race,
  xlab = "Age of scan (weeks)",
  ylab = "race",
  title = "Correlations betwen scan age and race",
)

ggscatterstats(
  data = data,
  x = X.Gestational_Age_scan,
  y = X.Weighted_Avg_XYZ_mean,
  xlab = "Age of scan (weeks)",
  ylab = "X.Weighted_Avg_XYZ_mean",
  title = "Correlations betwen scan age and XYZ_mean",
)

gghistostats(
  data = data,
  x = X.Gestational_Age_scan,
  title = "Distribution for age of scan",
  xlab = "Age of scan (weeks)",
  binwidth = 1
)

tmp <- data[c("X.Weighted_Avg_PYR_drift","X.Weighted_Avg_PYR_mean",
              "X.Weighted_Avg_XYZ_drift","X.Weighted_Avg_XYZ_mean")]
ggcorrmat(
  data = tmp,
  cor.vars.names = c("PYR_drift","PYR_mean","XYZ_drift","XYZ_mean"),
  output = "plot"
)


ggcorrmat(
  data = data,
  cor.vars = c("X.Weighted_Avg_PYR_drift","X.Weighted_Avg_PYR_mean",
                     "X.Weighted_Avg_XYZ_drift","X.Weighted_Avg_XYZ_mean",
                     "X.Gestational_Age_scan","X.Gestational_Age_birth",
                     "X.Birth_weight","sex"),
  cor.vars.names = c("PYR_drift","PYR_mean",
               "XYZ_drift","XYZ_mean",
               "Age_scan","Age_birth",
               "Birth_weight","sex"),
  output = "plot"
)

tmp2 <- data[c("Subject.ID","X.Weighted_Avg_PYR_drift","X.Weighted_Avg_PYR_mean")]
tmp2 <- melt(tmp2, id.vars = 'Subject.ID', variable.names = 'series')
ggplot(tmp2, aes(x=value, fill=variable)) +
  geom_histogram(alpha=.5, position="identity")

tmp2 <- data[c("Subject.ID","X.Weighted_Avg_XYZ_drift","X.Weighted_Avg_XYZ_mean")]
tmp2 <- melt(tmp2, id.vars = 'Subject.ID', variable.names = 'series')
ggplot(tmp2, aes(x=value, fill=variable)) +
  geom_histogram(alpha=.5, position="identity")

  
