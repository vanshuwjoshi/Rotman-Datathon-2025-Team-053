install.packages("readxl")
library(readxl)
rot1 <- read_excel("C:\\Users\\Akom's Lenovo\\Desktop\\Rotmancleaned_dataset.xlsx")
install.packages('car')
library(car)
#####################################
#subset data
library(dplyr)

rot2<-as.data.frame(rot1)

subrot2 <- dplyr::select(rot1,
    `Energy imports, net (% of energy use)`,
    `Inflation, consumer prices (annual %)...33`,
    `Consumer price index (2010 = 100)...6`,
    `Oil rents (% of GDP)...7`,
    `Adjusted net national income per capita (current US$)`,
    `GDP per capita (current US$)...22`,
    `Human capital index (HCI) (scale 0-1)`,
    `Unemployment, total (% of total labor force) (national estimate)`)
######################################
#test predictors for normality
qqPlot(rot1$`Energy imports, net (% of energy use)`)
qqPlot(rot1$`Inflation, consumer prices (annual %)...33`)
qqPlot(rot1$`Consumer price index (2010 = 100)...6`)

qqPlot(rot1$`Oil rents (% of GDP)...7`)
#NOT NORMAL
qqPlot(rot1$`Adjusted net national income per capita (current US$)`)
#NOTNORMAL
qqPlot(rot1$`GDP per capita (current US$)...22`)
#NOTNORMAL
qqPlot(rot1$`Human capital index (HCI) (scale 0-1)`)
#NOTNORMAL
qqPlot(rot1$`Unemployment, total (% of total labor force) (national estimate)`)
#NOTNORMAL
#####################################
#assess linearity
plot(rot1$`Energy imports, net (% of energy use)`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Inflation, consumer prices (annual %)...33`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Consumer price index (2010 = 100)...6`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Oil rents (% of GDP)...7`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Adjusted net national income per capita (current US$)`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`GDP per capita (current US$)...22`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Human capital index (HCI) (scale 0-1)`,rot1$`Cost to export, border compliance (US$)...30`)
plot(rot1$`Unemployment, total (% of total labor force) (national estimate)`,rot1$`Cost to export, border compliance (US$)...30`)
####################################
#run regression model and revisions
reg1<-lm(rot1$`Cost to export, border compliance (US$)...30`~
           rot1$`Energy imports, net (% of energy use)`+
           rot1$`Inflation, consumer prices (annual %)...33`+
           rot1$`Consumer price index (2010 = 100)...6`+
           rot1$`Oil rents (% of GDP)...7`+
           rot1$`Adjusted net national income per capita (current US$)`+
           rot1$`GDP per capita (current US$)...22`+
           rot1$`Human capital index (HCI) (scale 0-1)`+
           rot1$`Unemployment, total (% of total labor force) (national estimate)`)
summary(reg1)

#correlation matrix for predictors
cor(subrot2)

#new regression, removing highly correlated variables
reg2<-lm(rot1$`Cost to export, border compliance (US$)...30`~
           rot1$`Inflation, consumer prices (annual %)...33`+
           rot1$`Consumer price index (2010 = 100)...6`+
           rot1$`Oil rents (% of GDP)...7`+
           rot1$`Human capital index (HCI) (scale 0-1)`+
           rot1$`Unemployment, total (% of total labor force) (national estimate)`)
summary(reg2)

#new regression without unemployment bc statistically insignificant
reg3<-lm(rot1$`Cost to export, border compliance (US$)...30`~
           rot1$`Inflation, consumer prices (annual %)...33`+
           rot1$`Consumer price index (2010 = 100)...6`+
           rot1$`Oil rents (% of GDP)...7`+
           rot1$`Human capital index (HCI) (scale 0-1)`)
summary(reg3)
