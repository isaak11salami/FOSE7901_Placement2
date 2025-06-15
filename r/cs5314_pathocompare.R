#Start by loading in the required packages
library("survival")
library("survminer")

#Okay, now let's grab our dataset

getwd()
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 2/Deliverables/Report/data/raw/")

patho<- read.csv("cs5314_pathocompare.csv")
patho
is.data.frame(patho)

#Set up the model, plot and analyset
pcmod <- survfit(Surv(time, status) ~ treatment, data=patho)
pcmod

ggsurvplot(pcmod, data=patho, xlab="Days")
#Fix up key
treatments <- c("StockA,10^5","StockA,10^6","StockB,10^5","StockB,10^6","Trial1,10^5","Trial1,10^6")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(pcmod, data = patho, xlab = "Days",legend.lab = treatments, legend.title="Inoculation Groups")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=patho)

pdiff <- pairwise_survdiff(Surv(time, status) ~ treatment, data=patho)
pdiff

symnum(pdiff$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

