#Start by loading in the required packages
library("survival")
library("survminer")

#Okay, now let's grab our dataset

getwd()
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 2/Deliverables/Report/data/raw/")

dose<- read.csv("cs5314_dose assay.csv")
dose
is.data.frame(dose)

#Set up the model, plot and analyset
domod <- survfit(Surv(time, status) ~ treatment, data=dose)
domod

ggsurvplot(domod, data=dose, xlab="Days")
#Fix up key
dosage <- c("10^5","10^6")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(domod, data = dose, xlab = "Days", legend.labs = dosage, legend.title="Inoculation Dosage (cells/larva)")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=dose)

