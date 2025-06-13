#Let's analyse the C.albicans CS5314 infection assay data using 
#a Log-Rank test and see if we can get the same result as Andrea's
#analysis using Prism

#Start by laoding in the required packages
install.packages("survival")
install.packages("survminer")
library("survival")
library("survminer")

#Okay, now let's grab our dataset

getwd()
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 2/Report/data/raw/")

treat<- read.csv("cs5314_treatment assay.csv")
treat
is.data.frame(treat)

#Set up the model, plot and analyset
trmod <- survfit(Surv(time, status) ~ treatment, data=treat)
trmod

ggsurvplot(ifmod, data=treat, xlab="Days")
#Fix up key
peptides <- c("AmB","AmB+LF","AmB+LGF","AmB+LNL","AmB+LCL","LFG","LNL","LCL","LF","Tris Buffer")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(ifmod, data = treat, xlab = "Days", legend.labs = peptides, legend.title="Treatment Groups")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=treat)

