#Start by loading in the required packages
library("survival")
library("survminer")

#Okay, now let's grab our dataset

getwd()
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 2/Deliverables/Report/data/raw/")

#Let's start by comparing Trial1 and A
compa<- read.csv("cs5314_1vA.csv")
compa
is.data.frame(compa)

#Set up the model, plot and analyset
amod <- survfit(Surv(time, status) ~ treatment, data=compa)
amod

ggsurvplot(amod, data=compa, xlab="Days")
#Fix up key
treatments <- c("StockA,10^5","StockA,10^6","Trial1,10^5","Trial1,10^6")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(amod, data = compa, xlab = "Days",legend.lab = treatments, legend.title="Treatment Groups")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=compa)

adiff <- pairwise_survdiff(Surv(time, status) ~ treatment, data=compa)
adiff

symnum(adiff$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

#Now let's repeat for Trial1 vs StockB

compb<- read.csv("cs5314_1vB.csv")
compb
is.data.frame(compb)

#Set up the model, plot and analyset
bmod <- survfit(Surv(time, status) ~ treatment, data=compb)
bmod

ggsurvplot(bmod, data=compb, xlab="Days")
#Fix up key
treatments <- c("StockB,10^5","StockB,10^6","Trial1,10^5","Trial1,10^6")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(bmod, data = compb, xlab = "Days",legend.lab = treatments, legend.title="Treatment Groups")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=compb)

bdiff <- pairwise_survdiff(Surv(time, status) ~ treatment, data=compb)
bdiff

symnum(bdiff$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")
