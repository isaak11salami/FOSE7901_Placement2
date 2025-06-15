#Start by loading in the required packages
library("survival")
library("survminer")

#Okay, now let's grab our dataset

getwd()
setwd("~/Uni/2025/Session 1/Placements_FOSE7901/Placement 2/Deliverables/Report/data/raw/")

stock<- read.csv("cs5314_stock assay.csv")
stock
is.data.frame(stock)

#Set up the model, plot and analyset
stmod <- survfit(Surv(time, status) ~ treatment, data=stock)
stmod

ggsurvplot(stmod, data=stock, xlab="Days")
#Fix up key
treatments <- c("PBS", "StockA,10^5","StockA,10^6","StockB,10^5","StockB,10^6")

#Add in markings for each day (aided by Qwen AI)
p <- ggsurvplot(stmod, data = stock, xlab = "Days", legend.lab = treatments, legend.title="Inoculation Groups")
p$plot <- p$plot +
  scale_x_continuous(breaks = 0:9,labels = 0:9)
print(p)

#Descriptive Statistics
survdiff(Surv(time, status) ~ treatment, data=stock)

#Okay, so we know that there's some global differences between treatments.
#Now let's find which specific groups differ from one another

stdiff <- pairwise_survdiff(Surv(time, status) ~ treatment, data=stock)
stdiff

symnum(stdiff$p.value, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),
       symbols = c("****", "***", "**", "*", "+", " "),
       abbr.colnames = FALSE, na = "")

