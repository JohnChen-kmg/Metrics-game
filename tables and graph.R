library(haven)
library(tidyverse)
library(crosstable)
library(reporttools)
library(gmodels)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


data<- read_dta("angrist dataset/STAR_public_use.dta")

summary(star)

data <- select(star, finish4, graddeg, GPA_year1, GPA_year2, age,female, 
               credits_earned1, credits_earned2, english, dad1, dad2,
               mom1, mom2, gpa0, sfp, sfp_p, sfpany, sfsp, sfsp_p, signup,
               ssp, ssp_p)
data <- na.omit(data)
str(data)


# Descriptive analysis
# Summary of ambition variable: finsih4, graddeg
var1 <- with(data, data.frame("finish04"= factor(data$finish4, levels = 0:1, labels =  c("Intend not to finish in 4 years", "Intend to finish in 4 years")), 
             "graddeg"= factor(data$graddeg, levels = 0:1, labels =  c("Don't want more than a BA", "Want more than a BA"))))
tableNominal(var1[,c(1,2)], vertical = FALSE)

CrossTable(data$finish4, data$graddeg)
summary(tab1)


stargazer(data)

"Transplantation" = factor(jasa$transplant, levels = 0:1, labels =
                                   + c("no", "yes"))
