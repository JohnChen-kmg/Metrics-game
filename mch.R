set.seed(1234)
library(Matching)
library(ebal)
library(xtable)

star <- read_dta("angrist dataset/STAR_public_use.dta")

summary(star)


mch <- select(star, finish4, graddeg,lastmin, GPA_year1, GPA_year2, age,female, 
               credits_earned1, credits_earned2, english, dad1, dad2,
               mom1, mom2, sfp_p, ssp_p, signup,control,
              chooseUTM, gpa0, hcom, work1)

mch <- na.omit(mch)
mch$gpa <- (mch$GPA_year1+mch$GPA_year2)/2
        
# 5 categories

mch$lasmin2 <- ifelse(mch$lastmin>3,0,1)


# Confonders selection
varc <- c("age", "female", "english", "dad1", "dad2","mom1", "mom2","chooseUTM","gpa0",
          "hcom","work1")



# Match by Control group

# finish 4 vs gpa2
mch$control <- as.numeric(mch$control)
mch.c <- subset(mch, control>0)

mean(mch.c[mch.c$finish4==1,]$GPA_year1)-mean(mch.c[mch.c$finish4==0,]$GPA_year1)
ate.se1 = sqrt(var(mch.c[mch.c$finish4==1,]$GPA_year1))/length(mch.c[mch.c$finish4==1,]$GPA_year1) +
                      var(mch.c[mch.c$finish4==0,]$GPA_year1)/length(mch.c[mch.c$finish4==0,]$GPA_year1)

m7 <- Match(Y=mch.c$GPA_year1,
            Tr=mch.c$finish4,
            X=mch.c[,varc],ties=F, estimand = "ATE")
summary(m7)


bal.formula1 <- formula(lasmin2 ~ age+ female+ english+dad1+dad2,mom1+mom2+chooseUTM+gpa0+
                                hcom+work1)
mb.exact = MatchBalance(bal.formula1, data=mch.c, match.out=m7)

# lastmin vs gpa2
mean(mch.c[mch.c$lasmin2==1,]$GPA_year1)-mean(mch.c[mch.c$lasmin2==0,]$GPA_year1)
ate.se1 = sqrt(var(mch.c[mch.c$lasmin2==1,]$GPA_year1))/length(mch.c[mch.c$lasmin2==1,]$GPA_year1) +
        var(mch.c[mch.c$lasmin2==0,]$GPA_year1)/length(mch.c[mch.c$lasmin2==0,]$GPA_year1)

m8 <- Match(Y=mch.c$GPA_year1,
            Tr=mch.c$lasmin2,
            X=mch.c[,varc],ties=F, estimand = "ATE")
summary(m8)

bal.formula2 <- formula(lastmin2 ~ age+ female+ english+dad1+dad2,mom1+mom2+chooseUTM+gpa0+
                        hcom+work1)
mb.exact = MatchBalance(bal.formula2, data=mch.c, match.out=m8)









## RC (gpa1)
# sfp_p
mch$sfp_p <- as.numeric(mch$sfp_p)
mch.sfp <- subset(mch, sfp_p>0)
m10 <- Match(Y=mch.sfp$GPA_year1,
            Tr=mch.sfp$finish4,
            X=mch.sfp[,varc],ties=F, estimand = "ATE")
summary(m10)
# ssp_p
mch$ssp_p <- as.numeric(mch$ssp_p)
mch.ssp <- subset(mch, ssp_p>0)
m11 <- Match(Y=mch.ssp$GPA_year1,
             Tr=mch.ssp$finish4,
             X=mch.ssp[,varc],ties=F, estimand = "ATE")
summary(m11)
# signup
mch$signup <- as.numeric(mch$signup)
mch.ssp <- subset(mch, signup>0)
m11 <- Match(Y=mch.ssp$GPA_year1,
             Tr=mch.ssp$finish4,
             X=mch.ssp[,varc],ties=F, estimand = "ATE")
summary(m11)



