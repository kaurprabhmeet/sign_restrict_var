

###########################################
# Purpose:          FM413 Breakeven 5

# Author:           Prabhmeet Kaur

# Contact:          p.matta@lse.ac.uk

# Date Created:     06/03/2023

# Date Modified:    24/04/2023

###########################################

rm(list = ls())

# Get the username
username <- Sys.getenv("USER")

# Get the user's operating system
os <- Sys.info()["sysname"]

# Set the working directory based on username (No Linux)
if (os == "Windows") {
  # Set the working directory for Windows users
  working_dir <- paste("C:/Users/", username, "/MSc Econ/Dissertation/DataAnalysis", sep="")
}  else if (os == "Darwin") {
  # Set the working directory for macOS users
  working_dir <- paste("/Users/", username, "/Desktop/MSc Econ/Dissertation/DataAnalysis", sep="")
} else {
  # Set a default working directory if the operating system is not recognized
  setwd("~/my_project")
}

# Set the working directory
setwd(working_dir)

# Print the working directory
print(getwd())

###### Install required packages

# install.packages("minqa")  
# install.packages("~/Desktop/HI_0.5.tar.gz", repos = NULL, type = 'source')
# install.packages("mvnfast")
# install.packages("lubridate")  
# install.packages("~/Desktop/HI_0.5.tar.gz", repos = NULL, type = 'source')

packages <- c("VARsignR", "haven", "tseries", "urca", "xtable", "vars", "mFilter",
              "forecast", "tidyverse", "svars", "ggplot2", "ggfortify", "Matrix", "stargazer", "modelsummary",
              "labelVector", "textreg","haven")

lapply(packages, require, character.only = TRUE)

###### Load dataset and label
final_dataset <- read_dta("/Users/Prabhmeet/Desktop/MSc Econ/Dissertation/DataAnalysis/processed/final_dataset.dta")


final_dataset <- 
  set_label(final_dataset,
            breakeven5_monthly = "5 Year Breakeven Inflation",
            unemp = "Unemployment",
            wuxia_shadow = "Wu Xia Shadow Rate",
            inflation = "Inflation")

# Making individual vectors to create a df with only needed vars
unemp <- ts(final_dataset$unemp)  # unemp
breakeven5_monthly <- ts(final_dataset$breakeven5_monthly)  #  inflation exp
inflation <- ts(final_dataset$inflation)
wuxia <- ts(final_dataset$fedfunds_rate)
energy <- ts(final_dataset$global_energy_price)
rawmat <- ts(final_dataset$global_raw_mat)
usd_eur <- ts(final_dataset$usd_eur)
usd_yen <- ts(final_dataset$usd_yen)
break5 <- cbind(breakeven5_monthly, unemp, inflation, wuxia, usd_eur, usd_yen, rawmat, energy)
break5 <- na.omit(break5)
View(break5) 
plot.ts(break5)
vl <- c("5 Year Breakeven Inflation","Unemployment", "Inflation", "Wu Xia Shadow Rate")

break5 <- ts(break5, frequency = 12, start = c(2003, 1))  
lagselect <- VARselect(break5, lag.max = 15, type = "const")
lagselect$selection

stargazer((as.data.frame(break5)), type = "latex", digits=1,flip = TRUE, out = "~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/summary_break5.tex", title="Summary Table")
break5_coja <- final_dataset[, c("breakeven5_monthly", "unemp", "inflation", "wuxia_shadow")]


###########################################
# Johansen Cointegration and Dickey Fuller Tests
###########################################

jotest <- ca.jo(break5_coja, type="trace", K=2, ecdet="none", spec="longrun")
S <- summary(jotest)
str(S)
jotest_break5 <- cbind(S@teststat, S@cval)
stargazer(jotest_break5, summary=FALSE, rownames=FALSE, out = "~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/coja_break5.tex")

df_vec <-   final_dataset$breakeven5_monthly + (.1595*final_dataset$unemp) + (-2950.88*final_dataset$inflation) + (.10027*final_dataset$wuxia_shadow)
df_vec <- na.omit(df_vec)
adf_break5 <- adf.test(df_vec)
plot(df_vec, type="l")
adf_break5 <- tidy(adf_break5)


autoplot(break5, facet = TRUE, loadings.label=TRUE) + theme_bw()
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/break5_series.png")


###########################################
# Sign Restrictions SVAR
###########################################
###### set sign restrictions
# Assumption: Response of inflation expectations to a monetary policy shock is the object of interest -> left unrestricted.
# Assumption: increase in fed funds rate is to decrease inflation (3rd var): thus, -ve : -3
# Assumption: increase in fed funds rate -> increase in unemp since fed funds rate rel : +2
# Assumption: increase in fed funds rate -> Positive rela with itseld : +4

constr <- c(+4,+2,-3)    

# Mon policy on inflation < 0, unemp >0, fed funds rate > 0
# estimates the model
set.seed(93753)
vl <- c("5 Year Breakeven Inflation","Unemployment", "Inflation", "Fed Funds Rate", "USD-EUR", "USD-JPY", "Raw Material", "Energy")

###### Uhlig’s (2005) Rejection Method

model <- uhlig.reject(Y=break5, nlags=2, draws=1000, subdraws=100, nkeep=1000, KMIN=1, KMAX=6, constrained=constr, constant=TRUE, steps=24)
summary(model)
irfs <- model$IRFS  

irfplot(irfdraws=irfs, type="median",  labels=vl, bands=c(0.16, 0.84), grid=TRUE, bw=FALSE)
ggsave(filename="~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/irf_sign_break5_series.png")

fevd <- model$FEVDS
fevdplot(fevd, label=vl, save=FALSE, bands=c(0.16, 0.84), grid=TRUE,
         bw=FALSE, table=FALSE, periods=NULL)
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/fevd_sign_break5_series.png")
fevd.table <- fevdplot(fevd, table=TRUE, label=vl)
print(fevd.table)
stargazer(fevd.table, summary=FALSE, rownames=FALSE, out = "~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/fevd_sign_break5.tex", title="FEVD Table (5 Year Breakeven Inflation)")

shocks <- model$SHOCKS
ss <- ts(t(apply(shocks,2,quantile,probs=c(0.5, 0.16, 0.84))), frequency=12, start=c(2003,1))
plot(ss[,1], ylab = "Shadow Rate shock",  type="l", col="blue", ylim=c(min(ss), max(ss)))
abline(h=0, col="black")
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/shock_break5_series.png")
###### Rubio-Ramirez et al’s (2010) Rejection Method
model2 <- rwz.reject(Y=break5, nlags=2, draws=1000, subdraws=100, nkeep=1000,
                     KMIN=1, KMAX=6, constrained=constr, constant=FALSE, steps=24)
irfs2 <- model2$IRFS
# *title= "Rubio-Ramirez et al’s (2010) Rejection Method (Breakeven 5)"
irfplot(irfdraws=irfs2, type="median", labels=vl, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/irf_rr_break5_series.png")
model3 <- uhlig.penalty(Y=break5, nlags=2, draws=1000, subdraws=100,
                        nkeep=1000, KMIN=1, KMAX=6, constrained=constr,
                        constant=FALSE, steps=24, penalty=100, crit=0.001)
irfs3 <- model3$IRFS
irfplot(irfdraws=irfs3, type="median", labels=vl, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/irf_penalty_break5_series.png")

 
#Fry and Pagan (2011) strong differences between the MT impulse responses and the median responses thus biased results
fp.target(Y=break5, irfdraws=irfs,  nlags=2,  constant=F, labels=vl, target=TRUE,
          type="median", bands=c(0.16, 0.84), save=FALSE,  grid=TRUE, bw=FALSE,
          legend=TRUE, maxit=1000)
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/irf_fp_break5_series.png")




###########################################
# SVAR
###########################################
reduced.form <- vars::VAR(break5, lag.max = 2, ic = "AIC" )
structural.form <- id.ngml(reduced.form)
summary(structural.form)
structural.form$B <- structural.form$B[,c(3,2,1)]
structural.form$B[,3] <- structural.form$B[,3]*(-1)
impulse.response <- irf(structural.form, horizon = 30)
plot(impulse.response, scales = 'free_y')
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/irf_svar_break5_series.png")
cores <- parallel::detectCores() - 1
#boot.svar <- wild.boot(structural.form, n.ahead = 30, nboot = 500, nc = cores)
#plot(boot.svar)
fevd <- fevd(structural.form, n.ahead = 30)
plot(fevd)
ggsave("~/Desktop/Dissertation/DataAnalysis/outputfiles/Break5/fevd_svar_break5_series.png")
#hist.decomp <- hd(structural.form, series = 1)
#plot(hist.decomp)
#counterfactuals <- cf(structural.form, series = 1)
#plot(counterfactuals)

model0 <- rfbvar(Y=break5, nlags=2, draws=1000, constant=FALSE,
                 steps=24, shock=4)
# get posterior draws
fevd0 <- model0$FEVDS
# plot impulse response functions
fevdplot(fevddraws=fevd0, type="median", labels=vl, save=FALSE, bands=c(0.16, 0.84),
         grid=TRUE, bw=FALSE)
irfs0 <- model0$IRFS
# plot impulse response functions
irfplot(irfdraws=irfs0, type="mean", labels=vl, save=FALSE, bands=c(0.16, 0.84),
        grid=TRUE, bw=FALSE)
