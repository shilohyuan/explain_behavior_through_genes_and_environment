library(lme4)
library(lmerTest)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(umx)
setwd('/Users/zhou0767/Box/Shiloh_Geography_Spring2019/growthModel')
data <- readRDS('regression_withBFI_withtestage.rds')
data$SVID <- as.character(data$SVID)
colnames(data)[11] <- "dailyKM"
### SIV note - not sure who these people are, never give a variable
### the same name as a function
some.IDs <- c("16568893388709171686",
              "10610545108730384870",
              "154009451010724325",
              "16240839238530568677",
              "6410009645449417190",
              "8578982621091992037",
              "14960215945076347365")

### for some reason there are three columns labeled ID. Remove the
### first one as it's not an SVID or UMich ID, then remove the second
### one because it's redundant with the third.
data <- data[,-c(1,4)]
### Remove the individuals specified above "some.IDs"
data <- data[-which(as.character(data$ID) %in% some.IDs),]

### A little bit of quick renaming
names(data)[2]  <- "R" ## Rename rog to R (radius of gyration)
names(data)[4]  <- "E" ## Rename entropy to E
names(data)[8] <- "P" ## rename nbTrip to P (places visited per day)
names(data)[9] <- "D" ## rename dailyKM to D (distance traveled per day)

### Import David's file with ID maps and zygosity, to facilitate the
### genetic analyses later
IDs <- read.table("/Users/zhou0767/Box//Shiloh_Geography_Spring2019/Brazel_Share/processed/id_mapping.csv", sep=",", header=T, stringsAsFactor=F)
IDs$FAMID <- 1001:(nrow(IDs)+1000)
IDs.l <- data.frame(SVID = c(IDs$T1, IDs$T2),
                    UMICH=c(IDs$T1_alternate_id, IDs$T2_alternate_id),
                    bestzygos=c(IDs$bestzygos, IDs$bestzygos),
                    FAMID=c(IDs$FAMID, IDs$FAMID),
                    IDSC = c(rep(1, nrow(IDs)), rep(2,nrow(IDs))))
### Merge in David's zygosity codes
data <- left_join(data, IDs.l, by="SVID")


### -------------###
### Mixed models ###
### -------------###

P_lm1 <- lmer(P ~ age_scale + sex + summer + appType  + wdays  + (age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="bobyqa"))
R_lm1 <- lmer(R ~ age_scale + sex + summer + appType  + wdays  + (age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="bobyqa"))
E_lm1 <- lmer(E ~ age_scale + sex + summer + appType  + wdays  + (age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="bobyqa"))
D_lm1 <- lmer(D ~ age_scale + sex + summer + appType  + wdays  + (age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="bobyqa"))

### ------------------------- ###
### Model-estimated variances ###
### ------------------------- ###
age <- seq(min(data$age_scale), max(data$age_scale), .01)
get_variance <- function(mod) {
   #mod <- P_lm1
    x <- summary(mod)
    FID  <- x$varcor$FID
    SVID <- x$varcor$SVID
    age <- seq(min(data$age_scale), max(data$age_scale), .01)
    lambda <- matrix(cbind(rep(1, length(age)), age), nrow=length(age), ncol=2)
    variance <- diag(lambda%*%FID%*%t(lambda)) + diag(lambda%*%SVID%*%t(lambda)) 
    # what is the purpose of this vairance?
    return(variance)
}

par(mfrow=c(2,2))
plot(age, get_variance(P_lm1), type="l", main="No. of Places", ylab="Model-estimated Variance")
plot(age, get_variance(R_lm1), type="l", main="Activity Space", ylab="Model-estimated Variance")
plot(age, get_variance(E_lm1), type="l", main="Entropy", ylab="Model-estimated Variance")
plot(age, get_variance(D_lm1), type="l", main="Distance", ylab="Model-estimated Variance")


### -------------------------------- ###
### Biometric Variance Decomposition ###
### -------------------------------- ###
###
### First step is to extract conditional modes from the two random
### effects, then sum them. This results in an intercept value and
### slope value for each person.
extract_ranefs <- function(mod) {
    ranef.family <- ranef(mod)$FID %>%
                             rownames_to_column("FID") %>%
                             rename(inter.family=`(Intercept)`, slope.family=age_scale)
    ## Now extract ind-level random effect, create two new columns for
    ## SVID and FID, and rename the random intercept and random slope
    ## to ease code readibility after the left join below.
    ranef.indivi <- ranef(mod)$`SVID:FID` %>%
                             rownames_to_column("both") %>%
                             separate(both, c("SVID", "FID"), ":") %>%
                             rename(inter.indivi=`(Intercept)`, slope.indivi=age_scale)
    ## Join these together and create the 
    conditional.modes <- left_join(ranef.indivi, ranef.family, by = "FID") %>%
        transmute(SVID   = as.character(SVID),
                  intercept = `inter.indivi` + `inter.family`,
                  slope     = `slope.indivi` + `slope.family`,
                  )
    return(conditional.modes)
}

### Function to decompose the variance of the intercept and slope
get_heritability <- function(mod, data) {
  #mod <- P_lm1
    m.ranefs <- left_join(extract_ranefs(mod), distinct(subset(data, select=c("SVID", "FAMID", "IDSC", "bestzygos.y"))), by="SVID")
    m.ranefs.w <- reshape(m.ranefs, v.names=c("intercept", "slope"), timevar="IDSC", idvar="FAMID", direction="wide", sep="") 
    # Why having intercept1 and slope1
    m.mzData <- m.ranefs.w[m.ranefs.w$bestzygos == "MZ", ]
    m.dzData <- m.ranefs.w[m.ranefs.w$bestzygos == "DZ" | m.ranefs.w$bestzygos == "OS", ]
    m.i = umxACEv(selDVs = c("intercept", "slope"), sep = "", dzData = m.dzData, mzData = m.mzData, type= "FIML")
    # is this a bivariate model?
    return(m.i)
}

p.ace <- get_heritability(P_lm1, data=data)
r.ace <- get_heritability(R_lm1, data=data)
e.ace <- get_heritability(E_lm1, data=data)
d.ace <- get_heritability(D_lm1, data=data)

### Extract the genetic and environmental var-covar matrices, then
### combine with age to derive model-estimated genetic and
### environmental variances as a function of age.
age <- seq(min(data$age_scale), max(data$age_scale), .01)
get_genetic_variance <- function(umxmod) {
    #umxmod <- p.ace  
    x <- summary(umxmod)
    ### Extract the genetic and environmental var-covar matrices
    v.g <- matrix(c(x$parameters$Estimate[3], x$parameters$Estimate[4],  x$parameters$Estimate[4],  x$parameters$Estimate[5]),  byrow=T, ncol=2, nrow=2)
    # x$parameters$Estimate[3] A_r1c1; A_r2c1; A_r2c1, A_r2c2
    # what is this matrix 
    v.c <- matrix(c(x$parameters$Estimate[6], x$parameters$Estimate[7],  x$parameters$Estimate[7],  x$parameters$Estimate[8]),  byrow=T, ncol=2, nrow=2)
    v.e <- matrix(c(x$parameters$Estimate[9], x$parameters$Estimate[10], x$parameters$Estimate[10], x$parameters$Estimate[11]), byrow=T, ncol=2, nrow=2)
    age <- seq(min(data$age_scale), max(data$age_scale), .01)
    lambda <- matrix(cbind(rep(1, length(age)), age), nrow=length(age), ncol=2)
    est.age.v.g <- diag(lambda%*%v.g%*%t(lambda))
    est.age.v.c <- diag(lambda%*%v.c%*%t(lambda))
    est.age.v.e <- diag(lambda%*%v.e%*%t(lambda))
    return(list(est.age.v.g, est.age.v.c, est.age.v.e))
}

p.v <- get_genetic_variance(umxmod=p.ace)
r.v <- get_genetic_variance(umxmod=r.ace)
e.v <- get_genetic_variance(umxmod=e.ace)
d.v <- get_genetic_variance(umxmod=d.ace)

### plot the genetic, shared environmental, and non-shared environmental variances
par(lwd=2, cex=1.8, mfrow=c(2,2), cex.axis=1.3, cex.lab=1.3)
plot(age, p.v[[1]], col="blue", type="l", lwd=3, xlab="Age", ylab="Variance", main="No. of Places", ylim=c(0,3))
lines(age, p.v[[2]], col="red", lwd=3)
lines(age, p.v[[3]], col="green", lwd=3)
legend("topright", legend=c("var(A)", "var(C)", "var(E)"), col=c("blue", "red", "green"), lwd=2, cex=1.3)
plot(age,  r.v[[1]], col="blue", type="l", lwd=3, xlab="Age", ylab="Variance", main="Activity Space", ylim=c(0,30))
lines(age, r.v[[2]], col="red", lwd=3)
lines(age, r.v[[3]], col="green", lwd=3)
plot(age,  e.v[[1]], col="blue", type="l", lwd=3, xlab="Age", ylab="Variance", main="Entropy", ylim=c(0,.4))
lines(age, e.v[[2]], col="red", lwd=3)
lines(age, e.v[[3]], col="green", lwd=3)
plot(age,  d.v[[1]], col="blue", type="l", lwd=3, xlab="Age", ylab="Variance", main="Distance", ylim=c(0,130))
lines(age, d.v[[2]], col="red", lwd=3)
lines(age, d.v[[3]], col="green", lwd=3)


### plot the heritabilities
par(lwd=2, cex=1.8, cex.axis=1.3, cex.lab=1.3, mfrow=c(1,1))
axis(1, at=seq(0,9, by=1), labels = FALSE)

plot(age,  p.v[[1]]/(p.v[[1]] + p.v[[2]] + p.v[[3]]), col="orange",  type="l", lwd=6, xlab="Age", ylab="Heritability", main="Change in Heritability", ylim=c(0,.8),xaxt="none")
lines(age, r.v[[1]]/(r.v[[1]] + r.v[[2]] + r.v[[3]]), col="firebrick",   type="l", lwd=6, xlab="Age",xaxt="none")
lines(age, e.v[[1]]/(e.v[[1]] + e.v[[2]] + e.v[[3]]), col="darkgreen", type="l", lwd=6, xlab="Age",xaxt="none")
lines(age, d.v[[1]]/(d.v[[1]] + d.v[[2]] + d.v[[3]]), col="black",  type="l", lwd=6, xlab="Age",xaxt="none")

axis(1, at=0:8, labels=c(14:22))
legend("topleft", legend=c( "Entropy", "Activity Space", "No. of Places","Distance"), col=c("darkgreen", "orange", "firebrick", "black"), lwd=6,  cex=.8)

### ggplot line plot ###

### plot the shared environment effect
par(lwd=2, cex=1.8, cex.axis=1.3, cex.lab=1.3, mfrow=c(1,1))
plot(age,  p.v[[2]]/(p.v[[1]] + p.v[[2]] + p.v[[3]]), col="orange",  type="l", lwd=6, xlab="Age", ylab="Shared Environment", main="Change in Shared Environment", ylim=c(0,.8))
lines(age, r.v[[2]]/(r.v[[1]] + r.v[[2]] + r.v[[3]]), col="firebrick",   type="l", lwd=6, xlab="Age")
lines(age, e.v[[2]]/(e.v[[1]] + e.v[[2]] + e.v[[3]]), col="darkgreen", type="l", lwd=6, xlab="Age")
lines(age, d.v[[2]]/(d.v[[1]] + d.v[[2]] + d.v[[3]]), col="black",  type="l", lwd=6, xlab="Age")
legend("bottomleft", legend=c( "Entropy", "Activity Space", "No. of Places","Distance"), col=c("darkgreen", "orange", "firebrick", "black"), lwd=6, cex=1.5)

### plot 
par(lwd=2, cex=1.8, cex.axis=1.3, cex.lab=1.3, mfrow=c(1,1))
plot(age,  p.v[[3]]/(p.v[[1]] + p.v[[2]] + p.v[[3]]), col="orange",  type="l", lwd=6, xlab="Age", ylab="Non-Shared Environment", main="Change in Non-Shared Environment", ylim=c(0,.8))
lines(age, r.v[[3]]/(r.v[[1]] + r.v[[2]] + r.v[[3]]), col="firebrick",   type="l", lwd=6, xlab="Age")
lines(age, e.v[[3]]/(e.v[[1]] + e.v[[2]] + e.v[[3]]), col="darkgreen", type="l", lwd=6, xlab="Age")
lines(age, d.v[[3]]/(d.v[[1]] + d.v[[2]] + d.v[[3]]), col="black",  type="l", lwd=6, xlab="Age")
legend("top", legend=c( "Entropy", "Activity Space", "No. of Places","Distance"), col=c("darkgreen", "orange", "firebrick", "black"), lwd=6, cex=1.5)







### ------------------------------- ###
###            MML Plot             ###
### ------------------------------- ###
###
### This code is adapted from a script Shiloh sent in April 2020.
###
extract.predicted.trajectory <- function(mod, phenotype) {
    ## Extract family-level random effect, create new column with FID,
    ## and rename the random intercept and random slope to ease code
    ## readibility after the left_join below.
    ranef.family <- ranef(mod)$FID %>%
                             rownames_to_column("FID") %>%
                             rename(inter.family=`(Intercept)`, slope.family=age_scale)
    ## Now extract ind-level random effect, create two new columns for
    ## SVID and FID, and rename the random intercept and random slope
    ## to ease code readibility after the left join below.
    ranef.indivi <- ranef(mod)$`SVID:FID` %>%
                             rownames_to_column("both") %>%
                             separate(both, c("SVID", "FID"), ":") %>%
                             rename(inter.indivi=`(Intercept)`, slope.indivi=age_scale)
    ## Join these together and create predicted trajectories
    predicted.trajectory <- left_join(ranef.indivi, ranef.family, by = "FID") %>%
        transmute(SVID   = SVID,
                  predicted.intercept = `inter.indivi` + `inter.family` + fixef(mod)["(Intercept)"],
                  predicted.slope     = `slope.indivi` + `slope.family` + fixef(mod)["age_scale"],
                  )
    ## Rename for easier merging later
    names(predicted.trajectory) <- paste(names(predicted.trajectory), ".", phenotype, sep="")
    names(predicted.trajectory)[1] <- "SVID"
    return(predicted.trajectory)
}

P.trajectories <- extract.predicted.trajectory(P_lm1, phenotype="P")
E.trajectories <- extract.predicted.trajectory(E_lm1, phenotype="E")
R.trajectories <- extract.predicted.trajectory(R_lm1, phenotype="R")
D.trajectories <- extract.predicted.trajectory(D_lm1, phenotype="D")

## For loop to generate youngest and oldest ages ## SIV note to self -
## there must be a slick way to do this with 'mutate' in dplyr.
SVIDs    <- unique(data$SVID)
youngest <- rep(NA,length(SVIDs))
oldest   <- rep(NA,length(SVIDs))
for (i in 1:length(SVIDs)){
    tmp <- subset(data, SVID==SVIDs[i])
    youngest[i] <- min(tmp$age, na.rm=T) - 14.21
    oldest[i]   <- max(tmp$age, na.rm=T) - 14.21
}
 
dat.to.plot <- data.frame(SVID = SVIDs, youngest=youngest, oldest=oldest)
dat.to.plot <- left_join(dat.to.plot, P.trajectories, by="SVID")
dat.to.plot <- left_join(dat.to.plot, E.trajectories, by="SVID")
dat.to.plot <- left_join(dat.to.plot, R.trajectories, by="SVID")
dat.to.plot <- left_join(dat.to.plot, D.trajectories, by="SVID")

## Calculate y1 and y2, values on the outcome at the youngest and
## oldest age, respectively
dat.to.plot <- dat.to.plot %>%
    mutate(y1.P = youngest*predicted.slope.P + predicted.intercept.P,
           y2.P =   oldest*predicted.slope.P + predicted.intercept.P,
           y1.E = youngest*predicted.slope.E + predicted.intercept.E,
           y2.E =   oldest*predicted.slope.E + predicted.intercept.E,
           y1.R = youngest*predicted.slope.R + predicted.intercept.R,
           y2.R =   oldest*predicted.slope.R + predicted.intercept.R,
           y1.D = youngest*predicted.slope.D + predicted.intercept.D,
           y2.D =   oldest*predicted.slope.D + predicted.intercept.D,
           )

### a convenient place to put all the fixed effects
K <- rbind(fixef(P_lm1), fixef(E_lm1), fixef(R_lm1), fixef(D_lm1))

### Finally, to the plotting
plot_mml <- function(dat.to.plot, y1, y2, K.row, age.offset, ylab) {
    ## just some functions to select and rename columns to make the plotting function simpler
    dat.tmp <- dat.to.plot %>% select("youngest", "oldest", as.character(y1), as.character(y2))
    names(dat.tmp)[c(3,4)] <- c("y1", "y2")
    ## Begin the plot
    m <- ggplot(dat.tmp, aes(youngest, y1))
    ## Loop through all individuals
    for (i in 1:nrow(dat.tmp)){ 
        m <- m + geom_segment(aes(x = youngest+age.offset, y = y1, xend=oldest+age.offset, yend=y2), data=dat.tmp[i,], color = "grey")
    }
    ## Add the fixed effect line
    rslt <- m + geom_abline(slope= K[K.row,2], intercept = (K[K.row,1] - age.offset*K[K.row,2])) + xlab("Age (years)") + ylab(ylab)
    return(rslt)
}

### Get each plot, then combine into 2x2 grid
P.plot <- plot_mml(dat.to.plot=dat.to.plot, y1="y1.P", y2="y2.P", K.row=1, age.offset=14.21, ylab="No. Places Visited Each Day")
E.plot <- plot_mml(dat.to.plot=dat.to.plot, y1="y1.E", y2="y2.E", K.row=2, age.offset=14.21, ylab="Entropy")
R.plot <- plot_mml(dat.to.plot=dat.to.plot, y1="y1.R", y2="y2.R", K.row=3, age.offset=14.21, ylab="Activity Space")
D.plot <- plot_mml(dat.to.plot=dat.to.plot, y1="y1.D", y2="y2.D", K.row=4, age.offset=14.21, ylab="Distance Traveled")
grid.arrange(P.plot,R.plot,E.plot,D.plot,nrow=2,ncol=2)




### ------------------------------------------------------------ ###
###  Simple sliding window to estimate mean as function of age   ###
### ------------------------------------------------------------ ###

### Residualize for the fixed effect covariates
data$P.resid <- resid(lm(P ~ sex + summer + wdays + appType, na.action = na.exclude, data=data))
data$D.resid <- resid(lm(D ~ sex + summer + wdays + appType, na.action = na.exclude, data=data))
data$R.resid <- resid(lm(R ~ sex + summer + wdays + appType, na.action = na.exclude, data=data))
data$E.resid <- resid(lm(E ~ sex + summer + wdays + appType, na.action = na.exclude, data=data))

### Set up the sliding window. Window size is 1/2 year and I increment in hundredths of a year. Probably overkill
rslt <- data.frame(mean.age=NA, P.resid=NA, D.resid=NA, R.resid=NA, E.resid=NA, N=NA)
count <- 1
for(i in seq(0,6,.01)) {
    data2 <- subset(data, age > 14+i & age < 14.5+i)
    rslt[count,1] <- mean(data2$age, na.rm=T)
    rslt[count,2] <- mean(data2$P.resid,  na.rm=T)
    rslt[count,3] <- mean(data2$D.resid, na.rm=T)
    rslt[count,4] <- mean(data2$R.resid,     na.rm=T)
    rslt[count,5] <- mean(data2$E.resid, na.rm=T)
    rslt[count,6] <- nrow(data2)
    count <- count + 1
}

pdf("sliding_window_mean.pdf")
par(mfrow=c(2,3))
plot(rslt$mean.age, rslt$P.resid, main="Places, residualized",             ylab="Number of Places", xlab="Age")
plot(rslt$mean.age, rslt$D.resid, main="Distance, residualized",           ylab="Distance",         xlab="Age")
plot(rslt$mean.age, rslt$R.resid, main="Radius of Gyration\nresidualized", ylab="Radius",           xlab="Age")
plot(rslt$mean.age, rslt$E.resid, main="E, residualized",                  ylab="E",                xlab="Age")
plot(rslt$mean.age, rslt$N)
dev.off()



### ---------------------------- ###
###  Plot individual regressions ###
### ---------------------------- ###

individual.regression <- function(phenotype, ylim) {
    IDs <- unique(data$ID)
    count <- 1
    for(i in IDs) {
        ind <- subset(data, ID == i)
        mod <- lm(paste(phenotype, " ~ age"), data=ind)
        intercept <- mod$coefficients[1]
        slope <- mod$coefficients[2]
        if(count == 1) {
            curve(intercept + slope*x, xlim=c(14,20), ylim=ylim, from=min(ind$age), to=max(ind$age))
        } else {
            curve(intercept + slope*x, xlim=c(14,20), add=T, from=min(ind$age), to=max(ind$age))
        }
        count <- count + 1
    }
}

par(mfrow=c(2,2))
individual.regression("P.resid", ylim=c(-3,4))
individual.regression("D.resid", ylim=c(-20,50))
individual.regression("R.resid", ylim=c(-10,40))
individual.regression("E.resid", ylim=c(-1,1))

####






### ---------------------------------- ###
### Generalized additive mixed models
### ---------------------------------- ###

library(gamm4)
P_ml <- gamm4(P.resid ~ s(age_scale, k = 20), random = ~(1 | SVID), data = data)


plotit <- function(model, data, name) {
    model_pred <- predict(model$gam, se.fit = T)
    model_data <-
        tibble(
            age_scale = data$age_scale + 14,
            frac = model_pred$fit,
            lower = model_pred$fit - 1.96 * model_pred$se.fit,
            upper = model_pred$fit + 1.96 * model_pred$se.fit
        )
    model_data$pheno <- name
    model_plot <- ggplot(model_data, aes(age_scale, frac, fill = pheno, color = pheno)) +
    geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
        geom_line() +
        scale_color_brewer("", palette = "Dark2") +
        scale_fill_brewer("", palette = "Dark2") +
        labs(
            x = "Age (years)",
            y = name
        ) +
        theme(legend.direction = "horizontal", legend.position = c(0.45, 0.05)) +
        xlim(14, 22)
    return(model_plot)
}

plotit(model=P_ml, data=data, name="Places Visited")

P_ml <- gamm4(D ~ s(age_scale, k = 8), random = ~(1 | FID/SVID), data = subset(data, appType==0))
P_pred <- predict(P_ml$gam, se.fit = T)
P_plot_data <-
  tibble(
    age_scale = data$age_scale + 14,
    frac = P_pred$fit,
    lower = P_pred$fit - 1.96 * P_pred$se.fit,
    upper = P_pred$fit + 1.96 * P_pred$se.fit
  )
P_plot_data$pheno <- "daily KM"
home_school_plot <- ggplot(P_plot_data, aes(age_scale, frac, fill = pheno, color = pheno)) +
    geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
    geom_line() +
    scale_color_brewer("", palette = "Dark2") +
    scale_fill_brewer("", palette = "Dark2") +
    labs(
        x = "Age (years)",
        y = "Predicted fraction of time"
    ) +
    theme(legend.direction = "horizontal", legend.position = c(0.45, 0.05)) +
    xlim(14, 22)
home_school_plot


P_ml <- gamm4(E ~ s(age_scale, k = 4), random = ~(1 | FID/SVID), data = data)

P_pred <- predict(P_ml$gam, se.fit = T)
P_plot_data <-
  tibble(
    age_scale = data$age_scale + 14,
    frac = P_pred$fit,
    lower = P_pred$fit - 1.96 * P_pred$se.fit,
    upper = P_pred$fit + 1.96 * P_pred$se.fit
  )
P_plot_data$pheno <- "Places Visited"

home_school_plot <- ggplot(P_plot_data, aes(age_scale, frac, fill = pheno, color = pheno)) +
    geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
    geom_line() +
    scale_color_brewer("", palette = "Dark2") +
    scale_fill_brewer("", palette = "Dark2") +
    labs(
        x = "Age (years)",
        y = "Predicted fraction of time"
    ) +
    theme(legend.direction = "horizontal", legend.position = c(0.45, 0.05)) +
    xlim(14, 24)
home_school_plot







####################################### An attempt at quadratic models, but they faile
data2 <- data
data2$age_scale <- data2$age_scale * 100
P_lm1.q <- lmer(P ~ age_scale + I(age_scale^2) + sex + summer + appType  + wdays  + (I(age_scale^2) + age_scale||FID/SVID), data=data2, na.action = na.exclude, REML=F,control=lmerControl(optimizer="bobyqa"))
R_lm1.q <- lmer(R ~ age_scale + I(age_scale^2) + sex + summer + appType  + wdays  + (I(age_scale^2) + age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="Nelder_Mead"))
E_lm1.q <- lmer(E ~ age_scale + I(age_scale^2) + sex + summer + appType  + wdays  + (I(age_scale^2) + age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="Nelder_Mead"))
D_lm1.q <- lmer(D ~ age_scale + I(age_scale^2) + sex + summer + appType  + wdays  + (I(age_scale^2) + age_scale|FID/SVID), data=data, na.action = na.exclude, REML=F,control=lmerControl(optimizer="Nelder_Mead"))
anova(P_lm1, P_lm1.q)
anova(E_lm1, E_lm1.q)
anova(R_lm1, R_lm1.q)
anova(D_lm1, D_lm1.q)
#######################################################################################
