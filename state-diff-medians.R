#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# State-Level Synthetic Case Control File                                       #
# Last update: 12/31/14                                                         # 
#################################################################################
### Packages and Data ###
library(foreign)
library(Synth)

stmed <- read.dta("state-diff-medians.dta")
clp.states <- c("AZ", "CA", "CO", "CT", "DE", "FL", "KS", "KY", "ME", "MD", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "OK", "OR", "PA", "RI", "SD", "UT", "WV", "WY")
west <- c("WA", "OR", "CA", "ID", "NV", "AZ", "UT", "WY", "MT", "CO", "NM")
mwest <- c("ND", "SD", "NE", "KS", "OK", "MO", "IA", "MN", "WI", "IL", "IN", "MI", "OH")
south <- c("TX", "LA", "AR", "TN", "KY", "AL", "GA", "FL", "SC", "NC", "VA", "WV")

stmed$clp <- ifelse(stmed$state %in% clp.states, 1, 0) 
stmed$west <- ifelse(stmed$state %in% west, 1, 0) 
stmed$mwest <- ifelse(stmed$state %in% mwest, 1, 0) 
stmed$south <- ifelse(stmed$state %in% south, 1, 0) 
stmed <- stmed[ , c("state", "year", "alpha", "pm", "avgdist", "ideology", "ideoex", "dividedgov", "gopshare", "deficit", "squire_score", "income", "unemployment", "population", "populationdensity", "grants", "gsp", "termlimits_house", "termlimits_senate", "demcontrol", "clp", "govideo2", "west", "mwest", "south")]

## Impute missing difference in party median and average distance values ##
pm.mi <- lm(pm ~ ideology + ideoex + dividedgov + gopshare + deficit + squire_score + income + unemployment + population + populationdensity + grants + gsp + termlimits_house + termlimits_senate + demcontrol + clp + govideo2 + west + mwest + south, data = stmed)
ad.mi <- lm(avgdist ~ ideology + ideoex + dividedgov + gopshare + deficit + squire_score + income + unemployment + population + populationdensity + grants + gsp + termlimits_house + termlimits_senate + demcontrol + clp + govideo2 + west + mwest + south, data = stmed)

dm <- stmed[ , -c(1:5)]
stmed$pm2 <- as.matrix(cbind(1, dm)) %*% coef(pm.mi)
stmed$pm3 <- ifelse(is.na(stmed$pm) == TRUE, stmed$pm2, stmed$pm)

stmed$ad2 <- as.matrix(cbind(1, dm)) %*% coef(ad.mi)
stmed$ad3 <- ifelse(is.na(stmed$avgdist) == TRUE, stmed$ad2, stmed$avgdist)

### Synthetic Case Control ###
## Arizona ##
# Controls #
controls <- unique(stmed$alpha)
az.c <- controls[-which(controls == 3 | controls == 19 | controls == 23)]

# Data preparation #
az.dp <- dataprep(stmed, 
  predictors = c("ad3", "ideology", "dividedgov", "gopshare", "deficit", "squire_score", "income", "unemployment", "population", "populationdensity", "grants", "gsp", "termlimits_house", "termlimits_senate", "demcontrol", "clp", "govideo2", "west", "mwest", "south"),
  predictors.op = "mean",
  time.predictors.prior = 1993:2000,
  dependent = "pm3",
  unit.variable = "alpha",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 3,
  controls.identifier = az.c,  
  time.optimize.ssr = 1993:2000,
  time.plot = 1993:2007)

# Generate synthetic AZ #
az.synth <- synth(az.dp, method = "BFGS")
az.diag <- synth.tab(dataprep.res = az.dp, synth.res = az.synth)

# Plot party medians for AZ and S-AZ #
az.pred <- az.dp$Y0plot %*% az.synth$solution.w 

pdf("az.pdf")

par(mar = c(5, 5, .5, .5))
plot(1993:2007, az.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid(); abline(v = seq(1993, 2007, 2), col = "lightgray", lty = 3)
abline(h = 0, lwd = 1)
abline(v = 2001, lwd = 1)
lines(1993:2007, az.dp$Y1plot, lwd = 2)
lines(1993:2007, az.pred, lwd = 2, lty = 2)
axis(1, at = 1993:2007, cex.axis = 1.25, las = 2)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Party Medians", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Arizona", "Synthetic Arizona"), lwd = 2, col = "black", lty = c(1, 2), cex = 1.25)
text(1997, .7, "First year after", cex = 1.25)
text(1997, .55, "public financing", cex = 1.25)
arrows(1998.75, .7, 2000.9, .7, length = .1)
rug(stmed$pm3, side = 2, ticksize = .025)
text(1995.5, 2, bquote(paste("MSPE" ==  .(round(az.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Maine ##
# Controls #
me.c <- controls[-which(controls == 3 | controls == 19 | controls == 23)]

# Data preparation #
me.dp <- dataprep(stmed, 
  predictors = c("ad3", "ideology", "dividedgov", "gopshare", "deficit", "squire_score", "income", "unemployment", "population", "populationdensity", "grants", "gsp", "termlimits_house", "termlimits_senate", "demcontrol", "clp", "govideo2", "west", "mwest", "south"),
  predictors.op = "mean",
  time.predictors.prior = 1993:2000,
  dependent = "pm3",
  unit.variable = "alpha",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 19,
  controls.identifier = me.c,  
  time.optimize.ssr = 1993:2000,
  time.plot = 1993:2007)

# Generate synthetic ME #
me.synth <- synth(me.dp, method = "BFGS")
me.diag <- synth.tab(dataprep.res = me.dp, synth.res = me.synth)

# Plot party medians for ME and S-ME #
me.pred <- me.dp$Y0plot %*% me.synth$solution.w 

pdf("me.pdf")

par(mar = c(5, 5, .5, .5))
plot(1993:2007, me.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid(); abline(v = seq(1993, 2007, 2), col = "lightgray", lty = 3)
abline(v = 2001, lwd = 1)
lines(1993:2007, me.dp$Y1plot, lwd = 2)
lines(1993:2007, me.pred, lwd = 2, lty = 2)
axis(1, at = 1993:2007, cex.axis = 1.25, las = 2)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Party Medians", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Maine", "Synthetic Maine"), lwd = 2, col = "black", lty = c(1, 2), cex = 1.25)
text(1997, .7, "First year after", cex = 1.25)
text(1997, .55, "public financing", cex = 1.25)
arrows(1998.75, .7, 2000.9, .7, length = .1)
rug(stmed$pm3, side = 2, ticksize = .025)
text(1995.5, 2, bquote(paste("MSPE" ==  .(round(me.synth$loss.v, 5)))), cex = 1.25)

dev.off()

# Plot gap between AZ and S-AZ, ME and S-ME, and placebo states #
gaps <- matrix(nrow = length(me.c), ncol = 15)
mspe.ratios <- numeric(length(controls))
bad.fit <- numeric(44)
for(i in 1:44){
me.p.dp <- dataprep(stmed, 
  predictors = c("ad3", "ideology", "dividedgov", "gopshare", "deficit", "squire_score", "income", "unemployment", "population", "populationdensity", "grants", "gsp", "termlimits_house", "termlimits_senate", "demcontrol", "clp", "govideo2", "west", "mwest", "south"),
  predictors.op = "mean",
  time.predictors.prior = 1993:2000,
  dependent = "pm3",
  unit.variable = "alpha",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = me.c[i],
  controls.identifier = me.c[-i],  
  time.optimize.ssr = 1993:2000,
  time.plot = 1993:2007)

me.p.synth <- synth(me.p.dp, method = "BFGS")
placebo.pred <- me.p.dp$Y0plot %*% me.p.synth$solution.w
gaps[i, ] <- me.p.dp$Y1plot - (me.p.dp$Y0plot %*% me.p.synth$solution.w) 
mspe.ratios[i] <- mean((me.p.dp$Y1plot[3:5] - placebo.pred[3:5])^2)/mean((me.p.dp$Y1plot[1:2] - placebo.pred[1:2])^2)
bad.fit[i] <- as.numeric(ifelse(me.p.synth$loss.v > me.synth$loss.v*10, 1, 0))
print(i)
}

# Treated state gaps #
me.gaps <- me.dp$Y1plot - (me.dp$Y0plot %*% me.synth$solution.w) 
az.gaps <- az.dp$Y1plot - (az.dp$Y0plot %*% az.synth$solution.w) 

# Treated state MSPE ratios #
me.ratio <- mean((me.dp$Y1plot[4:5] - me.pred[4:5])^2)/mean((me.dp$Y1plot[1:3] - me.pred[1:3])^2)
az.ratio <- mean((az.dp$Y1plot[3:5] - az.pred[3:5])^2)/mean((az.dp$Y1plot[1:2] - az.pred[1:2])^2)

# Treated state permutation p-values #
me.p <- length(which(mspe.ratios >= me.ratio))/(length(controls) + 2)
az.p <- length(which(mspe.ratios >= az.ratio))/(length(controls) + 2)

pdf("az-me-gaps.pdf")

par(mar = c(5, 5, .5, .5))
plot(1993:2007, me.gaps, type = "n", ylim = c(-1, 1), lwd = 2, xlab = "", ylab = "", axes = FALSE) 
box(); grid(); abline(v = seq(1993, 2007, 2), col = "lightgray", lty = 3)
abline(h = c(-.75, -.25, .25, .75), lty = 3, col = "lightgray")
for(i in 1:44){
if(bad.fit[i] == 0){
lines(1993:2007, gaps[i, ], type = "l", col = "gray80", lwd = 2)
} else cat("bad fit", "\n")
} 
abline(h = 0, lwd = 1)
abline(v = 2001, lwd = 1)
lines(1993:2007, az.gaps, lwd = 2)
lines(1993:2007, me.gaps, lwd = 2, lty = 2)
axis(1, at = 1993:2007, cex.axis = 1.25, las = 2)
axis(2, at = seq(-1, 1, .25), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Polarization from Synthetic State", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Arizona", "Maine", "Placebo States"), lwd = 2, lty = c(1, 2, 1), col = c("black", "black", "gray80"), cex = 1.25)
text(1996.85, -.5, "First year after", cex = 1.25)
text(1996.85, -.575, "public financing", cex = 1.25)
arrows(1998.75, -.5, 2000.9, -.5, length = .1)

dev.off()

save.image("state-diff-medians.RData")