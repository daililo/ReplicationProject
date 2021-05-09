#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# New Jersey Synthetic Case Control File -- Floor Vote Ideal Points             #
# Last update: 12/31/14                                                         # 
#################################################################################
### Packages and Data ###
library(foreign)
library(Synth)

assemb.floor <- read.dta("assemb-floor.dta")
assemb.floor$female <- ifelse(assemb.floor$Gender == "F", 1, 0)
assemb.floor$gop <- ifelse(assemb.floor$Party == "R", 1, 0)
treated <- c(67, 68, 78, 89, 136)
controls <- unique(assemb.floor$idnum[assemb.floor$PublicFinance == 0])
controls <- controls[-which(controls %in% treated)]
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

### Synthetic Case Control ### 

## Linda R. Greenstein ##
# Data preparation #
lrg.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:5,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 67,
  controls.identifier = controls,  
  time.optimize.ssr = 1:5,
  time.plot = 1:8)

# Generate synthetic Linda R. Greenstein #
lrg.synth <- synth(lrg.dp, method = "BFGS")
lrg.diag <- synth.tab(dataprep.res = lrg.dp, synth.res = lrg.synth)

# Plot LRG and S-LRG #
lrg.pred <- lrg.dp$Y0plot %*% lrg.synth$solution.w 

pdf("lrg-ip-fl.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, lrg.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 6, lwd = 1)
lines(1:8, lrg.dp$Y1plot, lwd = 2)
lines(1:8, lrg.pred, lwd = 2, lty = 2)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Floor Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Linda R. Greenstein", "Synthetic Linda R. Greenstein"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(3.25, 2.25, "First year after public financing", cex = 1.25)
arrows(5.05, 2.25, 5.95, 2.25, length = .1)
rug(assemb.floor$extremity, side = 2, ticksize = .025)
text(2.75, 1, bquote(paste("MSPE" == .(round(lrg.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Louis D. Greenwald ##
# Data preparation #
ldg.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:3,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 68,
  controls.identifier = controls,  
  time.optimize.ssr = 1:3,
  time.plot = 1:8)

# Generate synthetic Louis D. Greenwald #
ldg.synth <- synth(ldg.dp, method = "BFGS")
ldg.diag <- synth.tab(dataprep.res = ldg.dp, synth.res = ldg.synth)

# Plot LDG and S-LDG #
ldg.pred <- ldg.dp$Y0plot %*% ldg.synth$solution.w 

pdf("ldg-ip-fl.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, ldg.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 4, lwd = 1)
lines(1:8, ldg.dp$Y1plot, lwd = 2)
lines(1:8, ldg.pred, lwd = 2, lty = 2)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Floor Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topright", inset = .025, bty = "n", c("Louis D. Greenwald", "Synthetic Louis D.", "Greenwald"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(2.05, .25, "First year after", cex = 1.25)
text(2.055, .1, "public financing", cex = 1.25)
arrows(2.95, .25, 3.95, .25, length = .1)
rug(assemb.floor$extremity, side = 2, ticksize = .025)
text(2.25, 1.2, bquote(paste("MSPE" == .(round(ldg.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Gordon M. Johnson ##
# Data preparation #
gmj.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:5,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 78,
  controls.identifier = controls,  
  time.optimize.ssr = 1:5,
  time.plot = 1:8)

# Generate synthetic Gordon M. Johnson #
gmj.synth <- synth(gmj.dp, method = "BFGS")
gmj.diag <- synth.tab(dataprep.res = gmj.dp, synth.res = gmj.synth)

# Plot GMJ and S-GMJ #
gmj.pred <- gmj.dp$Y0plot %*% gmj.synth$solution.w 

pdf("gmj-ip-fl.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, gmj.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 6, lwd = 1)
lines(1:8, gmj.dp$Y1plot, lwd = 2)
lines(1:8, gmj.pred, lwd = 2, lty = 2)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Floor Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Gordon M. Johnson", "Synthetic Gordon M. Johnson"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(3.25, .25, "First year after public financing", cex = 1.25)
arrows(5.05, .25, 5.95, .25, length = .1)
rug(assemb.floor$extremity, side = 2, ticksize = .025)
text(2.75, 1.75, bquote(paste("MSPE" == .(round(gmj.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Alison L. McHose ##
# Data preparation #
alm.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:5,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 89,
  controls.identifier = controls,  
  time.optimize.ssr = 1:5,
  time.plot = 1:8)

# Generate synthetic Alison L. McHose #
alm.synth <- synth(alm.dp, method = "BFGS")
alm.diag <- synth.tab(dataprep.res = alm.dp, synth.res = alm.synth)

# Plot ALM and S-ALM #
alm.pred <- alm.dp$Y0plot %*% alm.synth$solution.w 

pdf("alm-ip-fl.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, alm.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 6, lwd = 1)
lines(1:8, alm.dp$Y1plot, lwd = 2)
lines(1:8, alm.pred, lwd = 2, lty = 2)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Floor Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Alison L. McHose", "Synthetic Alison L. McHose"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(3.25, .25, "First year after public financing", cex = 1.25)
arrows(5.05, .25, 5.95, .25, length = .1)
rug(assemb.floor$extremity, side = 2, ticksize = .025)
text(2.75, 2.3, bquote(paste("MSPE" == .(round(alm.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Samuel D. Thompson ##
# Data preparation #
sdt.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:3,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 136,
  controls.identifier = controls,  
  time.optimize.ssr = 1:3,
  time.plot = 1:8)

# Generate synthetic Samuel D. Thompson #
sdt.synth <- synth(sdt.dp, method = "BFGS")
sdt.diag <- synth.tab(dataprep.res = sdt.dp, synth.res = sdt.synth)

# Plot SDT and S-SDT #
sdt.pred <- sdt.dp$Y0plot %*% sdt.synth$solution.w 

pdf("sdt-ip-fl.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, sdt.dp$Y1plot, type = "n", ylim = c(0, 3.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 4, lwd = 1)
lines(1:8, sdt.dp$Y1plot, lwd = 2)
lines(1:8, sdt.pred, lwd = 2, lty = 2)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Floor Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topright", inset = .025, bty = "n", c("Samuel D. Thompson", "Synthetic Samuel D.", "Thompson"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(2.05, .25, "First year after", cex = 1.25)
text(2.05, .1, "public financing", cex = 1.25)
arrows(2.95, .25, 3.95, .25, length = .1)
rug(assemb.floor$extremity, side = 2, ticksize = .025)
text(2.25, 1.45, bquote(paste("MSPE" == .(round(sdt.synth$loss.v, 5)))), cex = 1.25)

dev.off()

# Plot gap between treatment/synthetics and placebo legislators #
gaps <- matrix(nrow = length(controls), ncol = 8)
mspe.ratios <- numeric(length(controls))
bad.fit <- numeric(28)
for(i in 1:28){
placebo.dp <- dataprep(assemb.floor, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:3,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = controls[i],
  controls.identifier = controls[-i],  
  time.optimize.ssr = 1:3,
  time.plot = 1:8)  
placebo.synth <- synth(placebo.dp, method = "BFGS")
placebo.pred <- placebo.dp$Y0plot %*% placebo.synth$solution.w
gaps[i, ] <- placebo.dp$Y1plot - (placebo.dp$Y0plot %*% placebo.synth$solution.w)
mspe.ratios[i] <- mean((placebo.dp$Y1plot[4:8] - placebo.pred[4:8])^2)/mean((placebo.dp$Y1plot[1:3] - placebo.pred[1:3])^2) 
bad.fit[i] <- as.numeric(ifelse(placebo.synth$loss.v > alm.synth$loss.v*5, 1, 0))
print(i)
}

# Treated legislator gaps #
lrg.gaps <- lrg.dp$Y1plot - (lrg.dp$Y0plot %*% lrg.synth$solution.w) 
ldg.gaps <- ldg.dp$Y1plot - (ldg.dp$Y0plot %*% ldg.synth$solution.w) 
gmj.gaps <- gmj.dp$Y1plot - (gmj.dp$Y0plot %*% gmj.synth$solution.w) 
alm.gaps <- alm.dp$Y1plot - (alm.dp$Y0plot %*% alm.synth$solution.w) 
sdt.gaps <- sdt.dp$Y1plot - (sdt.dp$Y0plot %*% sdt.synth$solution.w) 

# Treated legislator MSPE ratios #
lrg.ratio <- mean((lrg.dp$Y1plot[6:8] - lrg.pred[6:8])^2)/mean((lrg.dp$Y1plot[1:5] - lrg.pred[1:5])^2)
ldg.ratio <- mean((ldg.dp$Y1plot[4:8] - ldg.pred[4:8])^2)/mean((ldg.dp$Y1plot[1:3] - ldg.pred[1:3])^2)
gmj.ratio <- mean((gmj.dp$Y1plot[6:8] - gmj.pred[6:8])^2)/mean((gmj.dp$Y1plot[1:5] - gmj.pred[1:5])^2)
alm.ratio <- mean((alm.dp$Y1plot[6:8] - alm.pred[6:8])^2)/mean((alm.dp$Y1plot[1:5] - alm.pred[1:5])^2)
sdt.ratio <- mean((sdt.dp$Y1plot[4:8] - sdt.pred[4:8])^2)/mean((sdt.dp$Y1plot[1:3] - sdt.pred[1:3])^2)

# Treated legislator permutation p-values #
lrg.p <- length(which(mspe.ratios >= lrg.ratio))/(length(controls) + 5)
ldg.p <- length(which(mspe.ratios >= ldg.ratio))/(length(controls) + 5)
gmj.p <- length(which(mspe.ratios >= gmj.ratio))/(length(controls) + 5)
alm.p <- length(which(mspe.ratios >= alm.ratio))/(length(controls) + 5)
sdt.p <- length(which(mspe.ratios >= sdt.ratio))/(length(controls) + 5)

pdf("assemb-floor-gaps.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:8, ldg.gaps, type = "n", ylim = c(-1.5, 1.5), lwd = 2, xlab = "", ylab = "", axes = FALSE) 
box(); grid()
for(i in 1:28){
if(bad.fit[i] == 0){
lines(1:8, gaps[i, ], type = "l", col = "gray80", lwd = 2)
} else cat("bad fit", "\n")
} 
abline(h = 0, lwd = 1)
abline(v = 4, lwd = 1)
lines(1:8, ldg.gaps, lwd = 2, lty = 1)
lines(1:8, sdt.gaps, lwd = 2, lty = 2)
lines(1:8, alm.gaps, lwd = 2, lty = 3)
lines(1:8, lrg.gaps, lwd = 2, lty = 4)
lines(1:8, gmj.gaps, lwd = 2, lty = 5)
axis(1, at = 1:8, labels = seq(2003, 2010, 1), cex.axis = 1.25)
axis(2, at = seq(-1.5, 1.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Extremity from Synthetic Legislator", cex.lab = 1.5, line = 3.5)
legend(.6, 1.6, inset = 0, ncol = 2, bty = "n", c("Louis D. Greenwald", "Samuel D.", "Thompson", "Alison L. McHose", "Linda R. Greenstein", "Gordon M. Johnson", "Placebo Legislators"), lwd = 2, lty = c(1, 2, -1, 3, 4, 5, 1), col = c(rep("black", times = 6), "gray80"), cex = 1.25)
text(2.05, -1.125, "First year after", cex = 1.25)
text(2.05, -1.25, "public financing", cex = 1.25)
arrows(2.9, -1.125, 3.95, -1.125, length = .1)

dev.off()

save.image("assemb-floor-ip.RData")