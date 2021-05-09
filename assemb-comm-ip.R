#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# New Jersey Synthetic Case Control File -- Committee Vote Ideal Points         #
# Last update: 12/31/14                                                         # 
#################################################################################
### Packages and Data ###
library(foreign)
library(Synth)

assemb.comm <- read.dta("assemb-comm.dta")
assemb.comm$female <- ifelse(assemb.comm$Gender == "F", 1, 0)
assemb.comm$gop <- ifelse(assemb.comm$Party == "R", 1, 0)
treated <- c(71, 72, 82, 92, 141)
controls <- unique(assemb.comm$idnum[assemb.comm$PublicFinance == 0])
controls <- controls[-which(controls %in% treated)]
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

### Synthetic Case Control ### 

## Linda R. Greenstein ##
# Data preparation #
lrg.dp <- dataprep(assemb.comm, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:3,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 71,
  controls.identifier = controls,  
  time.optimize.ssr = 1:3,
  time.plot = 1:5)

# Generate synthetic Linda R. Greenstein #
lrg.synth <- synth(lrg.dp, method = "BFGS")
lrg.diag <- synth.tab(dataprep.res = lrg.dp, synth.res = lrg.synth)

# Plot LRG and S-LRG #
lrg.pred <- lrg.dp$Y0plot %*% lrg.synth$solution.w 

pdf("lrg-ip-co.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:5, lrg.dp$Y1plot, type = "n", ylim = c(0, 4.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 4, lwd = 1)
lines(1:5, lrg.dp$Y1plot, lwd = 2)
lines(1:5, lrg.pred, lwd = 2, lty = 2)
axis(1, at = 1:5, labels = seq(2002, 2010, 2), cex.axis = 1.25)
axis(2, at = seq(0, 4.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Committee Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Linda R. Greenstein", "Synthetic Linda R. Greenstein"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(2.25, .1, "First year after public financing", cex = 1.25)
arrows(3.3, .1, 3.95, .1, length = .1)
rug(assemb.comm$extremity, side = 2, ticksize = .025)
text(1.75, 1.4, bquote(paste("MSPE" == .(round(lrg.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Louis D. Greenwald ##
# Data preparation #
ldg.dp <- dataprep(assemb.comm, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:2,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 72,
  controls.identifier = controls,  
  time.optimize.ssr = 1:2,
  time.plot = 1:5)

# Generate synthetic Louis D. Greenwald #
ldg.synth <- synth(ldg.dp, method = "BFGS")
ldg.diag <- synth.tab(dataprep.res = ldg.dp, synth.res = ldg.synth)

# Plot LDG and S-LDG #
ldg.pred <- ldg.dp$Y0plot %*% ldg.synth$solution.w 

pdf("ldg-ip-co.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:5, ldg.dp$Y1plot, type = "n", ylim = c(0, 4.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 3, lwd = 1)
lines(1:5, ldg.dp$Y1plot, lwd = 2)
lines(1:5, ldg.pred, lwd = 2, lty = 2)
axis(1, at = 1:5, labels = seq(2002, 2010, 2), cex.axis = 1.25)
axis(2, at = seq(0, 4.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Committee Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Louis D. Greenwald", "Synthetic Louis D.", "Greenwald"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(1.75, .25, "First year after", cex = 1.25)
text(1.75, .1, "public financing", cex = 1.25)
arrows(2.3, .25, 2.95, .25, length = .1)
rug(assemb.comm$extremity, side = 2, ticksize = .025)
text(1.75, 1.55, bquote(paste("MSPE" == .(round(ldg.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Gordon M. Johnson ##
# Data preparation #
gmj.dp <- dataprep(assemb.comm, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:3,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 82,
  controls.identifier = controls,  
  time.optimize.ssr = 1:3,
  time.plot = 1:5)

# Generate synthetic Gordon M. Johnson #
gmj.synth <- synth(gmj.dp, method = "BFGS")
gmj.diag <- synth.tab(dataprep.res = gmj.dp, synth.res = gmj.synth)

# Plot GMJ and S-GMJ #
gmj.pred <- gmj.dp$Y0plot %*% gmj.synth$solution.w 

pdf("gmj-ip-co.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:5, gmj.dp$Y1plot, type = "n", ylim = c(0, 4.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 4, lwd = 1)
lines(1:5, gmj.dp$Y1plot, lwd = 2)
lines(1:5, gmj.pred, lwd = 2, lty = 2)
axis(1, at = 1:5, labels = seq(2002, 2010, 2), cex.axis = 1.25)
axis(2, at = seq(0, 4.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Committee Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Gordon M. Johnson", "Synthetic Gordon M. Johnson"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(2.25, .1, "First year after public financing", cex = 1.25)
arrows(3.3, .1, 3.95, .1, length = .1)
rug(assemb.comm$extremity, side = 2, ticksize = .025)
text(1.75, 1.4, bquote(paste("MSPE" == .(round(gmj.synth$loss.v, 5)))), cex = 1.25)

dev.off()

## Samuel D. Thompson ##
# Data preparation #
sdt.dp <- dataprep(assemb.comm, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:2,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = 141,
  controls.identifier = controls,  
  time.optimize.ssr = 1:2,
  time.plot = 1:5)

# Generate synthetic Samuel D. Thompson #
sdt.synth <- synth(sdt.dp, method = "BFGS")
sdt.diag <- synth.tab(dataprep.res = sdt.dp, synth.res = sdt.synth)

# Plot SDT and S-SDT #
sdt.pred <- sdt.dp$Y0plot %*% sdt.synth$solution.w 

pdf("sdt-ip-co.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:5, sdt.dp$Y1plot, type = "n", ylim = c(0, 4.5), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 3, lwd = 1)
lines(1:5, sdt.dp$Y1plot, lwd = 2)
lines(1:5, sdt.pred, lwd = 2, lty = 2)
axis(1, at = 1:5, labels = seq(2002, 2010, 2), cex.axis = 1.25)
axis(2, at = seq(0, 4.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Extremity in Committee Vote Ideal Points", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Samuel D. Thompson", "Synthetic Samuel D.", "Thompson"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)
text(1.75, .25, "First year after", cex = 1.25)
text(1.75, .1, "public financing", cex = 1.25)
arrows(2.3, .25, 2.95, .25, length = .1)
rug(assemb.comm$extremity, side = 2, ticksize = .025)
text(1.85, 1.55, bquote(paste("MSPE" == .(round(sdt.synth$loss.v, 5)))), cex = 1.25)

dev.off()

# Plot gap between treatment/synthetics and placebo legislators #
predictors2 <- predictors[-2] # For controls == 1 (John M. Bramnick)

gaps <- matrix(nrow = length(controls), ncol = 5)
mspe.ratios <- numeric(length(controls))
bad.fit <- numeric(22)
for(i in 1:22){
if(i == 1){
placebo.dp <- dataprep(assemb.comm, 
  predictors = predictors2,
  predictors.op = "mean",
  time.predictors.prior = 1:2,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = controls[i],
  controls.identifier = controls[-i],  
  time.optimize.ssr = 1:2,
  time.plot = 1:5)
} else
placebo.dp <- dataprep(assemb.comm, 
  predictors = predictors,
  predictors.op = "mean",
  time.predictors.prior = 1:2,
  dependent = "extremity",
  unit.variable = "idnum",
  unit.names.variable = "Name",
  time.variable = "timenum",
  treatment.identifier = controls[i],
  controls.identifier = controls[-i],  
  time.optimize.ssr = 1:2,
  time.plot = 1:5)
  
placebo.synth <- synth(placebo.dp, method = "BFGS")
placebo.pred <- placebo.dp$Y0plot %*% placebo.synth$solution.w
gaps[i, ] <- placebo.dp$Y1plot - placebo.pred 
mspe.ratios[i] <- mean((placebo.dp$Y1plot[3:5] - placebo.pred[3:5])^2)/mean((placebo.dp$Y1plot[1:2] - placebo.pred[1:2])^2)
bad.fit[i] <- as.numeric(ifelse(placebo.synth$loss.v > sdt.synth$loss.v*5, 1, 0))
print(i)
}

# Treated legislator gaps #
lrg.gaps <- lrg.dp$Y1plot - (lrg.dp$Y0plot %*% lrg.synth$solution.w) 
ldg.gaps <- ldg.dp$Y1plot - (ldg.dp$Y0plot %*% ldg.synth$solution.w) 
gmj.gaps <- gmj.dp$Y1plot - (gmj.dp$Y0plot %*% gmj.synth$solution.w) 
sdt.gaps <- sdt.dp$Y1plot - (sdt.dp$Y0plot %*% sdt.synth$solution.w) 

# Treated legislator MSPE ratios #
lrg.ratio <- mean((lrg.dp$Y1plot[4:5] - lrg.pred[4:5])^2)/mean((lrg.dp$Y1plot[1:3] - lrg.pred[1:3])^2)
ldg.ratio <- mean((ldg.dp$Y1plot[3:5] - ldg.pred[3:5])^2)/mean((ldg.dp$Y1plot[1:2] - ldg.pred[1:2])^2)
gmj.ratio <- mean((gmj.dp$Y1plot[4:5] - gmj.pred[4:5])^2)/mean((gmj.dp$Y1plot[1:3] - gmj.pred[1:3])^2)
sdt.ratio <- mean((sdt.dp$Y1plot[3:5] - sdt.pred[3:5])^2)/mean((sdt.dp$Y1plot[1:2] - sdt.pred[1:2])^2)

# Treated legislator permutation p-values #
lrg.p <- length(which(mspe.ratios >= lrg.ratio))/(length(controls) + 4)
ldg.p <- length(which(mspe.ratios >= ldg.ratio))/(length(controls) + 4)
gmj.p <- length(which(mspe.ratios >= gmj.ratio))/(length(controls) + 4)
sdt.p <- length(which(mspe.ratios >= sdt.ratio))/(length(controls) + 4)

pdf("assemb-comm-gaps.pdf")

par(mar = c(5, 5, .5, .5))
plot(1:5, ldg.gaps, type = "n", ylim = c(-1.5, 1.5), lwd = 2, xlab = "", ylab = "", axes = FALSE) 
box(); grid()
for(i in 1:22){
if(bad.fit[i] == 0){
lines(1:5, gaps[i, ], type = "l", col = "gray80", lwd = 2)
} else cat("bad fit", "\n")
} 
abline(h = 0, lwd = 1)
abline(v = 3, lwd = 1)
lines(1:5, ldg.gaps, lwd = 2, lty = 1)
lines(1:5, sdt.gaps, lwd = 2, lty = 2)
lines(1:5, lrg.gaps, lwd = 2, lty = 3)
lines(1:5, gmj.gaps, lwd = 2, lty = 4)
axis(1, at = 1:5, labels = seq(2002, 2010, 2), cex.axis = 1.25)
axis(2, at = seq(-1.5, 1.5, .5), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Extremity from Synthetic Legislator", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, ncol = 2, bty = "n", c("Louis D. Greenwald", "Samuel D. Thompson", "Linda R. Greenstein", "Gordon M. Johnson", "Placebo Legislators"), lwd = 2, lty = c(1, 2, 3, 4, 1), col = c(rep("black", times = 4), "gray80"), cex = 1.25)
text(1.75, -1.125, "First year after", cex = 1.25)
text(1.75, -1.25, "public financing", cex = 1.25)
arrows(2.3, -1.125, 2.95, -1.125, length = .1)

dev.off()

save.image("assemb-comm-ip.RData")