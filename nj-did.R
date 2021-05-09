#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# New Jersey Difference-in-Differences File                                     #
# Last update: 1/4/14                                                           # 
#################################################################################
library(foreign)

### Floor Votes ###
assemb.floor <- read.dta("assemb-floor.dta")
assemb.floor$female <- ifelse(assemb.floor$Gender == "F", 1, 0)
assemb.floor$gop <- ifelse(assemb.floor$Party == "R", 1, 0)
treated <- c(67, 68, 78, 89, 136)
controls <- unique(assemb.floor$idnum[assemb.floor$PublicFinance == 0])
controls <- controls[-which(controls %in% treated)]
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

assemb.floor$post.treat <- ifelse(assemb.floor$timenum > 3, 1, 0)
assemb.floor$treated <- ifelse(assemb.floor$idnum %in% treated, 1, 0)

floor.did <- lm(extremity ~ variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 + post.treat + treated + post.treat*treated, data = assemb.floor)

round(coef(floor.did), 3)
round(confint(floor.did)["post.treat:treated", ], 3)
round(range(assemb.floor$extremity), 3)
round(confint(floor.did)["post.treat:treated", ], 3)[2]/sd(assemb.floor$extremity)

### Committee Votes ###
assemb.comm <- read.dta("assemb-comm.dta")
assemb.comm$female <- ifelse(assemb.comm$Gender == "F", 1, 0)
assemb.comm$gop <- ifelse(assemb.comm$Party == "R", 1, 0)
treated <- c(71, 72, 82, 92, 141)
controls <- unique(assemb.comm$idnum[assemb.comm$PublicFinance == 0])
controls <- controls[-which(controls %in% treated)]
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

assemb.comm$post.treat <- ifelse(assemb.comm$timenum > 3, 1, 0)
assemb.comm$treated <- ifelse(assemb.comm$idnum %in% treated, 1, 0)

comm.did <- lm(extremity ~ variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 + post.treat + treated + post.treat*treated, data = assemb.comm)

round(coef(comm.did), 3)
round(confint(comm.did)["post.treat:treated", ], 3)
round(range(assemb.comm$extremity), 3)
round(confint(comm.did)["post.treat:treated", ], 3)[2]/sd(assemb.comm$extremity)