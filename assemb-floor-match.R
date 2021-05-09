#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# New Jersey Matching File -- Floor Vote Ideal Points                           #
# Last update: 1/7/15                                                           # 
#################################################################################
### Packages and Data ###
library(foreign)
library(doBy)
library(rbounds)

assemb.floor <- read.dta("assemb-floor.dta")
assemb.floor$female <- ifelse(assemb.floor$Gender == "F", 1, 0)
assemb.floor$gop <- ifelse(assemb.floor$Party == "R", 1, 0)
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

## Pre-treatment data to use in matching ##
match.data05 <- assemb.floor[assemb.floor$timenum < 4 & assemb.floor$idnum != 67 & assemb.floor$idnum != 78 & assemb.floor$idnum != 89, c("Name", "idnum", "Year", "timenum", "PublicFinance", "extremity", "variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")]
match.data07 <- assemb.floor[assemb.floor$timenum < 6 & assemb.floor$idnum != 68 & assemb.floor$idnum != 136, c("Name", "idnum", "Year", "timenum", "PublicFinance", "extremity", "variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")]

collapse05 <- summaryBy(extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 ~ idnum, keep.names = TRUE, data = match.data05)
collapse05$treat <- ifelse(collapse05$idnum == 68 | collapse05$idnum == 136, 1, 0)
collapse05 <- data.frame(collapse05, 
  extremitym = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.floor[assemb.floor$timenum > 3 & assemb.floor
  $idnum != 67 & assemb.floor$idnum != 78 & assemb.floor$idnum != 89, c("idnum", "extremity")])[ , 2],
  extremity1 = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.floor[assemb.floor$timenum == 4 & assemb.floor
  $idnum != 67 & assemb.floor$idnum != 78 & assemb.floor$idnum != 89, c("idnum", "extremity")])[ , 2]) 

collapse07 <- summaryBy(extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 ~ idnum, keep.names = TRUE, data = match.data07)
collapse07$treat <- ifelse(collapse07$idnum == 67 | collapse07$idnum == 78 | collapse07$idnum == 89, 1, 0)
collapse07 <- data.frame(collapse07, 
  extremitym = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.floor[assemb.floor$timenum > 5 & assemb.floor
  $idnum != 68 & assemb.floor$idnum != 136, c("idnum", "extremity")])[ , 2],
  extremity1 = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.floor[assemb.floor$timenum == 6 & assemb.floor
  $idnum != 68 & assemb.floor$idnum != 136, c("idnum", "extremity")])[ , 2])

full <- rbind(collapse05, collapse07[collapse07$idnum %in% c(67, 78, 89), ])

## Genetic matching ##
M <- 1 # Number of matches to find for each treated unit

genout <- GenMatch(Tr = full$treat, X = full[ , 2:11], BalanceMatrix = full[ , 2:11], pop.size = 1000, M = M)

# Outcome: mean post-treatment extremity
gmatchm <- Match(Y = full$extremitym, Tr = full$treat, X = full[ , 2:11], Weight.matrix = genout, M = M)
gbalancem <- MatchBalance(treat ~ extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000, match.out = gmatchm, data = full, nboots = 1000) 

# Rosenbaum bounds
grbm.e <- hlsens(gmatchm, Gamma = 3, GammaInc = .2)
grbm.p <- psens(gmatchm, Gamma = 3, GammaInc = .2)

# Outcome: first year post-treatment extremity
gmatch1 <- Match(Y = full$extremity1, Tr = full$treat, X = full[ , 2:11], Weight.matrix = genout, M = M)
gbalance1 <- MatchBalance(treat ~ extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000, match.out = gmatch1, data = full, nboots = 1000) 

# Rosenbaum bounds
grb1.e <- hlsens(gmatch1, Gamma = 3, GammaInc = .2)
grb1.p <- psens(gmatch1, Gamma = 3, GammaInc = .2)
