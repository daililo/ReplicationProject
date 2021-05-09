#################################################################################
# Do Campaign Donors Influence Polarization? Evidence from Public Financing in  #
# the American States                                                           #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# New Jersey Matching File -- Committee Vote Ideal Points                       #
# Last update: 1/7/15                                                           # 
#################################################################################
### Packages and Data ###
library(foreign)
library(doBy)
library(rbounds)
library(rgenoud)

assemb.comm <- read.dta("assemb-comm.dta")
assemb.comm$female <- ifelse(assemb.comm$Gender == "F", 1, 0)
assemb.comm$gop <- ifelse(assemb.comm$Party == "R", 1, 0)
predictors <- c("variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")

## Pre-treatment data to use in matching ##
match.data05 <- assemb.comm[assemb.comm$timenum < 3 & assemb.comm$idnum != 71 & assemb.comm$idnum != 82, c("Name", "idnum", "Year", "timenum", "PublicFinance", "extremity", "variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")]
match.data07 <- assemb.comm[assemb.comm$timenum < 4 & assemb.comm$idnum != 72 & assemb.comm$idnum != 141, c("Name", "idnum", "Year", "timenum", "PublicFinance", "extremity", "variance", "Leader", "MoneyRaised", "Seniority", "Vote", "Race", "female", "gop", "Pres_DemShare_2000")]

collapse05 <- summaryBy(extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 ~ idnum, keep.names = TRUE, data = match.data05)
collapse05$treat <- ifelse(collapse05$idnum == 72 | collapse05$idnum == 141, 1, 0)
collapse05 <- data.frame(collapse05, 
  extremitym = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.comm[assemb.comm$timenum > 2 & assemb.comm  
  $idnum != 71 & assemb.comm$idnum != 82, c("idnum", "extremity")])[ , 2],
  extremity1 = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.comm[assemb.comm$timenum == 3 & assemb.comm
  $idnum != 71 & assemb.comm$idnum != 82, c("idnum", "extremity")])[ , 2]) 

collapse07 <- summaryBy(extremity + variance + Leader + MoneyRaised + Seniority + Vote + Race + female + gop + Pres_DemShare_2000 ~ idnum, keep.names = TRUE, data = match.data07)
collapse07$treat <- ifelse(collapse07$idnum == 71 | collapse07$idnum == 82, 1, 0)
collapse07 <- data.frame(collapse07, 
  extremitym = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.comm[assemb.comm$timenum > 3 & assemb.comm  
  $idnum != 72 & assemb.comm$idnum != 141, c("idnum", "extremity")])[ , 2],
  extremity1 = summaryBy(extremity ~ idnum, keep.names = TRUE, data = assemb.comm[assemb.comm$timenum == 4 & assemb.comm
  $idnum != 72 & assemb.comm$idnum != 141, c("idnum", "extremity")])[ , 2])

full <- rbind(collapse05, collapse07[collapse07$idnum %in% c(71, 82), ])

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
