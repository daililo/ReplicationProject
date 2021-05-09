#################################################################################
# Public Financing and the Electoral Connection Between                         #
# Private Donors and Legislators                                                #
# Jeffrey J. Harden and Justin H. Kirkland                                      #
# State-Level Difference-in-Differences File                                    #
# Last update: 1/4/14                                                           # 
#################################################################################
library(foreign)

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

stmed$post.treat <- ifelse(stmed$year > 2000, 1, 0)
stmed$treated <- ifelse(stmed$alpha %in% c(3, 19), 1, 0)

st.did <- lm(pm3 ~ ideology + ideoex + dividedgov + gopshare + deficit + squire_score + income + unemployment + population + populationdensity + grants + gsp + termlimits_house + termlimits_senate + demcontrol + clp + govideo2 + west + mwest + south + post.treat + treated + post.treat*treated, data = stmed)

round(coef(st.did), 3)
round(confint(st.did)["post.treat:treated", ], 3)
round(range(stmed$pm3), 3)
round(confint(st.did)["post.treat:treated", ], 3)[2]/sd(stmed$pm3)
