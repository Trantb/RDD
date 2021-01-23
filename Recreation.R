library(rdrobust)
library(haven)
library(openxlsx)
library(tidyverse)

df <- read_dta("The datafile")

#keep current expenses only
df <- df[df$purpose2 == "current expenses",]

#Create the running variables - pctfor
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)

#Keep only renew tax to avoid endogenous variables
df <- df[df$description == "R",]

#Bind all covariances so that it is easier to write the robust function
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
#percentage of black population, percentage of hispanics population,
#percentage of having no childrean, separation, divorce, having bachelors degress
#graduate degree, labor force participation

#Bind all different bands into one vector
g <- data.frame('bwselect' = c('msesum', 'msecomb1', 'msecomb2', 'cerrd',
                               'certwo','cersum','cercomb1', 'cercomb2',
                               'mserd', 'msetwo'))
#Information about bandwidth can be found with rdrobust

## drop years before 1995 and after 2011
df <- df[df$year >= 1995,]
df <- df[df$year <= 2011,]

da <- select(df, contains('lead')) #Include all the lead
de <- select(df, contains('lag')) #Include all the lag
da <- cbind(da, de)
da <- da[,order(colnames(da))] # sort the name alphabetically
t(t(names(da)))
currentexp.lower <- data.frame(NULL) # lower 5% compared to the original bandwidth
currentexp.higher <- data.frame(NULL) # higher 5% compared to the original bandwidth
for (i in 1:length(da)){ # da includes all the dependent variables
  for (j in levels(g[,1])){ # g includes all the bandwidth we want to test
    rdd <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, covs = covs, covs_drop = T,
                             bwselect = j, all = 'iii'), 
                    error = function(e) paste('na')) # The first rdd is run
    rdd.lower <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 0.95*rdd$bws[1,], #the second one takes bandwidth of 95% the original one
                                   b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                   bwselect = j, all = 'iii'), 
                          error = function(e) paste('na'))
    rdd.higher <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 1.05*rdd$bws[1,], #the third one takes bandwidth of 105% the original one
                                    b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                    bwselect = j, all = 'iii'),
                           error = function(e) paste('na'))
    currentexp.lower[i, 1] <- names(da)[i]
    currentexp.higher[i, 1] <- names(da)[i]
    currentexp.lower[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                     round(rdd.lower$pv[2], 3))
    currentexp.higher[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                      round(rdd.higher$pv[2], 3))
  }
}
#Because a lot of dependent variables do not have any datapoints to calculate, they will stop and
#notice error. Therefore, the loop does not work anytime it encounters an error. Therefore, I use the
#tryCatch function so that the code can continue the loop and print na for any error. 



## keep year >=1995. Doing the same tasks above but excluding any year before 1995
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "recreation",]
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
df <- df[df$description == "R",]
df <- df[df$year >= 1995,]
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
da <- select(df, contains('lead'))
de <- select(df, contains('lag'))
da <- cbind(da, de)
da <- da[,order(colnames(da))]
recreation.lower.1995 <- data.frame(NULL)
recreation.higher.1995 <- data.frame(NULL)
for (i in 1:length(da)){
  for (j in levels(g[,1])){
    rdd <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, covs = covs, covs_drop = T,
                             bwselect = j, all = 'iii'), 
                    error = function(e) paste('na'))
    rdd.lower <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 0.95*rdd$bws[1,],
                                   b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                   bwselect = j, all = 'iii'), 
                          error = function(e) paste('na'))
    rdd.higher <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 1.05*rdd$bws[1,],
                                    b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                    bwselect = j, all = 'iii'),
                           error = function(e) paste('na'))
    recreation.lower.1995[i, 1] <- names(da)[i]
    recreation.higher.1995[i, 1] <- names(da)[i]
    recreation.lower.1995[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                          round(rdd.lower$pv[2], 3))
    recreation.higher.1995[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                           round(rdd.higher$pv[2], 3))
  }
}


## keep year <= 2011. Doing the same tasks as above but excluding any year after 2011.
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "recreation",]
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
df <- df[df$description == "R",]
df <- df[df$year <= 2011,]
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
da <- select(df, contains('lead'))
de <- select(df, contains('lag'))
da <- cbind(da, de)
da <- da[,order(colnames(da))]
recreation.lower.2011 <- data.frame(NULL)
recreation.higher.2011 <- data.frame(NULL)
for (i in 1:length(da)){
  for (j in levels(g[,1])){
    rdd <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, covs = covs, covs_drop = T,
                             bwselect = j, all = 'iii'), 
                    error = function(e) paste('na'))
    rdd.lower <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 0.95*rdd$bws[1,],
                                   b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                   bwselect = j, all = 'iii'), 
                          error = function(e) paste('na'))
    rdd.higher <- tryCatch(rdrobust(da[,i], df$pctfor, c = 0.5, h = 1.05*rdd$bws[1,],
                                    b = rdd$bws[2,], covs = covs, covs_drop = T, 
                                    bwselect = j, all = 'iii'),
                           error = function(e) paste('na'))
    recreation.lower.2011[i, 1] <- names(da)[i]
    recreation.higher.2011[i, 1] <- names(da)[i]
    recreation.lower.2011[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                          round(rdd.lower$pv[2], 3))
    recreation.higher.2011[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                           round(rdd.higher$pv[2], 3))
  }
}
