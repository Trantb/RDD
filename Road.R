df <- df[df$purpose2 == "road",]
#Create the running variables
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
#Keep only renew tax to avoid endogenous variables
df <- df[df$description == "R",]
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
## Road
library(tidyverse)
da <- select(df, contains('lead'))
de <- select(df, contains('lag'))
da <- cbind(da, de)
da <- da[,order(colnames(da))]
road.lower <- data.frame(NULL)
road.higher <- data.frame(NULL)
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
    road.lower[i, 1] <- names(da)[i]
    road.higher[i, 1] <- names(da)[i]
    road.lower[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                               round(rdd.lower$pv[2], 3))
    road.higher[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                round(rdd.higher$pv[2], 3))
  }
}

#keep year >=1995
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "road",]
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
road.lower.1995 <- data.frame(NULL)
road.higher.1995 <- data.frame(NULL)
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
    road.lower.1995[i, 1] <- names(da)[i]
    road.higher.1995[i, 1] <- names(da)[i]
    road.lower.1995[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                    round(rdd.lower$pv[2], 3))
    road.higher.1995[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                     round(rdd.higher$pv[2], 3))
  }
}

## keep year <= 2011
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "road",]
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
road.lower.2011 <- data.frame(NULL)
road.higher.2011 <- data.frame(NULL)
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
    road.lower.2011[i, 1] <- names(da)[i]
    road.higher.2011[i, 1] <- names(da)[i]
    road.lower.2011[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                    round(rdd.lower$pv[2], 3))
    road.higher.2011[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                     round(rdd.higher$pv[2], 3))
  }
}
