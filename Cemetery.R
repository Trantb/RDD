#### Cemetery
df <- read_dta("cosub_place_county_votes_property.dta")
df$purpose2
df <- df[df$purpose2 == "cemetery",]
#Create the running variables
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
#Keep only renew tax to avoid endogenous variables
df <- df[df$description == "R",]
#Bind all covariances so that it is easier to write the robust function
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
## Cemetery
library(tidyverse)
da <- select(df, contains('lead'))
de <- select(df, contains('lag'))
da <- cbind(da, de)
da <- da[,order(colnames(da))]
cemetery.lower <- data.frame(NULL)
cemetery.higher <- data.frame(NULL)
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
    cemetery.lower[i, 1] <- names(da)[i]
    cemetery.higher[i, 1] <- names(da)[i]
    cemetery.lower[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                   round(rdd.lower$pv[2], 3))
    cemetery.higher[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                    round(rdd.higher$pv[2], 3))
  }
}

##Keep year >=1995
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "cemetery",]
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
cemetery.lower.1995 <- data.frame(NULL)
cemetery.higher.1995 <- data.frame(NULL)
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
    cemetery.lower.1995[i, 1] <- names(da)[i]
    cemetery.higher.1995[i, 1] <- names(da)[i]
    cemetery.lower.1995[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                        round(rdd.lower$pv[2], 3))
    cemetery.higher.1995[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                         round(rdd.higher$pv[2], 3))
  }
}

##Keep year <=2011
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "cemetery",]
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
cemetery.lower.2011 <- data.frame(NULL)
cemetery.higher.2011 <- data.frame(NULL)
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
    cemetery.lower.2011[i, 1] <- names(da)[i]
    cemetery.higher.2011[i, 1] <- names(da)[i]
    cemetery.lower.2011[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                        round(rdd.lower$pv[2], 3))
    cemetery.higher.2011[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                         round(rdd.higher$pv[2], 3))
  }
}
