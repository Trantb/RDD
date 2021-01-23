df <- df[df$purpose2 == "police",]
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
df <- df[df$description == "R",]
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
da <- select(df, contains('lead'))
de <- select(df, contains('lag'))
da <- cbind(da, de)
da <- da[,order(colnames(da))]
police.lower <- data.frame(NULL)
police.higher <- data.frame(NULL)
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
    police.lower[i, 1] <- names(da)[i]
    police.higher[i, 1] <- names(da)[i]
    police.lower[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                 round(rdd.lower$pv[2], 3))
    police.higher[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                  round(rdd.higher$pv[2], 3))
  }
}

### keep year >=1995
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "police",]
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
police.lower.1995 <- data.frame(NULL)
police.higher.1995 <- data.frame(NULL)
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
    police.lower.1995[i, 1] <- names(da)[i]
    police.higher.1995[i, 1] <- names(da)[i]
    police.lower.1995[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                      round(rdd.lower$pv[2], 3))
    police.higher.1995[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                       round(rdd.higher$pv[2], 3))
  }
}

#### keep year <= 2011
df <- read_dta("cosub_place_county_votes_property.dta")
df <- df[df$purpose2 == "police",]
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
police.lower.2011 <- data.frame(NULL)
police.higher.2011 <- data.frame(NULL)
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
    police.lower.2011[i, 1] <- names(da)[i]
    police.higher.2011[i, 1] <- names(da)[i]
    police.lower.2011[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                      round(rdd.lower$pv[2], 3))
    police.higher.2011[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                       round(rdd.higher$pv[2], 3))
  }
}
