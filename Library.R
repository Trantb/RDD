#### Library
df <- df[df$purpose2 == "library",]
#Create the running variables
pctfor <- df$votes_for/(df$votes_against + df$votes_for)
df <- cbind(df, pctfor)
#Keep only renew tax to avoid endogenous variables
df <- df[df$description == "R",]
#Bind all covariances so that it is easier to write the robust function
covs <- cbind(df$pctblack, df$pcthisp, df$pctsinparhhld, 
              df$pctnokids, df$pctseparated, df$pctdivorced, 
              df$pctbachelors, df$pctgraddeg, df$lforcepartrate, deparse.level = 2)
#### Library
da <- select(df, contains('lead'))
da <- da[,order(colnames(da))]
library.lower <- data.frame(NULL)
library.higher <- data.frame(NULL)
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
    library.lower[i, 1] <- names(da)[i]
    library.higher[i, 1] <- names(da)[i]
    library.lower[i, j] <- ifelse(is.atomic(rdd.lower) == TRUE, 'na', 
                                  round(rdd.lower$pv[2], 3))
    library.higher[i, j] <- ifelse(is.atomic(rdd.higher) == TRUE, 'na',
                                   round(rdd.higher$pv[2], 3))
  }
}
