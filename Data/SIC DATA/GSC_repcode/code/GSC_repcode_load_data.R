
# set working directory
# curcomp <- "C:/Users/iosgood/Dropbox/Globalizing supply chain/" 
setwd(paste(curcomp, "GSC_repcode", sep = ""))

# set seed for random numbers
set.seed(12345)

###############################################
## Load in the data and create new variables ##
###############################################

usdat <- read.csv("data/GSC_repdata_main.csv", header = T)
usdat$diff <- factor(usdat$diff, levels = c("Homogeneous","Mod. differentiated","Differentiated"))

# rank-based measures 
usdat$imp0509ps <- rank(usdat$imp0509/usdat$sales0509)
usdat$rpimp0509ps <- rank(usdat$rpimp0509/usdat$sales0509)
usdat$nonrpimp0509ps <- rank(usdat$nonrpimp0509/usdat$sales0509)
usdat$exp0509ps <- rank(usdat$exp0509/usdat$sales0509)
usdat$totusinp0509ps <- rank(usdat$totusinp0509/usdat$sales0509);
usdat$totusrpinp0509ps <- rank(usdat$totusrpinp0509/usdat$sales0509);
usdat$totusnonrpinp0509ps <- rank(usdat$totusnonrpinp0509/usdat$sales0509);
usdat$totusout0509ps <- rank(usdat$totusout0509/usdat$sales0509);

usdat$imp1014ps <- rank(usdat$imp1014/usdat$sales1014)
usdat$rpimp1014ps <- rank(usdat$rpimp1014/usdat$sales1014)
usdat$nonrpimp1014ps <- rank(usdat$nonrpimp1014/usdat$sales1014)
usdat$exp1014ps <- rank(usdat$exp1014/usdat$sales1014)
usdat$totusinp1014ps <- rank(usdat$totusinp1014/usdat$sales1014);
usdat$totusrpinp1014ps <- rank(usdat$totusrpinp1014/usdat$sales1014);
usdat$totusnonrpinp1014ps <- rank(usdat$totusnonrpinp1014/usdat$sales1014);
usdat$totusout1014ps <- rank(usdat$totusout1014/usdat$sales1014);

usdat$fdi1ps <- rank(usdat$fdi1/usdat$sales0509); 

# before and after agreement trade measures
sls0509 <- usdat$sales0509[1:403]; sls1014 <- usdat$sales1014[1:403];
aslsvec <- c(sls1014, rep(sls0509, 4), sls1014, sls0509, sls1014, sls0509, rep(NA, 403))
bslsvec <- c(sls1014, rep(sls0509, 4), sls1014, rep(sls0509, 3), rep(NA, 403))

usdat$asales <- aslsvec
usdat$arpimpps <- rank(usdat$arpimp/aslsvec)
usdat$anonrpimpps <- rank(usdat$anonrpimp/aslsvec)
usdat$aexpps <- rank(usdat$aexp/aslsvec)
usdat$ainpimpps <- rank(usdat$ainpimp/aslsvec)
usdat$ausoutps <- rank(usdat$ausout/aslsvec)

usdat$bsales <- bslsvec
usdat$brpimpps <- rank(usdat$brpimp/aslsvec)
usdat$bnonrpimpps <- rank(usdat$bnonrpimp/aslsvec)
usdat$bexpps <- rank(usdat$bexp/aslsvec)
usdat$binpimpps <- rank(usdat$binpimp/aslsvec)
usdat$busoutps <- rank(usdat$busout/aslsvec)

# is there a supporting or lobbying association: dichotomous variable
usdat$supassoc <- usdat$assposadcvd %in% c("Favor","Divided")
usdat$lobassoc <- usdat$numassoclob > 0
usdat$lobsupassoc <- usdat$numsupassoclob > 0

#
usdat$inforce <- 1; usdat$inforce[usdat$country %in% c("Latin America","Korea")] <- 0
usdat$year <- rep(c(2011, 2004, 2005, 2004, 2001, 2011, 2006, 2007, 2004, 1994), each = 403)

# a false outcome in the functions below
usdat$korrelus <- rnorm(nrow(usdat))

# no variation in outcomes; for fixed effects models
nofirmvar <- c()
for(i in unique(usdat$naicsdes)){
  nofirmvar[i] <- ifelse(sum(usdat$numsupfirm[usdat$naicsdes == i]) == 0, TRUE, FALSE)
} 
usdat$nofirmvar <- nofirmvar
noassocvar <- c()
for(i in unique(usdat$naicsdes)){
  noassocvar[i] <- ifelse(sum(usdat$supassoc[usdat$naicsdes == i]) %in% c(0,12) , TRUE, FALSE)
} 
usdat$noassocvar <- noassocvar

usdat$noclust3firmvar <- FALSE; usdat$noclust3assocvar <- FALSE
for(i in unique(usdat$clust3)){
  if(sum(usdat[usdat$clust3 == i, "numsupfirm"]) == 0){usdat[usdat$clust3 == i,"noclust3firmvar"] <- TRUE}
  if(sum(usdat[usdat$clust3 == i, "supassoc"]) == 0){usdat[usdat$clust3 == i,"noclust3assocvar"] <- TRUE}
}

write.csv(usdat, "data/GSC_repdata_main_cleaned.csv", row.names = F)

#############################
## Libraries and functions ##
#############################

## load all packages
library(foreign);
library(formula.tools); 
library(lme4);
library(lmtest);
library(sandwich); 
library(xtable);
library(Zelig);
require(pscl)

# clustered standard errors
get_CL_vcov<-function(model, cluster){
  require(sandwich, quietly = TRUE); require(lmtest, quietly = TRUE)
  #calculate degree of freedom adjustment
  M <- length(unique(cluster)); N <- length(cluster); K <- model$rank; dfc <- (M/(M-1))*((N-1)/(N-K))
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

# simulations for linear models
lscs2 <- function(model, cur, csvar, csquant, setvar = data.frame(), ln = FALSE, reps = 1){ 
  if(csvar != "diff" & ("diff" %in% names(setvar) == FALSE)) diff <- "Differentiated"
  factvar <- class(cur[,csvar]) == "factor"
  modobj <- mod <- lm(model, data = cur); 
  betas <- mvrnorm(reps*nrow(cur), mu = coef(mod), Sigma = vcov(mod)); 
  modeln <- model; lhs(modeln) <- quote(korrelus);
  mod <- lm(modeln, data = cur)
  cs0 <- ifelse(factvar, csquant[1], quantile(cur[,csvar], csquant[1])); cs1 <- ifelse(factvar, csquant[2], quantile(cur[,csvar], csquant[2]));
  vars <- all.vars(model)[all.vars(model) %in% names(setvar) == FALSE]
  cov.values <- cur[,vars]; if(length(names(setvar)) > 0){cov.values <- cbind(cov.values, setvar)};
  cov.values[,csvar] <- cs0; cov.values0 <- cov.values; cov.values0 <- do.call(rbind, replicate(reps, cov.values0, simplify=FALSE)) 
  cov.values[,csvar] <- cs1; cov.values1 <- cov.values; cov.values1 <- do.call(rbind, replicate(reps, cov.values1, simplify=FALSE)) 
  covs0 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values0)); pred0 <- c(); for(i in 1:nrow(cur)){pred0[i] <- sum(betas[i,]*covs0[i,])};
  covs1 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values1)); pred1 <- c(); for(i in 1:nrow(cur)){pred1[i] <- sum(betas[i,]*covs1[i,])};
  cs <- ifelse(ln, exp(pred1) - exp(pred0), pred1 - pred0); csout <- paste(round2(quantile(cs, .5), 2), regstars(cs), sep = "")
  cs <- c(csout)
  return(list(cs, quantile(pred0, c(.5, .025, .975)), quantile(pred1, c(.5, .025, .975))))
}

# simulations for negbin models
countcs2 <- function(model, cur, csvar, csquant, setvar = data.frame(), reps = 1, cluster = TRUE){ 
  clustvar <- cur$clust3
  if(csvar != "diff" & ("diff" %in% names(setvar) == FALSE)) diff <- "Differentiated"
  factvar <- class(cur[,csvar]) == "factor"
  modobj <- mod <- glm.nb(model, data = cur); 
  if(cluster){vcmat <- vcov(mod)}else{vcmat <- get_CL_vcov(modobj, cluster = clustvar)} 
  betas <- mvrnorm(reps*nrow(cur), mu = coef(mod), Sigma = vcov(mod)); 
  modeln <- model; lhs(modeln) <- quote(korrelus);
  mod <- lm(modeln, data = cur)
  cs0 <- ifelse(factvar, csquant[1], quantile(cur[,csvar], csquant[1])); cs1 <- ifelse(factvar, csquant[2], quantile(cur[,csvar], csquant[2]));
  vars <- all.vars(model)[all.vars(model) %in% names(setvar) == FALSE]
  cov.values <- cur[,vars]; if(length(names(setvar)) > 0){cov.values <- cbind(cov.values, setvar)};
  cov.values[,csvar] <- cs0; cov.values0 <- cov.values; cov.values0 <- do.call(rbind, replicate(reps, cov.values0, simplify=FALSE)) 
  cov.values[,csvar] <- cs1; cov.values1 <- cov.values; cov.values1 <- do.call(rbind, replicate(reps, cov.values1, simplify=FALSE)) 
  covs0 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values0)); pred0 <- c(); for(i in 1:nrow(cur)){pred0[i] <- sum(betas[i,]*covs0[i,])};
  covs1 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values1)); pred1 <- c(); for(i in 1:nrow(cur)){pred1[i] <- sum(betas[i,]*covs1[i,])};
  cs <- exp(pred1) - exp(pred0); csout <- paste(round2(quantile(cs, .5), 2), regstars(cs), sep = "")
  cs <- c(csout); 
  return(list(cs, quantile(exp(pred0), c(.5, .025, .975)), quantile(exp(pred1), c(.5, .025, .975))))
}

# simulations for logistic regression models
logcs2 <- function(model, cur, csvar, csquant, setvar = data.frame(), reps = 1, cluster = TRUE){ 
  clustvar <- cur$clust3
  if(csvar != "diff" & ("diff" %in% names(setvar) == FALSE)) diff <- "Differentiated"
  factvar <- class(cur[,csvar]) == "factor"
  modobj <- mod <- glm(model, data = cur, family = "binomial"); 
  if(cluster){vcmat <- vcov(mod)}else{vcmat <- get_CL_vcov(modobj, cluster = clustvar)} 
  betas <- mvrnorm(reps*nrow(cur), mu = coef(mod), Sigma = vcov(mod)); 
  modeln <- model; lhs(modeln) <- quote(korrelus);
  mod <- lm(modeln, data = cur)
  cs0 <- ifelse(factvar, csquant[1], quantile(cur[,csvar], csquant[1])); cs1 <- ifelse(factvar, csquant[2], quantile(cur[,csvar], csquant[2]));
  vars <- all.vars(model)[all.vars(model) %in% names(setvar) == FALSE]
  cov.values <- cur[,vars]; if(length(names(setvar)) > 0){cov.values <- cbind(cov.values, setvar)};
  cov.values[,csvar] <- cs0; cov.values0 <- cov.values; cov.values0 <- do.call(rbind, replicate(reps, cov.values0, simplify=FALSE)) 
  cov.values[,csvar] <- cs1; cov.values1 <- cov.values; cov.values1 <- do.call(rbind, replicate(reps, cov.values1, simplify=FALSE)) 
  covs0 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values0)); pred0 <- c(); for(i in 1:nrow(cur)){pred0[i] <- sum(betas[i,]*covs0[i,])};
  covs1 <- model.matrix(mod, data = data.frame(korrelus = 1, cov.values1)); pred1 <- c(); for(i in 1:nrow(cur)){pred1[i] <- sum(betas[i,]*covs1[i,])};
  cs <- inv.logit(pred1) - inv.logit(pred0); csout <- paste(round2(quantile(cs, .5), 2), regstars(cs), sep = "")
  cs <- c(csout)
  return(list(cs, quantile(inv.logit(pred0), c(.5, .025, .975)), quantile(inv.logit(pred1), c(.5, .025, .975))))
}

# 
interleave <- function(v1,v2){ord1 <- 2*(1:length(v1))-1; ord2 <- 2*(1:length(v2)); return(c(v1,v2)[order(c(ord1,ord2))])}

# significance test 
stars2 <- function(cfs, ses){ 
  tstat <- cfs/ses
  sts <- rep("", length(cfs))
  sts[abs(tstat) > qnorm(.975)] <- "^{*}"
  sts[abs(tstat) > qnorm(.995)] <- "^{**}"
  sts[abs(tstat) > qnorm(.9995)] <- "^{***}"
  return(sts)
}

regstars <- function(vec){ 
  sts <- ""
  if(0 < quantile(vec,  .025) | 0 > quantile(vec,  .975)){sts <- "^{*}"}
  if(0 < quantile(vec,  .005) | 0 > quantile(vec,  .995)){sts <- "^{**}"}
  if(0 < quantile(vec,  .0005) | 0 > quantile(vec,  .9995)){sts <- "^{***}"}
  return(sts)
}

# rounding function
round2 <- function(x, k = 2) format(round(x, k), nsmall=k)

# function for writing tables
ptable <- function(contents, file, addrow = FALSE){
  if(addrow == FALSE){addtorow <- list(pos=list(nrow(contents)), command = c("% filler"))}
  print(xtable(contents), type='latex', sanitize.text.function=identity, include.rownames = FALSE, include.colnames = FALSE, 
  file = file, only.contents = TRUE, hline.after = NULL, add.to.row = addtorow)
}

# for summary statistics
sums <- function(vec){v <- as.numeric(vec); return(c(mean(v, na.rm = T), sd(v, na.rm = T), min(v, na.rm = T), max(v, na.rm = T)))} 


