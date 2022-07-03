
# set working directory
# curcomp <- "C:/Users/iosgood/Dropbox/Globalizing supply chain/" 
setwd(paste(curcomp, "GSC_repcode", sep = ""))

# set seed for random numbers
set.seed(12345)

# load in data
usdat <- read.csv("data/GSC_repdata_main_cleaned.csv", header = TRUE)
usdat$diff <- factor(usdat$diff, levels = c("Homogeneous","Mod. differentiated","Differentiated"))

#################################
## Summary statistics; Table 3 ##
#################################

## Table 3
# outcomes
o2 <- sums(usdat$numsupfirm); o3 <- sums(usdat$supassoc)
ss <- rbind(o2, o3); ss[,c(1,2)] <- apply(ss[,c(1,2)], 2, round2, k = 2)
vars <- c("\\# Supporting firms", "Supporting assoc.") # "Support", 

usdat[usdat$numsupfirm == max(usdat$numsupfirm),c("naicscode","naicsdes","country")]
numsupfirmmax <- "321113, Sawmills; NAFTA"; numsupassocmax <- "-"

tab3p1 <- cbind(ss, c(numsupfirmmax, numsupassocmax))
ptable(cbind(vars, tab3p1), "paper/tables/tab3p1.tex")

o1 <- sums(usdat$numfirmlob[(usdat$country == "NAFTA") == FALSE]); o2 <- sums(usdat$numassoclob[(usdat$country == "NAFTA") == FALSE] > 0); 
ss <- rbind(o1, o2); ss[,c(1,2)] <- apply(ss[,c(1,2)], 2, round2, k = 2)
vars <- c("\\# lobbying firms", "Lobbying assoc.") # "Support", 
# print(xtable(cbind(vars, ss)), include.rownames = FALSE, sanitize.text.function = identity)

o1 <- sums(usdat$numsupfirmlob[(usdat$country == "NAFTA") == FALSE]); o2 <- sums(usdat$numsupassoclob[(usdat$country == "NAFTA") == FALSE] > 0); 
ss <- rbind(o1, o2); ss[,c(1,2)] <- apply(ss[,c(1,2)], 2, round2, k = 2)
vars <- c("... among supporting firms", "... with a supporting association") # "Support", 
# print(xtable(cbind(vars, ss)), include.rownames = FALSE, sanitize.text.function = identity)

# covariates
e1 <- sums(log10(usdat$rpimp0509 + 1));
e2 <- sums(log10(usdat$totusinp0509 + 1));
e2a <- sums(log10(usdat$totusnonrpinp0509 + 1));
e2b <- sums(log10(usdat$totusrpinp0509 + 1));
e3 <- sums(log10(usdat$totusout0509 + 1));
e4 <- sums(log10(usdat$nonrpimp0509 + 1))
e5 <- sums(log10(usdat$exp0509 + 1))
e6 <- sums(log10(usdat$sales0509)); 
e7a <- sums(usdat$diff == "Homogeneous"); 
e7b <- sums(usdat$diff == "Mod. differentiated"); 
e7c <- sums(usdat$diff == "Differentiated"); 

ss <- rbind(e1, e2, e2a, e2b, e3); ss[,1:4] <- apply(ss[,1:4], 2, round2, k = 2)
vars <- c("$\\ln$ Related-party imports", "$\\ln$ Inputs","$\\ln$ Inputs (related-party)", "$\\ln$ Inputs (non-related-party)", 
          "$\\ln$ Downstream exports")

usdat[usdat$rpimp0509 == max(usdat$rpimp0509),c("naicscode","naicsdes","country")];
rpimpmax <- "336111, Autos; NAFTA"
usdat[usdat$totusinp0509 == max(usdat$totusinp0509),c("naicscode","naicsdes","country")];
totusinpmax <- "324110, Petroleum refineries; NAFTA"
usdat[usdat$totusrpinp0509 == max(usdat$totusrpinp0509),c("naicscode","naicsdes","country")];
totusrpinpmax <- "\"$\\phantom{24110, Petroleum refineries; NAFT}$\""
usdat[usdat$totusnonrpinp0509 == max(usdat$totusnonrpinp0509),c("naicscode","naicsdes","country")];
totusnonrpinpmax <- "\"$\\phantom{24110, Petroleum refineries; NAFT}$\""
usdat[usdat$totusout0509 == max(usdat$totusout0509),c("naicscode","naicsdes","country")]
totusoutmax <- "211111, Oil and Gas Extraction; NAFTA"

tab3p2 <- cbind(ss, c(rpimpmax,totusinpmax, totusrpinpmax,totusnonrpinpmax, totusoutmax)); 
ptable(cbind(vars, tab3p2), "paper/tables/tab3p2.tex")

ss <- rbind(e4, e5, e7a, e7b, e7c, e6); ss[,1:4] <- apply(ss[,1:4], 2, round2, k = 2)
vars <- c("$\\ln$ Imports (Non-related party)", "$\\ln$ Exports",
          "Homogeneous", "Mod. differentiated", "Differentiated", "$\\ln$ Sales"
)

usdat[usdat$nonrpimp0509 == max(usdat$nonrpimp0509),c("naicscode","naicsdes","country")];
nonrpimpmax <- "211111, Oil and Gas Extraction; NAFTA"
usdat[usdat$exp0509 == max(usdat$exp0509),c("naicscode","naicsdes","country")]
expmax <- "336111, Autos; NAFTA"
usdat[usdat$sales0509 == max(usdat$sales0509),c("naicscode","naicsdes","country")]
salesmax <- "324110, Petroleum refineries"

tab3p3 <- cbind(ss, c(nonrpimpmax,expmax,"-","-","-",salesmax)); 
ptable(cbind(vars, tab3p3), "paper/tables/tab3p3.tex")

# some distribution of firm support across differentiation summary stats.
cur <- usdat; 
table(cur$numsupfirm[cur$diff == "Homogeneous"])/nrow(cur[cur$diff == "Homogeneous",])
table(cur$numsupfirm[cur$diff == "Differentiated"])/nrow(cur[cur$diff == "Differentiated",])
table(cur$numsupfirm[cur$diff == "Homogeneous" & cur$numsupfirm > 0])/nrow(cur[cur$diff == "Homogeneous" & cur$numsupfirm > 0,])
table(cur$numsupfirm[cur$diff == "Differentiated" & cur$numsupfirm > 0])/nrow(cur[cur$diff == "Differentiated" & cur$numsupfirm > 0,])
mean(cur$numsupfirm[cur$diff == "Homogeneous"])
mean(cur$numsupfirm[cur$diff == "Differentiated"])

##################################
## Main results; Tables 4 and 5 ##
##################################

## Table 4
cur <- usdat; 
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); tab4rsq <- as.numeric(round2(pR2(fit), 2)[5])
fit0 <- glm.nb(numsupfirm ~ 1, data = cur)

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf1 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);
# csvar = "rpimp0509"; csquant = c(.5, .5); cs1 <- countcs2(model, cur, csvar, csquant); cs1

model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial);  tab4rsq <- c(tab4rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf2 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)
# csvar = "rpimp0509"; csquant = c(.75, .25); cs1 <- logcs2(model, cur, csvar, csquant); cs1

model = numsupfirm ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509+1) #+ country 
fit <- glm.nb(model, data = cur); tab4rsq <- c(tab4rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <-  countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf3 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

model = supassoc ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); tab4rsq <- c(tab4rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf4 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.")
tab4 <- cbind(rep(vars, 1), cbind(inf1, inf2, inf3, inf4));
tab4 <- rbind(tab4, c("Pseudo-R$^2$", tab4rsq))

addtorow <- list(); addtorow$pos <- list(3, 7, 10)
addtorow$command <- c( 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule', collapse=''), '  ')
)

ptable(cbind(tab4), "paper/tables/tab4.tex", addrow = T)

## Table 5
cur <- usdat; 
model = numsupfirm ~ log(rpimp0509+1) + log(totusrpinp0509+1) + log(totusnonrpinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); tab5rsq <- as.numeric(round2(pR2(fit), 2)[5])
csvar = "totusrpinp0509"; csquant = c(.25, .75); cs1a <- countcs2(model, cur, csvar, csquant); cs1a <- cs1a[[1]]
csvar = "totusnonrpinp0509"; csquant = c(.25, .75); cs1b <- countcs2(model, cur, csvar, csquant); cs1b <- cs1b[[1]]
inf1 <- c(cs1a, cs1b)

model = supassoc ~ log(rpimp0509+1) + log(totusrpinp0509+1) + log(totusnonrpinp0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); tab5rsq <- c(tab5rsq, as.numeric(round2(pR2(fit), 2)[5]))
csvar = "totusrpinp0509"; csquant = c(.25, .75); cs1a <- logcs2(model, cur, csvar, csquant); cs1a <- cs1a[[1]]
csvar = "totusnonrpinp0509"; csquant = c(.25, .75); cs1b <- logcs2(model, cur, csvar, csquant); cs1b <- cs1b[[1]]
inf2 <- c(cs1a, cs1b)

model = numsupfirm ~ rpimp0509ps + totusrpinp0509ps + totusnonrpinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509)
fit <- glm.nb(model, data = cur); tab5rsq <- c(tab5rsq, as.numeric(round2(pR2(fit), 2)[5]))
csvar = "totusrpinp0509ps"; csquant = c(.25, .75); cs1a <- countcs2(model, cur, csvar, csquant); cs1a <- cs1a[[1]]
csvar = "totusnonrpinp0509ps"; csquant = c(.25, .75); cs1b <- countcs2(model, cur, csvar, csquant); cs1b <- cs1b[[1]]
inf3 <- c(cs1a, cs1b)

model = supassoc ~ rpimp0509ps + totusrpinp0509ps + totusnonrpinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509)
fit <- glm(model, data = cur, family = binomial); tab5rsq <- c(tab5rsq, as.numeric(round2(pR2(fit), 2)[5]))
csvar = "totusrpinp0509ps"; csquant = c(.25, .75); cs1a <- logcs2(model, cur, csvar, csquant); cs1a <- cs1a[[1]]
csvar = "totusnonrpinp0509ps"; csquant = c(.25, .75); cs1b <- logcs2(model, cur, csvar, csquant); cs1b <- cs1b[[1]]
inf4 <- c(cs1a, cs1b)

vars <- c("Inputs (rel. party)", "Inputs (non-rel. party)")
tab5 <- cbind(rep(vars, 1), cbind(inf1, inf2, inf3, inf4))
tab5 <- rbind(tab5, c("Pseudo-R$^2$", tab5rsq))

addtorow <- list(); addtorow$pos <- list(2); addtorow$command <- c(paste0(paste0('\\midrule', collapse=''), '  '))
ptable(cbind(tab5), "paper/tables/tab5.tex", addrow = T)

##############################
## Counterfactuals; Table 6 ##
##############################

# no global supply chains scenario
cur <- usdat
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)  
mod <- glm.nb(model, data = cur); vcmat <- get_CL_vcov(mod, cluster = cur$clust3); 
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(rpimp0509 + 1)")] <- log(.1*cur$rpimp0509+1); mm0[,c("log(totusinp0509 + 1)")] <- log(.1*cur$totusinp0509+1);  

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(exp(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(exp(cf1))}
inf1 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)  
mod <- glm(model, data = cur, family = "binomial"); vcmat <- get_CL_vcov(mod, cluster = cur$clust3)
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(rpimp0509 + 1)")] <- log(.1*cur$rpimp0509+1); mm0[,c("log(totusinp0509 + 1)")] <- log(.1*cur$totusinp0509+1);  

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(inv.logit(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(inv.logit(cf1))}
inf2 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

vars <- c("No. firms support", "Pr. assoc. support") 
tab6p1 <- cbind(rep(vars, 1), rbind(inf1, inf2)); row.names(tab6p1) <- NULL 
ptable(cbind(tab6p1), "paper/tables/tab6p1.tex")

# vastly greater trade deficits scenario
set.seed(12345); cur <- usdat
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
mod <- glm.nb(model, data = cur); vcmat <- get_CL_vcov(mod, cluster = cur$clust3); 
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(exp0509 + 1)")] <- log(.1*cur$exp0509+1); mm0[,c("log(nonrpimp0509 + 1)")] <- log(10*cur$nonrpimp0509+1);   

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(exp(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(exp(cf1))}
inf3 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
mod <- glm(model, data = cur, family = "binomial"); vcmat <- get_CL_vcov(mod, cluster = cur$clust3)
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(exp0509 + 1)")] <- log(.1*cur$exp0509+1); mm0[,c("log(nonrpimp0509 + 1)")] <- log(10*cur$nonrpimp0509+1);   

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(inv.logit(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(inv.logit(cf1))}
inf4 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

vars <- c("No. firms support", "Pr. assoc. support") 
tab6p2 <- cbind(rep(vars, 1), rbind(inf3, inf4)); row.names(tab6p2) <- NULL 
ptable(cbind(tab6p2), "paper/tables/tab6p2.tex")

# collapse of downstream exports scenario
set.seed(12345); cur <- usdat
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
mod <- glm.nb(model, data = cur); vcmat <- get_CL_vcov(mod, cluster = cur$clust3); 
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(totusout0509 + 1)")] <- log(.1*cur$totusout0509+1);   

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(exp(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(exp(cf1))}
inf5 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
mod <- glm(model, data = cur, family = "binomial"); vcmat <- get_CL_vcov(mod, cluster = cur$clust3)
betas <- mvrnorm(nrow(cur), mu = coef(mod), Sigma = vcmat); 
mm1 <- model.matrix(mod); mm0 <- mm1; mm0[,c("log(totusout0509 + 1)")] <- log(.1*cur$totusout0509+1); 

sup0 <- c(); sup1 <- c()
for(i in 1:nrow(betas)){cf0 <- mm0%*%betas[i,]; sup0[i] <- median(inv.logit(cf0)); cf1 <- mm1%*%betas[i,]; sup1[i] <- median(inv.logit(cf1))}
inf6 <- c(round2(median(sup1), 2), round2(median(sup0), 2), round2(quantile((sup1-sup0), .5), 2), 
  paste("[", round2(quantile((sup1-sup0), .025), 2), ", ", round2(quantile((sup1-sup0), .975), 2), "]", sep = ""))

vars <- c("No. firms support", "Pr. assoc. support") 
tab6p3 <- cbind(rep(vars, 1), rbind(inf5, inf6)); row.names(tab6p3) <- NULL 
ptable(cbind(tab6p3), "paper/tables/tab6p3.tex")


#########################
## Robustness; Table 7 ##
#########################

cur <- usdat; 
model = numsupfirm ~ log(rpimp1014+1) + log(totusinp1014+1) + log(totusout1014+1) + diff*(log(nonrpimp1014+1) + log(exp1014+1)) + log(sales1014)
fit <- glm.nb(model, data = cur); frmtab7rsq <- as.numeric(round2(pR2(fit), 2)[5])

csvar = "rpimp1014"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp1014"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout1014"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp1014"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp1014"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp1014"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp1014"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales1014"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf1 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ log(rpimp1014+1) + log(totusinp1014+1) + log(totusout1014+1) + diff*(log(nonrpimp1014+1) + log(exp1014+1)) + log(sales1014) #+ country 
fit <- glm(model, data = cur, family = binomial); asstab7rsq <- as.numeric(round2(pR2(fit), 2)[5])

csvar = "rpimp1014"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp1014"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout1014"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp1014"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp1014"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp1014"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp1014"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales1014"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf2 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

model = numsupfirm ~ rpimp1014ps + totusinp1014ps + totusout1014ps + diff*(nonrpimp1014ps + exp1014ps) + log(sales1014) 
fit <- glm.nb(model, data = cur); frmtab7rsq <- c(frmtab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp1014ps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp1014ps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout1014ps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp1014ps"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp1014ps"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp1014ps"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp1014ps"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales1014"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf1a <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ rpimp1014ps + totusinp1014ps + totusout1014ps + diff*(nonrpimp1014ps + exp1014ps) + log(sales1014) 
fit <- glm(model, data = cur, family = binomial); asstab7rsq <- c(asstab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp1014ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp1014ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout1014ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp1014ps"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp1014ps"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp1014ps"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp1014ps"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales1014"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf2a <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

##
model = numsupfirm ~ log(fdi1+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); frmtab7rsq <- c(frmtab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "fdi1"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf3 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ log(fdi1+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); asstab7rsq <- c(asstab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "fdi1"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf4 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

cur <- usdat[usdat$sector == "manu",]; nrow(cur)
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); frmtab7rsq <- c(frmtab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf5 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

cur <- usdat[usdat$sector == "manu",]; nrow(cur)
model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); asstab7rsq <- c(asstab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf6 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

cur <- usdat[(is.na(usdat$estab) == FALSE) & (usdat$sector %in% c("mining","ag")) == FALSE,]; nrow(cur)
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) + log(estab+1) + concen4 + concen20 + pchiit + pcviit
fit <- glm.nb(model, data = cur);  frmtab7rsq <- c(frmtab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
csvar = "estab"; csquant = c(.25, .75); cs12 <- countcs2(model, cur, csvar, csquant); cs12 <- cs12[[1]]
csvar = "concen4"; csquant = c(.25, .75); cs10a <- countcs2(model, cur, csvar, csquant); cs10a <- cs10a[[1]]
csvar = "concen20"; csquant = c(.25, .75); cs10b <- countcs2(model, cur, csvar, csquant); cs10b <- cs10b[[1]]
csvar = "pchiit"; csquant = c(.25, .75); cs11a <- countcs2(model, cur, csvar, csquant); cs11a <- cs11a[[1]]
csvar = "pcviit"; csquant = c(.25, .75); cs11b <- countcs2(model, cur, csvar, csquant); cs11b <- cs11b[[1]]

inf7 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b, cs12, cs10a, cs10b, cs11a, cs11b);

cur <- usdat[(is.na(usdat$assocbudget) == FALSE) & (is.na(usdat$numassoc) == FALSE) & (usdat$sector %in% c("mining","ag") == FALSE),]; nrow(cur)
model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) + log(assocbudget+1) + log(numassoc+1) + concen4 + concen20 + pchiit + pcviit 
fit <- glm(model, data = cur, family = binomial); asstab7rsq <- c(asstab7rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
csvar = "assocbudget"; csquant = c(.25, .75); cs12a <- logcs2(model, cur, csvar, csquant); cs12a <- cs12a[[1]]
csvar = "numassoc"; csquant = c(.25, .75); cs12b <- logcs2(model, cur, csvar, csquant); cs12b <- cs12b[[1]]
csvar = "concen4"; csquant = c(.25, .75); cs10a <- logcs2(model, cur, csvar, csquant); cs10a <- cs10a[[1]]
csvar = "concen20"; csquant = c(.25, .75); cs10b <- logcs2(model, cur, csvar, csquant); cs10b <- cs10b[[1]]
csvar = "pchiit"; csquant = c(.25, .75); cs11a <- logcs2(model, cur, csvar, csquant); cs11a <- cs11a[[1]]
csvar = "pcviit"; csquant = c(.25, .75); cs11b <- logcs2(model, cur, csvar, csquant); cs11b <- cs11b[[1]]

inf8 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b, cs12a, cs12b, cs10a, cs10b, cs11a, cs11b);

tab7 <- cbind(c(inf1[1], "", inf1[2:10], "", "", "", "", "", "", ""), c(inf2[1], "", inf2[2:10], "", "", "", "", "", "", ""), 
      c(inf1a[1], "", inf1a[2:10], "", "", "", "", "", "", ""), c(inf2a[1], "", inf2a[2:10], "", "", "", "", "", "", ""),
      c("", inf3[1:10], "", "", "", "", "", "", ""), c("", inf4[1:10], "", "", "", "", "", "", ""),
      c(inf5[1], "", inf5[2:10], "", "", "", "", "", "", ""), c(inf6[1], "", inf6[2:10], "", "", "", "", "", "", ""), 
      c(inf7[1], "", inf7[2:11], "", "", inf7[12:15]), c(inf8[1], "", inf8[2:10], "", inf8[11:16]))
tab7 <- tab7[,c(1,3,5,7,9,2,4,6,8,10)]
vars <- c("Rel. party imports", "DIA", "Inputs", "Downs. exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.", 
          "Num. firms", "Assocs. budget", "Num. assocs.", "4-firm conc.", "20-firm conc.", "Pct. HIIT", "Pct. VIIT")
tab7 <- cbind(vars, tab7)
tab7 <- rbind(tab7, c("Pseudo-R$^2$", frmtab7rsq, asstab7rsq))

addtorow <- list(); addtorow$pos <- list(4, 8, 18)
addtorow$command <- c(
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0('\\midrule  ')
)

ptable(cbind(tab7), "paper/tables/tab7.tex", addrow = T)



