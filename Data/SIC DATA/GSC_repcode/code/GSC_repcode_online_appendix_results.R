
# set working directory
# curcomp <- "C:/Users/iosgood/Dropbox/Globalizing supply chain/" 
setwd(paste(curcomp, "GSC_repcode", sep = ""))

# set seed for random numbers
set.seed(12345)

# load in data
usdat <- read.csv("data/GSC_repdata_main_cleaned.csv", header = TRUE)
usdat$diff <- factor(usdat$diff, levels = c("Homogeneous","Mod. differentiated","Differentiated"))

########################
## Lobbying; Table A2 ##
########################
 
cur <- usdat[usdat$country %in% c("NAFTA") == FALSE,]; nrow(cur)
model = numfirmlob ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) 
fit <- glm.nb(model, data = cur); tabA2rsq <- as.numeric(round2(pR2(fit), 2)[5])

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

model = lobassoc  ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) 
fit <- glm(model, data = cur, family = "binomial"); tabA2rsq <- c(tabA2rsq, as.numeric(round2(pR2(fit), 2)[5]))

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
inf2 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);
# csvar = "rpimp0509"; csquant = c(.5, .5); cs1 <- logcs2(model, cur, csvar, csquant); cs1 

model = numfirmlob ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509+1) #+ country 
fit <- glm.nb(model, data = cur); tabA2rsq <- c(tabA2rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

model = lobassoc ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff*(nonrpimp0509ps + exp0509ps) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); tabA2rsq <- c(tabA2rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "nonrpimp0509ps"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "exp0509ps"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "sales0509"; csquant = c(.25, .75); cs8 <-  logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf4 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b)

cur <- usdat[usdat$country %in% c("NAFTA") == FALSE,]; nrow(cur)
model = numsupfirmlob ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) 
fit <- glm.nb(model, data = cur); tabA2rsq <- c(tabA2rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

model = lobsupassoc  ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) 
fit <- glm(model, data = cur, family = "binomial"); tabA2rsq <- c(tabA2rsq, as.numeric(round2(pR2(fit), 2)[5]))

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
inf6 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.")
tabA2 <- cbind(rep(vars, 1), cbind(inf1, inf2, inf3, inf4, inf5, inf6))
tabA2 <- rbind(tabA2, c("Pseudo-R$^2$", tabA2rsq))

addtorow <- list(); addtorow$pos <- list(3, 7, 10)
addtorow$command <- c( 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule', collapse=''), '  ')
)

ptable(cbind(tabA2), "paper/tables/tabA2.tex", addrow = T)

#########################
## Lobbying Robustness ##
#########################

cur <- usdat[usdat$country %in% c("NAFTA") == FALSE,]; 
model = numfirmlob ~ log(rpimp1014+1) + log(totusinp1014+1) + log(totusout1014+1) + diff*(log(nonrpimp1014+1) + log(exp1014+1)) + log(sales1014)
fit <- glm.nb(model, data = cur); frmtabA3rsq <- as.numeric(round2(pR2(fit), 2)[5])

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

model = lobassoc ~ log(rpimp1014+1) + log(totusinp1014+1) + log(totusout1014+1) + diff*(log(nonrpimp1014+1) + log(exp1014+1)) + log(sales1014) #+ country 
fit <- glm(model, data = cur, family = binomial); asstabA3rsq <- as.numeric(round2(pR2(fit), 2)[5])

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

model = numfirmlob ~ rpimp1014ps + totusinp1014ps + totusout1014ps + diff*(nonrpimp1014ps + exp1014ps) + log(sales1014) 
fit <- glm.nb(model, data = cur); frmtabA3rsq <- c(frmtabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

model = lobassoc ~ rpimp1014ps + totusinp1014ps + totusout1014ps + diff*(nonrpimp1014ps + exp1014ps) + log(sales1014) 
fit <- glm(model, data = cur, family = binomial); asstabA3rsq <- c(asstabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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
model = numfirmlob ~ log(fdi1+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); frmtabA3rsq <- c(frmtabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

model = lobassoc ~ log(fdi1+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); asstabA3rsq <- c(asstabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

cur <- usdat[usdat$country %in% c("NAFTA") == FALSE & usdat$sector == "manu",]; nrow(cur)
model = numfirmlob ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
fit <- glm.nb(model, data = cur); frmtabA3rsq <- c(frmtabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

model = lobassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) #+ country 
fit <- glm(model, data = cur, family = binomial); asstabA3rsq <- c(asstabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

cur <- usdat[(usdat$country %in% c("NAFTA") == FALSE & (is.na(usdat$estab)) == FALSE) & (usdat$sector %in% c("mining","ag")) == FALSE,]; nrow(cur)
model = numfirmlob ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) + log(estab+1) + concen4 + concen20 + pchiit + pcviit
fit <- glm.nb(model, data = cur); frmtabA3rsq <- c(frmtabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

cur <- usdat[(usdat$country %in% c("NAFTA") == FALSE) & (is.na(usdat$assocbudget) == FALSE) & (is.na(usdat$numassoc) == FALSE) & (usdat$sector %in% c("mining","ag") == FALSE),]; nrow(cur)
model = lobassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509) + log(assocbudget+1) + log(numassoc+1) + concen4 + concen20 + pchiit + pcviit 
fit <- glm(model, data = cur, family = binomial); asstabA3rsq <- c(asstabA3rsq, as.numeric(round2(pR2(fit), 2)[5]))

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

tabA3 <- cbind(c(inf1[1], "", inf1[2:10], "", "", "", "", "", "", ""), c(inf2[1], "", inf2[2:10], "", "", "", "", "", "", ""), 
      c(inf1a[1], "", inf1a[2:10], "", "", "", "", "", "", ""), c(inf2a[1], "", inf2a[2:10], "", "", "", "", "", "", ""),
      c("", inf3[1:10], "", "", "", "", "", "", ""), c("", inf4[1:10], "", "", "", "", "", "", ""),
      c(inf5[1], "", inf5[2:10], "", "", "", "", "", "", ""), c(inf6[1], "", inf6[2:10], "", "", "", "", "", "", ""), 
      c(inf7[1], "", inf7[2:11], "", "", inf7[12:15]), c(inf8[1], "", inf8[2:10], "", inf8[11:16]))
tabA3 <- tabA3[,c(1,3,5,7,9,2,4,6,8,10)]
vars <- c("Rel. party imports", "DIA", "Inputs", "Downs. exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.", 
          "Num. firms", "Assocs. budget", "Num. assocs.", "4-firm conc.", "20-firm conc.", "Pct. HIIT", "Pct. VIIT")
tabA3 <- cbind(vars, tabA3)
tabA3 <- rbind(tabA3, c("Pseudo-R$^2$", frmtabA3rsq, asstabA3rsq))

addtorow <- list(); addtorow$pos <- list(4, 8, 18)
addtorow$command <- c(
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0('\\midrule  ')
)

ptable(cbind(tabA3), "paper/tables/tabA3.tex", addrow = T)

#########################################
## Bootstrap standard errors; Table A4 ##
#########################################

## Firms
mod <- lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509), data = usdat)
mm <- model.matrix(mod)

bs_firms <- read.csv("data/GSC_repdata_bootstrap_firms.csv", colClasses = "character"); bs_firms <- bs_firms[(bs_firms$naicscode == "311424") ==FALSE,]
bs_firms <- apply(bs_firms[,5:ncol(bs_firms)], 2, as.numeric)

betas <- matrix(NA, nrow = length(coef(mod)), ncol = ncol(bs_firms))
for(i in 1:ncol(bs_firms)){
  bs_samp <- sample(1:nrow(mm), nrow(mm), replace = TRUE)
  betas[,i] <- solve(t(mm[bs_samp,])%*%mm[bs_samp,])%*%(t(mm[bs_samp,])%*%log(bs_firms[bs_samp,i]+1))
}
coef1 <- apply(betas, 1, mean); sd1 <- apply(betas, 1, sd)

inf0 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
inf1 <- interleave(round2(coef1, k=3), paste(round2(sd1, k = 3), stars2(coef1, sd1), sep = ""))  

## Associations
mod <- lm(I(100*supassoc) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509), data = usdat)
mm <- model.matrix(mod) 

bs_assocs <- read.csv("data/GSC_repdata_bootstrap_assocs.csv", colClasses = "character"); #  bs_assocs <- bs_assocs[(bs_assocs$naicscode == "311424") ==FALSE,]
bs_assocs <- apply(bs_assocs[,5:ncol(bs_assocs)], 2, as.numeric)
# cor(apply(bs_assocs, 1, mean), usdat$supassoc)

betas <- matrix(NA, nrow = length(coef(mod)), ncol = ncol(bs_assocs))
for(i in 1:ncol(bs_assocs)){
  bs_samp <- sample(1:nrow(mm), nrow(mm), replace = TRUE)
  betas[,i] <- solve(t(mm[bs_samp,])%*%mm[bs_samp,])%*%(t(mm[bs_samp,])%*%(100*bs_assocs[bs_samp,i]))
}
coef3 <- apply(betas, 1, mean); sd3 <- apply(betas, 1, sd)

inf2 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
inf3 <- interleave(round2(coef3, k=3), paste(round2(sd3, k = 3), stars2(coef3, sd3), sep = ""))  

tab <- cbind(inf0, inf1, inf2, inf3)
tab <- tab[c(3:12,19:26,13:18, 1:2),]
vars <- c("Rel. party imports", "Inputs (rel. party)", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Imports $\\times$ Mod. diff.", "Imports $\\times$ Diff.", "Exports $\\times$ Mod. diff.", "Exports $\\times$ Diff.",  
          "Sales","Mod. diff", "Diff.", "Intercept")
tab <- cbind(interleave(vars, rep("", 13)), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,18)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)

ptable(cbind(tab), "paper/tables/tabA4.tex", addrow = T)

#########################################################
## Robustness with alternative timing; Table A5 and A6 ##
#########################################################

## Model with trade matched to two years after entry into force of the agreement
cur <- usdat[usdat$country != "NAFTA",]; nrow(cur)
model = numsupfirm ~ arpimp + ainpimp + ausout + diff*(aexp + anonrpimp) + log(asales)
fit <- glm(model, data = cur); tabA5rsq <- as.numeric(round2(pR2(fit), 2)[5])

csvar = "arpimp"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "ainpimp"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "ausout"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "anonrpimp"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "anonrpimp"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "aexp"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "aexp"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "asales"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf1 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ arpimp + ainpimp + ausout + diff*(aexp + anonrpimp) + log(asales)
fit <- glm(model, data = cur, family = binomial); tabA5rsq <- c(tabA5rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "arpimp"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "ainpimp"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "ausout"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "anonrpimp"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "anonrpimp"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "aexp"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "aexp"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "asales"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf2 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = numsupfirm ~ arpimpps + ainpimpps + ausoutps + diff*(aexpps + anonrpimpps) + log(asales)
fit <- glm(model, data = cur); tabA5rsq <- c(tabA5rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "arpimpps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "ainpimpps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "ausoutps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "anonrpimpps"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "anonrpimpps"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "aexpps"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "aexpps"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "asales"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf3 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ arpimpps + ainpimpps + ausoutps + diff*(aexpps + anonrpimpps) + log(asales) 
fit <- glm(model, data = cur, family = binomial); tabA5rsq <- c(tabA5rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "arpimpps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "ainpimpps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "ausoutps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "anonrpimpps"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "anonrpimpps"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "aexpps"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "aexpps"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "asales"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf4 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.")
tabA5 <- cbind(rep(vars, 1), cbind(inf1, inf2, inf3, inf4));
tabA5 <- rbind(tabA5, c("Pseudo-R$^2$", tabA5rsq))

addtorow <- list(); addtorow$pos <- list(3, 7, 10)
addtorow$command <- c( 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule', collapse=''), '  ')
)

ptable(cbind(tabA5), "paper/tables/tabA5.tex", addrow = T)

## Model with trade matched to two years after entry into force of the agreement
cur <- usdat[(usdat$country %in% c("NAFTA","Jordan")) == FALSE,]; nrow(cur)
model = numsupfirm ~ brpimp + binpimp + busout + diff*(bexp + bnonrpimp) + log(bsales) 
fit <- glm(model, data = cur); tabA6rsq <- c(as.numeric(round2(pR2(fit), 2)[5]))

csvar = "brpimp"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "binpimp"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "busout"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "bnonrpimp"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "bnonrpimp"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "bexp"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "bexp"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "bsales"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf1 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ brpimp + binpimp + busout + diff*(bexp + bnonrpimp) + log(bsales)  
fit <- glm(model, data = cur, family = binomial); tabA6rsq <- c(tabA6rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "brpimp"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "binpimp"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "busout"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "bnonrpimp"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "bnonrpimp"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "bexp"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "bexp"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "bsales"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf2 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = numsupfirm ~ brpimpps + binpimpps + busoutps + diff*(bexpps + bnonrpimpps) + log(bsales) 
fit <- glm(model, data = cur); tabA6rsq <- c(tabA6rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "brpimpps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "binpimpps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "busoutps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "bnonrpimpps"; csquant = c(.25, .75); cs6a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "bnonrpimpps"; csquant = c(.25, .75); cs6b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "bexpps"; csquant = c(.25, .75); cs7a <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "bexpps"; csquant = c(.25, .75); cs7b <- countcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "bsales"; csquant = c(.25, .75); cs8 <- countcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- countcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- countcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf3 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

model = supassoc ~ brpimpps + binpimpps + busoutps + diff*(bexpps + bnonrpimpps) + log(bsales)  
fit <- glm(model, data = cur, family = binomial); tabA6rsq <- c(tabA6rsq, as.numeric(round2(pR2(fit), 2)[5]))

csvar = "brpimpps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "binpimpps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "busoutps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
csvar = "bnonrpimpps"; csquant = c(.25, .75); cs6a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs6a <- cs6a[[1]]
csvar = "bnonrpimpps"; csquant = c(.25, .75); cs6b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs6b <- cs6b[[1]]
csvar = "bexpps"; csquant = c(.25, .75); cs7a <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Homogeneous")); cs7a <- cs7a[[1]]
csvar = "bexpps"; csquant = c(.25, .75); cs7b <- logcs2(model, cur, csvar, csquant, data.frame(diff = "Differentiated")); cs7b <- cs7b[[1]]
csvar = "bsales"; csquant = c(.25, .75); cs8 <- logcs2(model, cur, csvar, csquant); cs8 <- cs8[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Mod. differentiated"); cs9a <- logcs2(model, cur, csvar, csquant); cs9a <- cs9a[[1]]
csvar = "diff"; csquant = c("Homogeneous", "Differentiated"); cs9b <- logcs2(model, cur, csvar, csquant); cs9b <- cs9b[[1]]
inf4 <- c(cs1, cs2, cs4, cs6a, cs6b, cs7a, cs7b, cs8, cs9a, cs9b);

vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports $\\times$ Homog.", "Imports $\\times$ Diff.", "Exports $\\times$ Homog.", "Exports $\\times$ Diff.", "Sales",
          "Homog. $\\rightarrow$ Mod.", "Homog. $\\rightarrow$ Diff.")
tabA6 <- cbind(rep(vars, 1), cbind(inf1, inf2, inf3, inf4));
tabA6 <- rbind(tabA6, c("Pseudo-R$^2$", tabA6rsq))

addtorow <- list(); addtorow$pos <- list(3, 7, 10)
addtorow$command <- c( 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule', collapse=''), '  ')
)

ptable(cbind(tabA6), "paper/tables/tabA6.tex", addrow = T)

###############################
## Subsets; Tables A7 and A8 ##
###############################

## Table A7: examine impact of removing NAFTA
cur <- usdat 
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm.nb(model, data = cur) 
inf1 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
cur <- usdat[usdat$country != "NAFTA",]; nrow(cur)
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm.nb(model, data = cur) 
inf2 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
cur <- usdat[(usdat$country %in% c("NAFTA","Jordan","Chile","Singapore")) == FALSE,];  nrow(cur)
model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm.nb(model, data = cur) 
inf3 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
tinf1 <- cbind(inf1, inf2, inf3)

cur <- usdat 
model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm(model, data = cur, family = "binomial") 
inf1 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
cur <- usdat[usdat$country != "NAFTA",]; 
model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm(model, data = cur, family = "binomial") 
inf2 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
cur <- usdat[(usdat$country %in% c("NAFTA","Jordan","Chile","Singapore")) == FALSE,]; 
model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff*(log(nonrpimp0509+1) + log(exp0509+1)) + log(sales0509)
mod <- glm(model, data = cur, family = "binomial") 
inf3 <- interleave(round2(coef(mod), k=3), paste(round2(sqrt(diag(vcov(mod))), k = 3), stars2(coef(mod), sqrt(diag(vcov(mod)))), sep = ""))  
tinf2 <- cbind(inf1, inf2, inf3)
tabA7 <- cbind(tinf1, tinf2)

tabA7 <- tabA7[c(3:12,19:26,13:18, 1:2),]
vars <- c("Rel. party imports", "Inputs (rel. party)", "Outputs", 
          "Imports (non. rel. party)", "Exports","Imports $\\times$ Mod. diff.", "Imports $\\times$ Diff.", "Exports $\\times$ Mod. diff.", "Exports $\\times$ Diff.",  
          "Sales","Mod. diff", "Diff.", "Intercept")
tabA7 <- cbind(interleave(vars, rep("", 13)), tabA7)
rownames(tabA7) <- NULL
addtorow <- list(); addtorow$pos <- list(6,18)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)

ptable(cbind(tabA7), "paper/tables/tabA7.tex", addrow = T)


## Table A8: subset by export-import ratio
expdiff <- usdat$exp0509 - usdat$nonrpimp0509
usdat$ca <- 2
usdat$ca[expdiff > quantile(expdiff, .666666666)] <- 3
usdat$ca[expdiff < quantile(expdiff, .3333333)] <- 1
table(usdat$ca)

cur <- usdat[usdat$ca == 1,]; model = numsupfirm ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff + log(sales0509) 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf1cd <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 2,]; 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf1nt <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 3,]; 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf1ca <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 1,]; model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff + log(sales0509) #+ country 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf2cd <- c(cs1, cs2, cs4)

cur <- usdat[usdat$ca == 2,]; model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff + log(sales0509) #+ country 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf2nt <- c(cs1, cs2, cs4)

cur <- usdat[usdat$ca == 3,]; model = supassoc ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + diff + log(sales0509) #+ country 
csvar = "rpimp0509"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf2ca <- c(cs1, cs2, cs4)

cur <- usdat[usdat$ca == 1,]; model = numsupfirm ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff + log(sales0509) 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf3cd <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 2,]; 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf3nt <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 3,]; 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- countcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- countcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- countcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf3ca <- c(cs1, cs2, cs4);

cur <- usdat[usdat$ca == 1,]; model = supassoc ~ rpimp0509ps + totusinp0509ps + totusout0509ps + diff + log(sales0509) #+ country 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf4cd <- c(cs1, cs2, cs4)

cur <- usdat[usdat$ca == 2,]; 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf4nt <- c(cs1, cs2, cs4)

cur <- usdat[usdat$ca == 3,]; 
csvar = "rpimp0509ps"; csquant = c(.25, .75); cs1 <- logcs2(model, cur, csvar, csquant); cs1 <- cs1[[1]];
csvar = "totusinp0509ps"; csquant = c(.25, .75); cs2 <- logcs2(model, cur, csvar, csquant); cs2 <- cs2[[1]]
csvar = "totusout0509ps"; csquant = c(.25, .75); cs4 <- logcs2(model, cur, csvar, csquant); cs4 <- cs4[[1]]
inf4ca <- c(cs1, cs2, cs4)

tabA8 <- rbind(cbind(inf1cd, inf1nt, inf1ca), cbind(inf2cd, inf2nt, inf2ca), 
             cbind(inf3cd, inf3nt, inf3ca), cbind(inf4cd, inf4nt, inf4ca))

vars <- c("Rel. party imports", "Inputs", "Downstream exports")

tabA8 <- cbind(rep(vars, 4), tabA8); row.names(tabA8) <- NULL

addtorow <- list(); addtorow$pos <- list(3, 6, 9)
addtorow$command <- c(paste0(paste0('\\midrule \\multicolumn{4}{@{}l}{\\uline{Change in assoc. support, logged totals}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{4}{@{}l}{\\uline{Change in firm support, \\%-age sales}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{4}{@{}l}{\\uline{Change in assoc. support,  \\%-age sales}:} \\vspace{2pt}', collapse=''), '\\\\')
)

ptable(cbind(tabA8), "paper/tables/tabA8.tex", addrow = T)

###############
## FE Models ##
###############

cur <- usdat
mod0 <-  lm(log(numsupfirm+1) ~ log(rpimp0509+1) + country, data = cur); summary(mod0)
cur$rpimpcountrymean <- NA
for(i in unique(cur$country)){
  cur$rpimpcountrymean[cur$country ==i] <- mean(log(cur$rpimp0509[cur$country == i]+1))
}
mod1 <-  lm(log(numsupfirm+1) ~ I(log(rpimp0509+1) - rpimpcountrymean), data = cur); summary(mod1)
summary(usdat$exp0509[usdat$country == "NAFTA"])

cur <- usdat
mod0 <-  lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + log(nonrpimp0509+1) + log(exp0509+1), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + log(nonrpimp0509+1) + log(exp0509+1) + country, data = cur)
cuts <- grepl("country", names(coef(mod1))) == FALSE
pchisq(-2*(logLik(mod0)[1] - logLik(mod1)[1]), df = length(coef(mod1)) - length(coef(mod0)))
inf1 <- interleave(round2(coef(mod1)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod1))[cuts]), k = 3), stars2(coef(mod1)[cuts], sqrt(diag(vcov(mod1)))[cuts]), sep = ""))  
cur <- usdat[usdat$noclust3firmvar == FALSE,];
mod2 = lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + log(nonrpimp0509+1) + log(exp0509+1) + clust3, data = cur)
cuts <- grepl("clust3", names(coef(mod2))) == FALSE
pchisq(-2*(logLik(mod0)[1] - logLik(mod2)[1]), df = length(coef(mod2)) - length(coef(mod0)))
inf2 <- interleave(round2(coef(mod2)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod2))[cuts]), k = 3), stars2(coef(mod2)[cuts], sqrt(diag(vcov(mod2)))[cuts]), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,];
mod3 = lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + log(nonrpimp0509+1) + log(exp0509+1) + naicsdes, data = cur)
cuts <- grepl("naicsdes", names(coef(mod3))) == FALSE
pchisq(-2*(logLik(mod0)[1] - logLik(mod3)[1]), df = length(coef(mod3)) - length(coef(mod0)))
inf3 <- interleave(round2(coef(mod3)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod3))[cuts]), k = 3), stars2(coef(mod3)[cuts], sqrt(diag(vcov(mod3)))[cuts]), sep = ""))  
mod4 = lm(log(numsupfirm+1) ~ log(rpimp0509+1) + log(totusinp0509+1) + log(totusout0509+1) + log(nonrpimp0509+1) + log(exp0509+1) + naicsdes + country, data = cur)
cuts <- (grepl("naicsdes", names(coef(mod4))) | grepl("country", names(coef(mod4))))  == FALSE
pchisq(-2*(logLik(mod0)[1] - logLik(mod4)[1]), df = length(coef(mod4)) - length(coef(mod0)))
inf4 <- interleave(round2(coef(mod4)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod4))[cuts]), k = 3), stars2(coef(mod4)[cuts], sqrt(diag(vcov(mod4)))[cuts]), sep = ""))  

N <- c("N", 4030, 4030, 3560, nrow(cur), nrow(cur))
rsq <- function(mod) round2(summary(mod)$r.squared, k =2)
rsqr <- c("R$^2$", rsq(mod0), rsq(mod1), rsq(mod2), rsq(mod3), rsq(mod4)); 

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\') 
)
ptable(cbind(tab), "paper/tables/tabA9.tex", addrow = T)

cur <- usdat  
mod0 <-  lm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + country, data = cur)
cuts <- grepl("country", names(coef(mod1))) == FALSE
inf1 <- interleave(round2(coef(mod1)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod1))[cuts]), k = 3), stars2(coef(mod1)[cuts], sqrt(diag(vcov(mod1)))[cuts]), sep = ""))  
cur <- usdat[usdat$noclust3assocvar == FALSE,];
mod2 = lm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + clust3, data = cur)
cuts <- grepl("clust3", names(coef(mod2))) == FALSE
inf2 <- interleave(round2(coef(mod2)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod2))[cuts]), k = 3), stars2(coef(mod2)[cuts], sqrt(diag(vcov(mod2)))[cuts]), sep = ""))  
cur <- usdat[usdat$noassocvar == FALSE,]; 
mod3 = lm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + naicsdes, data = cur)
cuts <- grepl("naicsdes", names(coef(mod3))) == FALSE
inf3 <- interleave(round2(coef(mod3)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod3))[cuts]), k = 3), stars2(coef(mod3)[cuts], sqrt(diag(vcov(mod3)))[cuts]), sep = ""))  
mod4 = lm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + naicsdes + country, data = cur)
cuts <- (grepl("naicsdes", names(coef(mod4))) | grepl("country", names(coef(mod4))))  == FALSE
inf4 <- interleave(round2(coef(mod4)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod4))[cuts]), k = 3), stars2(coef(mod4)[cuts], sqrt(diag(vcov(mod4)))[cuts]), sep = ""))  

N <- c("N", 4030, 4030, 4030, nrow(cur), nrow(cur))
rsq <- function(mod) round2(summary(mod)$r.squared, k =2)
rsqr <- c("R$^2$", rsq(mod0), rsq(mod1), rsq(mod2), rsq(mod3), rsq(mod4)); 

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\') 
)
ptable(cbind(tab), "paper/tables/tabA10.tex", addrow = T)

# 
cur <- usdat  
mod0 <-  lm(log(numsupfirm+1) ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lm(log(numsupfirm+1) ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + country, data = cur)
cuts <- grepl("country", names(coef(mod1))) == FALSE
inf1 <- interleave(round2(coef(mod1)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod1))[cuts]), k = 3), stars2(coef(mod1)[cuts], sqrt(diag(vcov(mod1)))[cuts]), sep = ""))  
cur <- usdat[usdat$noclust3firmvar == FALSE,];
mod2 = lm(log(numsupfirm+1) ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + clust3, data = cur)
cuts <- grepl("clust3", names(coef(mod2))) == FALSE
inf2 <- interleave(round2(coef(mod2)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod2))[cuts]), k = 3), stars2(coef(mod2)[cuts], sqrt(diag(vcov(mod2)))[cuts]), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,]; 
mod3 = lm(log(numsupfirm+1) ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + naicsdes, data = cur)
cuts <- grepl("naicsdes", names(coef(mod3))) == FALSE
inf3 <- interleave(round2(coef(mod3)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod3))[cuts]), k = 3), stars2(coef(mod3)[cuts], sqrt(diag(vcov(mod3)))[cuts]), sep = ""))  
mod4 = lm(log(numsupfirm+1) ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + naicsdes + country, data = cur)
cuts <- (grepl("naicsdes", names(coef(mod4))) | grepl("country", names(coef(mod4))))  == FALSE
inf4 <- interleave(round2(coef(mod4)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod4))[cuts]), k = 3), stars2(coef(mod4)[cuts], sqrt(diag(vcov(mod4)))[cuts]), sep = ""))  

N <- c("N", 4030, 4030, 4030, nrow(cur), nrow(cur))
rsq <- function(mod) round2(summary(mod)$r.squared, k =2)
rsqr <- c("R$^2$", rsq(mod0), rsq(mod1), rsq(mod2), rsq(mod3), rsq(mod4)); 

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\') 
)
ptable(cbind(tab), "paper/tables/tabA11.tex", addrow = T)

cur <- usdat  
mod0 <-  lm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + country, data = cur)
cuts <- grepl("country", names(coef(mod1))) == FALSE
inf1 <- interleave(round2(coef(mod1)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod1))[cuts]), k = 3), stars2(coef(mod1)[cuts], sqrt(diag(vcov(mod1)))[cuts]), sep = ""))  
cur <- usdat[usdat$noclust3assocvar == FALSE,];
mod2 = lm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + clust3, data = cur)
cuts <- grepl("clust3", names(coef(mod2))) == FALSE
inf2 <- interleave(round2(coef(mod2)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod2))[cuts]), k = 3), stars2(coef(mod2)[cuts], sqrt(diag(vcov(mod2)))[cuts]), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,]; 
mod3 = lm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + naicsdes, data = cur)
cuts <- grepl("naicsdes", names(coef(mod3))) == FALSE
inf3 <- interleave(round2(coef(mod3)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod3))[cuts]), k = 3), stars2(coef(mod3)[cuts], sqrt(diag(vcov(mod3)))[cuts]), sep = ""))  
mod4 = lm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + naicsdes + country, data = cur)
cuts <- (grepl("naicsdes", names(coef(mod4))) | grepl("country", names(coef(mod4))))  == FALSE
inf4 <- interleave(round2(coef(mod4)[cuts], k=3), paste(round2(sqrt(diag(vcov(mod4))[cuts]), k = 3), stars2(coef(mod4)[cuts], sqrt(diag(vcov(mod4)))[cuts]), sep = ""))  

N <- c("N", 4030, 4030, 4030, nrow(cur), nrow(cur))
rsq <- function(mod) round2(summary(mod)$r.squared, k =2)
rsqr <- c("R$^2$", rsq(mod0), rsq(mod1), rsq(mod2), rsq(mod3), rsq(mod4)); 

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\'),
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\') 
)
ptable(cbind(tab), "paper/tables/tabA12.tex", addrow = T)

###############
## RE models ##
###############

cur <- usdat  
mod0 <-  glm(log(numsupfirm+1) ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lmer(log(numsupfirm+1) ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|country), data = cur)
inf1 <- interleave(round2(fixef(mod1), k=3), paste(round2(sqrt(diag(vcov(mod1))), k = 3), stars2(fixef(mod1), sqrt(diag(vcov(mod1)))), sep = ""))  
cur <- usdat[usdat$noclust3firmvar == FALSE,];
mod2 <-  lmer(log(numsupfirm+1) ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|clust3), data = cur)
inf2 <- interleave(round2(fixef(mod2), k=3), paste(round2(sqrt(diag(vcov(mod2))), k = 3), stars2(fixef(mod2), sqrt(diag(vcov(mod2)))), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,];
mod3 <-  lmer(log(numsupfirm+1) ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|naicsdes), data = cur)
inf3 <- interleave(round2(fixef(mod3), k=3), paste(round2(sqrt(diag(vcov(mod3))), k = 3), stars2(fixef(mod3), sqrt(diag(vcov(mod3)))), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,];
mod4 <-  lmer(log(numsupfirm+1) ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|country) + (1|naicsdes), data = cur)
inf4 <- interleave(round2(fixef(mod4), k=3), paste(round2(sqrt(diag(vcov(mod4))), k = 3), stars2(fixef(mod4), sqrt(diag(vcov(mod4)))), sep = ""))  

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)
ptable(cbind(tab), "Paper/Tables/tabA13.tex", addrow = T)

cur <- usdat  
mod0 <-  glm(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lmer(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|country), data = cur)
inf1 <- interleave(round2(fixef(mod1), k=3), paste(round2(sqrt(diag(vcov(mod1))), k = 3), stars2(fixef(mod1), sqrt(diag(vcov(mod1)))), sep = ""))  
cur <- usdat[usdat$noclust3assocvar == FALSE,];
mod2 <-  lmer(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|clust3), data = cur)
inf2 <- interleave(round2(fixef(mod2), k=3), paste(round2(sqrt(diag(vcov(mod2))), k = 3), stars2(fixef(mod2), sqrt(diag(vcov(mod2)))), sep = ""))  
cur <- usdat[usdat$noassocvar == FALSE,]; 
mod3 <-  lmer(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|naicsdes), data = cur)
inf3 <- interleave(round2(fixef(mod3), k=3), paste(round2(sqrt(diag(vcov(mod3))), k = 3), stars2(fixef(mod3), sqrt(diag(vcov(mod3)))), sep = ""))  
mod4 <-  lmer(supassoc ~ log(rpimp0509+1)+ log(totusinp0509+1) + log(totusout0509+1) 
  + (log(nonrpimp0509+1) + log(exp0509+1)) + (1|country) + (1|naicsdes), data = cur)
inf4 <- interleave(round2(fixef(mod4), k=3), paste(round2(sqrt(diag(vcov(mod4))), k = 3), stars2(fixef(mod4), sqrt(diag(vcov(mod4)))), sep = ""))  

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)
ptable(cbind(tab), "paper/tables/tabA14.tex", addrow = T)

cur <- usdat  
mod0 <-  glm(numsupfirm ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)), data = cur)
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  lmer(numsupfirm ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|country), data = cur)
inf1 <- interleave(round2(fixef(mod1), k=3), paste(round2(sqrt(diag(vcov(mod1))), k = 3), stars2(fixef(mod1), sqrt(diag(vcov(mod1)))), sep = ""))  
cur <- usdat[usdat$noclust3firmvar == FALSE,];
mod2 <-  lmer(numsupfirm ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|clust3), data = cur)
inf2 <- interleave(round2(fixef(mod2), k=3), paste(round2(sqrt(diag(vcov(mod2))), k = 3), stars2(fixef(mod2), sqrt(diag(vcov(mod2)))), sep = ""))  
cur <- usdat[usdat$nofirmvar == FALSE,]; 
mod3 <-  lmer(numsupfirm ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|naicsdes), data = cur)
inf3 <- interleave(round2(fixef(mod3), k=3), paste(round2(sqrt(diag(vcov(mod3))), k = 3), stars2(fixef(mod3), sqrt(diag(vcov(mod3)))), sep = ""))  
mod4 <-  lmer(numsupfirm ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|country) + (1|naicsdes), data = cur)
inf4 <- interleave(round2(fixef(mod4), k=3), paste(round2(sqrt(diag(vcov(mod4))), k = 3), stars2(fixef(mod4), sqrt(diag(vcov(mod4)))), sep = ""))  

N <- c("N", 4030, 4030, 4030, 4030, 4030)

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)
ptable(cbind(tab), "Paper/Tables/tabA15.tex", addrow = T)

cur <- usdat  
mod0 <-  glm(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)), data = cur, family = "binomial")
inf0 <- interleave(round2(coef(mod0), k=3), paste(round2(sqrt(diag(vcov(mod0))), k = 3), stars2(coef(mod0), sqrt(diag(vcov(mod0)))), sep = ""))  
mod1 <-  glmer(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|country), data = cur, family = "binomial")
inf1 <- interleave(round2(fixef(mod1), k=3), paste(round2(sqrt(diag(vcov(mod1))), k = 3), stars2(fixef(mod1), sqrt(diag(vcov(mod1)))), sep = ""))  
cur <- usdat[usdat$noclust3assocvar == FALSE,];
mod2 <-  glmer(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|clust3), data = cur, family = "binomial")
inf2 <- interleave(round2(fixef(mod2), k=3), paste(round2(sqrt(diag(vcov(mod2))), k = 3), stars2(fixef(mod2), sqrt(diag(vcov(mod2)))), sep = ""))  
cur <- usdat[usdat$noassocvar == FALSE,]; 
mod3 <-  glmer(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|naicsdes), data = cur, family = "binomial")
inf3 <- interleave(round2(fixef(mod3), k=3), paste(round2(sqrt(diag(vcov(mod3))), k = 3), stars2(fixef(mod3), sqrt(diag(vcov(mod3)))), sep = ""))  
mod4 <-  glmer(supassoc ~ I(rpimp0509ps/1000) + I(totusinp0509ps/1000) + I(totusout0509ps/1000) 
  + (I(nonrpimp0509ps/1000) + I(exp0509ps/1000)) + (1|country) + (1|naicsdes), data = cur, family = "binomial")
inf4 <- interleave(round2(fixef(mod4), k=3), paste(round2(sqrt(diag(vcov(mod4))), k = 3), stars2(fixef(mod4), sqrt(diag(vcov(mod4)))), sep = ""))  

N <- c("N", 4030, 4030, 4030, 4030, 4030)

tab <- cbind(inf0, inf1, inf2, inf3, inf4)
tab <- tab[c(3:12,1:2),]
vars <- c("Rel. party imports", "Inputs", "Downstream exports", 
          "Imports (non. rel. party)", "Exports","Intercept")
tab <- cbind(interleave(vars, rep("", length(vars))), tab)
rownames(tab) <- NULL
addtorow <- list(); addtorow$pos <- list(6,10)
addtorow$command <- c(# paste0(paste0('\\multicolumn{5}{@{}l}{\\uline{Related-party and intermediates trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Ordinary trade}:} \\vspace{2pt}', collapse=''), '\\\\'), 
  paste0(paste0('\\midrule \\multicolumn{5}{@{}l}{\\uline{Other controls}:} \\vspace{2pt}', collapse=''), '\\\\')
)
ptable(cbind(tab), "paper/tables/tabA16.tex", addrow = T)



