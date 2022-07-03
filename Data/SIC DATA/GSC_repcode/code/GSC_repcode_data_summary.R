
# set working directory
# curcomp <- "C:/Users/iosgood/Dropbox/Globalizing supply chain/" 
setwd(paste(curcomp, "GSC_repcode", sep = ""))

# load packages and define functions
library(xtable)
ptable <- function(contents, file, addrow = FALSE){
  if(addrow == FALSE){addtorow <- list(pos=list(nrow(contents)), command = c("% filler"))}
  print(xtable(contents), type='latex', sanitize.text.function=identity, include.rownames = FALSE, include.colnames = FALSE, 
  file = file, only.contents = TRUE, hline.after = NULL, add.to.row = addtorow)
}

######################################################################
## Collect information on each agreement's position-taking; Table 2 ##
######################################################################

cases <- c("KORUS", "AUSUS", "CAFTA", "Latin America", "MEFTA", "Chile", "Jordan", "Singapore", "Peru", "NAFTA")
country <- c("Korea", "Australia", "Central America", "Latin America", "Middle East", "Chile", "Jordan", "Singapore", "Peru", "NAFTA")
adj <- c("Korean", "Australian", "Central American", "Latin American", "Middle Eastern", "Chilean", "Jordanian", "Singaporean", "Peruvian", "North American")
cty <- c("kor", "aus", "caf", "all", "mef", "chl", "jor", "sng","per","naf") 
years <- c("2011", "2004", "2005", "2011", "2006", "2004", "2001","2004","2007","1994")

casedata <- list()

for(i in 1:length(cases)){
  # US associations 
  assocname <- paste(curcomp, "GSC_repcode/data/position data/", cases[i], " association positions evidence.csv", sep = "")
  assoc <- read.csv(assocname, header = TRUE); if(cases[i] == "KORUS") assoc <- assoc[,-3]

  # supporting US firms
  supfirmname <- paste(curcomp, "GSC_repcode/data/position data/", "All supporting firms industries ", cases[i], ".csv", sep = "")
  supfirm <- read.csv(supfirmname, header = FALSE)

  # function for counting sources
  sourcecount <- function(vec) return(sum(unlist(strsplit(vec, "/")) %in% c("", " ", NA) == FALSE))
  uniqsourcecount <- function(mat) return(length(unique(unlist(strsplit(as.character(unlist(mat)), "/")))))

  # association summary
  numassfav <- sum(assoc[,2] %in% c("Favor", "Oppose:ImputeFavor"));
  sources <- assoc[assoc[,2] %in% c("Favor", "Oppose:ImputeFavor","", " "), 3:ncol(assoc)]; 
  if(ncol(assoc)==3){assocsourcecount <- as.numeric(sources %in% c("", " ", NA) == FALSE)}else{assocsourcecount <- apply(sources, 1, sourcecount)}
  avgassocsource <- mean(assocsourcecount[assocsourcecount > 0]) 
  uniqassocsource <- uniqsourcecount(sources)
  if(assoc[1,1] == "Holder") {numassfav <- 0; numassopp <- 0; avgassocsource <- NA; uniqassocsource <- NA}

  # firm summary
  numfirmfav <- nrow(supfirm[supfirm[,5] %in% c("") == FALSE,])
  sources <- rbind(supfirm[,2:4]); # sources <- rbind(supfirm[,2:4], oppfirm[,2:4]); 
  firmsourcecount <- apply(sources, 1, sourcecount)
  avgfirmsource <- mean(firmsourcecount[firmsourcecount > 0])
  uniqfirmsource <- uniqsourcecount(sources)

  casedata[[i]] <- c(cases[i], years[i],
    numassfav, avgassocsource, uniqassocsource,
    numfirmfav, avgfirmsource, uniqfirmsource
  )
}

names(casedata) <- cases

dom <- c(1:8) 
summ <- rbind(casedata$NAFTA[dom], 
          casedata$Jordan[dom],
	    casedata$AUSUS[dom],
          casedata$Chile[dom],
          casedata$Singapore[dom],
          casedata$CAFTA[dom],
          casedata$MEFTA[dom],
          casedata$Peru[dom],
          casedata[names(casedata) == "Latin America"][[1]][dom],
          casedata$KORUS[dom]
)

summ <- data.frame(summ)
summ[,3:8] <- apply(summ[,3:8], 2, as.numeric)
summ[,3:8] <- apply(summ[,3:8], 2, round, 2)

names(summ) <- c("Agreement", "Year",
      "# support","Sources", "Unique", 
      "# support","Sources", "Unique")

levels(summ$Year) <- c(levels(summ$Year), "")
summ <- summ[,c(1:2,6:8,3:5)]

summ[,1] <- as.character(summ[,1])
summ[7,1] <- "Bah/Mor/Oman"
summ[9,1] <- "Col/Pan"

print(xtable(summ, digits = c(0, 0, 4, 0, 2, 0, 0, 2, 0)), type='latex', sanitize.text.function=identity, 
  include.rownames = FALSE, include.colnames = FALSE, 
  file = "paper/tables/tab2.tex", only.contents = TRUE, hline.after = NULL)



