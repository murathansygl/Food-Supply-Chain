
# set working directory to GSC_repcode file here
curcomp <- "C:/Users/iosgood/Dropbox/Globalizing supply chain/" 
setwd(paste(curcomp, "GSC_repcode", sep = ""))

# replicate summary of positiontaking data in Table 2
source("code/GSC_repcode_data_summary.R")

# load data for main analysis, functions, libraries
source("code/GSC_repcode_load_data.R")

# replicate Tables 3-7 in the main text
source("code/GSC_repcode_maintext_results.R")

# replicate Tables A2-16 in online appendix
source("code/GSC_repcode_online_appendix_results.R")