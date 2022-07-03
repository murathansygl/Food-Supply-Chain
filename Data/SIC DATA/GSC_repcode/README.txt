
1. Table of contents. 

The complGSC script which replicates all materials from the paper is contained in 
GSC_repcode and is called "GSC_repcode_master.R". You must set an appropriate
path to the folder on your computer which holds the replication folder, GSC_repcode.
This path should be set to the curcomp object located at the top of GSC_replication_script.R.

The replication file GSC_repcode contains four main folders:

code: this file contains the scripts used to produce the tables. 
The main scripts are "GSC_repcode_data_summary.R"; "GSC_repcode_load_data.R";
"GSC_repcode_maintext_results.R"; and "GSC_repcode_online_appendix_results.R".
All of the results contained in the main text are computed using the first
three of these files.

data: this file contains all of the datasets used to replicate the main results.
These are the main data set with the outcome and explanatory variables (GSC_repdata_main); 
a cleaned version of the data that is written by "GSC_repcode_load_data.R" and is loaded
by all subsequent scripts; two datafiles that contain 5000 bootstrapped samples of the outcome
variables; and a subfolder (data/position data) which contains data on firm and association
positiontaking on the agreements.

paper: this file contains a subfolder (paper/tables) which holds all of the tables as 
.tex documents. Because the replication scripts write to paper/tables, there is also a 
permanent version of the tables in the subfolder paper/permanent tables to facilitate 
comparison.


2. General replication directions, and Table and Figure notes.
-to replicate all of the data cleaning and analysis, run the script
"GSC_repcode_master.R". You must set a working directory with a path
leading to the folder containing GSC_repcode. Note that the complete replication
takes around 7 minutes. All tables are written to paper/tables.

-note that the various individual scripts are not modular. For example, the functions 
in "GSC_repcode_load_data.R" must be loaded prior to running "GSC_repcode_maintext_results.R".

-scripts for replication by Table/Figure of all empirical results
Table 2: GSC_repcode_data_summary.R
Table 3: GSC_repcode_maintext_results.R
Table 4: '                            ' 
Table 5: '                            '
Table 6: '                            '
Table 7: '                            '
Table A2: GSC_repcode_online_appendix_results.R
Table A3: '				      '
Table A4: '				      '
Table A5: '				      '
Table A6: '				      '
Table A7: '				      '
Table A8: '				      '
Table A9: '				      '
Table A10: '				      '
Table A11: '				      '
Table A12: '				      '
Table A13: '				      '
Table A14: '				      '
Table A15: '				      '
Table A16: '				      '


3. The version of R that was used to run these scripts is: 
R version 3.0.3 (2014-03-06)
Platform: x86_64-w64-mingw32/x64 (64-bit)


4. Additional dependencies:
other attached packages:
 [1] lme4_1.0-4           Matrix_1.1-2         lmtest_0.9-32        stargazer_5.1        pscl_1.04.4          vcd_1.3-1            gam_1.09            
 [8] coda_0.16-1          lattice_0.20-27      mvtnorm_0.9-9996     xtable_1.7-1         formula.tools_1.3.1  operator.tools_1.3.0 ZeligChoice_0.8-1   
[15] VGAM_0.9-2           Zelig_4.2-1          sandwich_2.2-10      zoo_1.7-10           boot_1.3-9           epicalc_2.15.1.0     nnet_7.3-7          
[22] MASS_7.3-29          survival_2.37-7      foreign_0.8-63       gridExtra_0.9.1      ggplot2_1.0.0        plyr_1.8
    
       
5. Seed locations

-"GSC_repcode_data_summary.R"
  --set.seed(12345) at beginning of script
-"GSC_repcode_maintext_results.R"
  --set.seed(12345) at beginning of script
-"GSC_repcode_online_appendix_results.R"
  --set.seed(12345) at beginning of script

 
