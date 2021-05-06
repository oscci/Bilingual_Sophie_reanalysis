#########################################################################################
# Script 2: Outliers
#########################################################################################
# This script runs after Script 1, which calculates LI values for all participants.
# Script 2 then looks for outlier values, based on the standard error of the LI values.

########################################################
# Install packages

list.of.packages <- c("tidyverse","osfr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(tidyverse)
require(osfr)

########################################################
# Specify directory and other variable parameters
rootdir <- getwd()
mintrials <- 15 # Minimum trials is 15

########################################################
# Loop through languages, L1 and L2

for (l in 1:2){
  resultsfile <- paste0(rootdir,"Study2_results",l,".csv") # File lists analysis results
  
  ########################################################
  # Read in results file
  
  # Can edit these lines to read in results files from OSF
  
  # if (!file.exists(resultsfile)){
  #   if (task_switch == 1){
  #     osf_retrieve_file("https://osf.io/8msj5") %>% osf_download(conflicts = "skip") # WordGen_results.csv
  #   }
  #   if (task_switch == 2){
  #     osf_retrieve_file("https://osf.io/jh37g") %>% osf_download(conflicts = "skip") # PPTT_results.csv
  #   }
  # }
  
  alldata <- read.csv(resultsfile)
  
  nsub <- dim(alldata)[1]
  
  
  # Loop through tasks (1=Phon, 2=Sem)
  tasks <- c('Phon', 'Sem')
  for (t in 1:2){
    exclusions  <- rep(0,nsub)
    
    
    ########################################################
    # Outliers are defined based on the standard error across trials.
    # LI values more than 2.2 times the difference between the first and third quartiles (2.2 * (Q3-Q1)) 
    # above the third quartile values are classed as outliers
    # (e.g: upper limit = Q3 + 2.2*(Q3-Q1)). 
    
    ## We used to do this by combining data from ALL tasks, but now I'm not sure that's such a good idea, 
    ## as one task has more trials than the other
    # allse<-vector() # start with an empty vector
    # for (t in 1:6){
    #   allse<-c(allse,LI_data[,(t+14)])
    # }
    
    
    myse <- alldata$L1_Phon.mean_se
    myN <- alldata$L1_Phon.N
    if (t==2){
      myse <- alldata$L1_Sem.mean_se
      myN <- alldata$L1_Sem.N
    }
    
    if(l==2){
      myse <- alldata$L2_Phon.mean_se
      myN <- alldata$L2_Phon.N
      if (t==2){
        myse <- alldata$L2_Sem.mean_se
        myN <- alldata$L2_Sem.N
      }
    }
    
    lower_quartile <- quantile(myse, probs=0.25, na.rm="TRUE")
    upper_quartile <- quantile(myse, probs=0.75, na.rm="TRUE")
    quartile_diff <- upper_quartile - lower_quartile
    
    upper_limit <- upper_quartile + 2.2*quartile_diff
    
    for (p in 1:(nsub)){
      if (is.na(myse[p]) == 0       # If se is not NA
          & myse[p] > upper_limit)  # And se is greater than the upper limit
      {exclusions[p] <- 1}         # Then set exclusion status to 1
    }
    
    # Mark tasks with too few useable trials as excluded
    
    for (p in 1:nsub){
      if (myN[p] < mintrials){ #If nFinal is less than mintrials
        exclusions[p] <- 2  # Then set exclusion status to 2
      }
    }
    
    # Save exclusions to alldata
    if (l==1){
      if (t == 1){alldata$L1_Phon.exclusions <- exclusions}
      if (t == 2){alldata$L1_Sem.exclusions <- exclusions}
    }
    if (l==2){
      if (t == 1){alldata$L2_Phon.exclusions <- exclusions}
      if (t == 1){alldata$L2_Sem.exclusions <- exclusions}
    }
    
  } # End loop for tasks

########################################################
# Save resultsfile
write.csv(alldata, resultsfile, row.names=FALSE)
  
} # End loop for languages

