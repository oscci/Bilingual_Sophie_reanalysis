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
  resultsfile <- paste0(rootdir,"/Miho_Results_L",l,".csv") # File lists analysis results

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

myse <- alldata$L1.mean_se
myN <- alldata$L1.N

if(l==2){
  myse <- alldata$L2.mean_se
  myN <- alldata$L2.N
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

if (l==1){alldata$L1.exclusions <- exclusions}
if (l==2){alldata$L2.exclusions <- exclusions}

########################################################
# Save resultsfile
write.csv(alldata, resultsfile, row.names=FALSE)
}

