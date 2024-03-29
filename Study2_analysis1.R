#########################################################################################
# Miho Script 1: Doppler Analysis of Miho's Japanese bilingualism data
########################################
#################################################
# This script reads in data from two languages (L1 and L2)  and calcuates LI values
# for phonological and semantic word generation tasks

########################################################
# Install packages. If you get an error here you may need to install the required packages first

require(readxl) # xlsx did not work on Macs - you may need to install java first. See java.com 
require(tidyverse)
require(osfr)
require(writexl)

########################################################
# Can edit these lines to automatically download data from OSF once the project is made public
# NB: it will not overwrite existing files

#osf_retrieve_file("https://osf.io/jt2ca") %>% osf_download(conflicts = "skip") # Edit the location of the new OSF project
#unzip ("A2_Oxford_fTCD_Data.zip", exdir = ".", overwrite = FALSE) # Edit the name for the .zip file

########################################################
# Specify directory and other variable parameters
dir <- getwd() # Set data directory path here
rawmeansdir<- "Study2_Grand_Means"
if(!file.exists(rawmeansdir)){dir.create(rawmeansdir)}
plotdir <- "Study2_LI_Plots"
if(!file.exists(plotdir)){dir.create(plotdir)}

checkmarkers=0; #set to 1 to see a sample of the file to check markers are there
initialdatacheck=0; #set to 1 toview raw data for each epoch
initialdatacheck1=0; # set to 1 to view epochs after normalisation
initialdatacheck2=0; #set to 1 to view epochs after heartbeat Correction
initialdatacheck3=0; # set to 1 to visualise after baseline correction
initialdatacheck4=0; # set to 1 to plot AND SAVE average for each subject

# Timings in secs
premarker=-11 # epoch start
basestart=-10 # baseline start
baseend=0 # baseline end
poistart=6 # period of interest start
poiend=20 # period of interest end
postmarker=25 # end of epoch 

extremehi=140 # define values for rejecting bad epochs 
extremelo=60 # (% above/below mean of 100 in normed/corrected data)
samplingrate=25
heartratemax = 125  # Maximum heartrate that would be expected, in bpm
peakdiffmin = 60/heartratemax * samplingrate # The minumum number of samples expected between heartbeats, based on heartratemax
baselinecorrect=1 # correct for baseline
interpolatebad=2;#set to 2 to replace brief dropout/spiking with mean value for that channel
#number specified here is max number of bad datapoints corrected for

prepoints=premarker*samplingrate # these define the data point marking parts of the epoch
basestartpoint=basestart*samplingrate
baseendpoint=baseend*samplingrate
postpoints=postmarker*samplingrate
poistartpoints=poistart*samplingrate
poiendpoints=poiend*samplingrate

trialsperrun=20


########################################################
# Specify subjects

# Read in Triallist to get list of subject IDs
# Can edit this line to download the data from OSF once the project is public
# osf_retrieve_file("https://osf.io/pvb57") %>%  osf_download(conflicts = "skip") # Edit the file location
trialloc<-"Study2_filelist.xlsx" # File lists all subjects and trial inclusions/exclusions

triallist_L1_Phon <- read_excel(trialloc, sheet=1)
triallist_L2_Phon <- read_excel(trialloc, sheet=2)
triallist_L1_Sem <- read_excel(trialloc, sheet=3)
triallist_L2_Sem <- read_excel(trialloc, sheet=4)

# List all of the subjects here. 
all_subjects <- triallist_L1_Phon$ID
nsubj <- length(all_subjects)

# Create matrices for results
resultsloc1 <- "Study2_resultsL1.csv" # File name for Results from session 1
resultsloc2 <- "Study2_resultsL2.csv" # File name for Results from session 2
# Can edit this line to download list of column names for results file directly from OSF
# osf_retrieve_file("https://osf.io/zuj6x") %>% osf_download(conflicts = "skip") # Edit file location
mycolumns<- read.csv("Study2_colnames.txt", header=FALSE)
results2 <- results1 <- as.data.frame(matrix(data=NA, nrow=nsubj, ncol=35))
colnames(results1) <- mycolumns[c(1:18,36:52), ]
colnames(results2) <- mycolumns[c(1,19:35,53:69), ]

results2$ID <- results1$ID <- all_subjects

# If results files already exist, load them from the workspace
if (file.exists(resultsloc1)){
  results1 <- read.csv(resultsloc1)
}
if (file.exists(resultsloc2)){
  results2 <- read.csv(resultsloc2)
}

# Sorry, this is ugly coding...
results1$L1_Phon.Comment <- as.character(results1$L1_Phon.Comment)
results2$L2_Phon.Comment <- as.character(results2$L2_Phon.Comment)
results1$L1_Phon.peak_laterality <- as.character(results1$L1_Phon.peak_laterality)
results1$L1_Phon.mean_laterality <- as.character(results1$L1_Phon.mean_laterality)
results2$L2_Phon.peak_laterality <- as.character(results2$L2_Phon.peak_laterality)
results2$L2_Phon.mean_laterality <- as.character(results2$L2_Phon.mean_laterality)
results1$L1_Sem.Comment <- as.character(results1$L1_Sem.Comment)
results2$L2_Sem.Comment <- as.character(results2$L2_Sem.Comment)
results1$L1_Sem.peak_laterality <- as.character(results1$L1_Sem.peak_laterality)
results1$L1_Sem.mean_laterality <- as.character(results1$L1_Sem.mean_laterality)
results2$L2_Sem.peak_laterality <- as.character(results2$L2_Sem.peak_laterality)
results2$L2_Sem.mean_laterality <- as.character(results2$L2_Sem.mean_laterality)

for (mysub in 1:length(all_subjects)){ # If you want to analyse ALL subjects you can change this to (mysub in 1:length(all_subjects)){    
  
  mysubname <- all_subjects[mysub]
  cat(paste0("BL",mysubname), "\n\n")
  
  # Read exp data
  dataloc <- paste0("Study2_data/BL", mysubname,".exp")
  
  dat<-read.table(dataloc, skip = 6,  header =FALSE, sep ="\t")
  # downsample to 25 Hz by taking every 4th point
  shortdat = filter(dat, row_number() %% 4 == 0) # downsample to 25 Hz by taking every 4th point
  allpts = nrow(shortdat) # total N points in long file
  shortdat[,1] = (seq(from=1,to=allpts*4,by=4)-1)/100 #create 1st column which is time in seconds from start
  
  # Loop through languages: L1=Japanese, L2=English
  for (language in 1:2){
    
    ########################################################
    # Loop through tasks: 1 = Phon, 2 = Sem
    tasks <- c('Phon', 'Sem')
    
    for (task in 1:2){
      # Select relevant triallist
      if (language == 1){
        triallist <- triallist_L1_Phon
        if (task == 2){
          triallist <- triallist_L1_Sem
        }
      }
      if (language == 2){
        triallist <- triallist_L2_Phon
        if (task == 2){
          triallist <- triallist_L2_Sem
        }
      }
    triallist$ID<-as.character(triallist$ID) # unfactor this column to avoid later difficulties
    
    # Initiate a comment - this will be added to the results file at the end of the analysis
    mycomment <- NA
    
    # select the columns we need for the analysis. Columns are for sec, L, R, marker
    if (language==1){ 
      wantcols = c(1,3,4,13) # L1_Phon = markers in column 13
      if (task==2){
        wantcols = c(1,3,4,14) # L1_Sem = markers in column 14
        }
      }
    if (language==2){ 
      wantcols = c(1,3,4,11) # L2_Phon = markers in column 11
      if (task==2){
        wantcols = c(1,3,4,12) # L2_Sem = markers in column 12
        }
      }
    
    rawdata = data.frame(shortdat[,wantcols])
    colnames(rawdata) = c ("sec","L","R","marker")
    
    #-------------------------------------------------------
    # Brief marker channel to check all OK
    #-------------------------------------------------------
    if (checkmarkers==1)
    {
      plot(rawdata$marker, type="l", col='red')
      cat("Press [enter] to continue")
      line <- readline()
    }

    #-----------------------------------------------------------
    #Now find markers; place where "marker" column goes from low to high value
    #-----------------------------------------------------------
    mylen = nrow(rawdata); # Number of timepoints in filtered data (rawdata)
    markerplus = c(0 ,rawdata$marker); # create vectors with offset of one
    markerchan = c(rawdata$marker,0); 
    markersub = markerchan - markerplus; # start of marker indicated by large difference between consecutive data points
    meanmarker <- mean(rawdata$marker) # We will identify big changes in marker value that are > 5 sds
    markersize <- meanmarker+3*sd(rawdata$marker)
    
    origmarkerlist = which(markersub>markersize)
    norigmarkers = length(origmarkerlist)
    
    if (origmarkerlist[1] == 1){ # Sometimes the trigger channel may have been 'up' at the start of the experiment. 
      
      # In this case, recalculate the time for trigger 1
      myaddcomment<-paste("Trigger channel started in 'up' position.")
      mycomment<-paste(mycomment,myaddcomment)
      # Find offset of first trigger
      markeroff = which(markersub < -markersize)
      # Triggers last 0.25 seconds
      origmarkerlist[1] <- markeroff[1] - (floor(0.25 * samplingrate))
      
      # BUT if there are more than 20 markers, this first one should be ignored
      if (norigmarkers > trialsperrun){
        origmarkerlist <- origmarkerlist[-1]
        norigmarkers <- length(origmarkerlist)
      }
    }
    
    if (origmarkerlist[norigmarkers] > (mylen-postpoints))
    {myaddcomment<-paste(". Short last epoch in run.")
    mycomment<-paste(mycomment,myaddcomment)
    } # indicates if there is a short last epoch; this may need disposing of
    
    excessmarkers=norigmarkers-trialsperrun
    # indicates if there are excess markers; hopefully these are practice trials. 
    # If the quantity does not indicate this, a comment is made in the "Dispose of practice trials" 
    # section below. Also, check there aren"t fewer than expected, and comment on this
    if (excessmarkers<0)
    {mycomment<-paste(mycomment,". Fewer markers than expected.")
    }
    
    # Check that markers are at least 30 s apart
    intervals=c(rawdata$sec[origmarkerlist],10000)-c(0,rawdata$sec[origmarkerlist])
    intervals=intervals[2:(length(intervals)-1)]
    # Ignore first and last values since these are arbitrary; other intervals should be around 30s 
    # but may be longer if recording interrupted. Shorter intervals indicate there have been spurious 
    # markers which will need dealing with
    if(min(intervals<30))
    {myaddcomment<-paste(". Possible spurious markers.")
    mycomment<-paste(mycomment,myaddcomment)
    }
    
    #---------------------------------------------------------
    # Look for practice trials, and dispose of them
    # Also identify unexpected extra markers; at present the
    # script is set to drop any excess markers from beginning of file 
    #---------------------------------------------------------
    if(excessmarkers>0)
    {
        myaddcomment<-paste(excessmarkers,". Unexpected markers found in run 1, investigate.")
        mycomment<-paste(mycomment,myaddcomment)
        markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))] #If unexpected extra markers
        # found, drop earlier ones until maximum possible markers are retained; then continue
    } else {
      markerlist<-origmarkerlist
    }
    
    #---------------------------------------------------------
    # Make vector indicating trials to be be included/excluded
    # based on behaviour, and drop markers for trials not completed
    #---------------------------------------------------------
    myinclude = triallist[mysub,2:(trialsperrun+1)]
    myinclude = as.numeric(myinclude)

    nmarkers = length(markerlist)
    
    #-----------------------------------------------------------
    # identify extreme values; can also check each epoch visually
    #------------------------------------------------------------
    #droprej and spikerej give lower and upper limits of signal for L and R channels
    droprej = rep(0,2);
    spikerej = droprej # initialise spikerej and droprej with zero
    mymax = max(rawdata[,2:3])
    
    droprej[1] = quantile(rawdata$L,.0001)
    droprej[2] = quantile(rawdata$R,.0001)
    spikerej[1] = quantile(rawdata$L,.9999)
    spikerej[2] = quantile(rawdata$R,.9999)
    
    for(i in 1:2) # For left and right sensors
    {if(droprej[i]<1)
    {droprej[i]=1 #value cannot be 0 or less! Lowest droprej value is 1
    }
    }
    
    #----------------------------------------------------------
    # epoch the accepted trials into an array
    # This has 4 dimensions; trials,points, L/R, raw/normalised/heartcorr/baselined
    #-----------------------------------------------------------
    # myepoched will be the full epoched trial
    myepoched <- array(0, dim=c(nmarkers,postpoints-prepoints+1,2,4)) # Set up an empty matrix
    # mybit will be the bit within the POI
    mybit = matrix(data = NA, nrow = poiendpoints-basestartpoint, ncol = 2)
    
    for(mym in 1:nmarkers) # for trials
    { 
      index1=markerlist[mym]+prepoints # index1 is index of the timepoint at the start of the epoch
      index2=markerlist[mym]+postpoints # index2 is the index of the timepoint at the end of the epoch
      
      # If recording started late, the start of the epoch for trial 1 will be beyond the recorded range. 
      # If this doesn"t affect the baseline period (ie, results will be unaffected), then replace with mean
      if (index1 < 0 & markerlist[mym]+basestartpoint > 0){
        cat("Recording started late. Padding start with zeros", "\n")
        replacement_mean_left = mean(rawdata[0:index2,2]) # Left hemisphere mean
        replacement_mean_right = mean(rawdata[0:index2,3]) # Right hemisphere mean
        myepoched[mym, ,1,1] = c(rep(replacement_mean_left,index1*-1+1),rawdata[0:index2,2])
        myepoched[mym, ,2,1] = c(rep(replacement_mean_right,index1*-1+1),rawdata[0:index2,3])
      }
      
      if (index1 > 1){
      myepoched[mym,,1,1]=rawdata[index1:index2,2] #L side
      myepoched[mym,,2,1]=rawdata[index1:index2,3] #R side
      }
      
      # Looks for data points lower than droprej or higher than spikerej. Only look between beginning of baseline and end of POI
      for(i in 1:2) # for left and right sides
      {rejpoints <- numeric(0) #coerces vector rejpoints to zero length between iterations 
      mybit[,i]=myepoched[mym, c((basestartpoint-prepoints):(poiendpoints-prepoints-1)), i, 1]
      thisbit = mybit[,i]
      
      # rejpoints is a list of points where signal indicates dropout or spiking
      rejpoints=c(rejpoints, which(thisbit < droprej[i]) + basestartpoint - prepoints -1); # identifies the indices of myepoched which will be marked as bad
      rejpoints=c(rejpoints, which(thisbit > spikerej[i]) + basestartpoint - prepoints -1);
      rejpoints=c(rejpoints, which(is.na(thisbit))) #triggered if epoch too short
      
      # if there are more than 2 rejected points, the whole epoch is marked as bad
      if(length(rejpoints)>interpolatebad)
      {myinclude[mym]=-1; #flag with -1; denotes drop this epoch; triggered by either channel
      }
      # if there are two or less (but more than zero)
      if(length(rejpoints)<=interpolatebad & length(rejpoints) > 0){
        for (p in 1:length(rejpoints)){
          myepoched[mym, rejpoints[p],i,1] = mean(myepoched[mym, c((basestartpoint-prepoints):(poiendpoints-prepoints-1)),i,1])}
        
      } # End of loop through rejpoints
      
      } # End of left / right loop
      
      if(mym==1)
      {badpoints=rejpoints
      }
      if(mym>1)
      {badpoints=c(badpoints,rejpoints)# keeps record of points with dropout/spiking between iterations
      }
      #-------------------------------------------
      # See epoch-by-epoch plots of raw data
      #-------------------------------------------
      
      # X axis will be time of full epoch
      timeline = rawdata$sec[1:(postpoints-prepoints+1)] #timeline used in all plots
      
      if(initialdatacheck==1) #set initialdatacheck to zero to avoid plotting
      {  myplotbit <- myepoched[mym, , ,1]
        #first plot the old values with no correction
        myylim <- range(c(range(na.omit(myplotbit[,1])),range(na.omit(myplotbit[,2]))))
        
        plot(timeline+premarker,myplotbit[,1],type="n",xlab="time (secs)",ylab="velocity",ylim=myylim)
        lines(timeline+premarker,myplotbit[,1],col="red")
        lines(timeline+premarker,myplotbit[,2],col="blue")
        
        #then overplot the corrected values in different colours
        lines(timeline+premarker,myepoched[mym,,1,1],col="pink")
        lines(timeline+premarker,myepoched[mym,,2,1],col="lightblue")
        abline(v=basestart)
        abline(v=baseend)
        abline(v=poistart)
        abline(v=poiend)
        
        mytitle=paste(mysubname, "Trial:", mym,"Include = ",myinclude[mym])
        title(mytitle)
        location1<-range(mybit)[2]-20
        location2<-range(mybit)[2]-80
        text(0,location1,"Red/blue values in POI have been overwritten with mean",cex=.7)
        text(0,location2,"1 = included; 0 = pre-excluded, -1 = rejected",cex=.7);
        cat("Press 9 for manual exclusion. Press 8 to retain excluded (-1). To retain current inclusion/exclusion status, press 1")
        myoverride <- as.integer(readline(prompt = ""))
        
        # These if statements process the user"s manual responses 
        if(is.na(myoverride)) # If the user presses enter but fails to press a number,
        {myoverride=1  # myoverride is coded as "1"to prevent crashing.
        }              # This means exclusion/inclusion is retained according to the automated system. 
        if(myoverride>1)
        {if(myoverride==9)
        {myinclude[mym]=-1}
          if(myoverride==8)
          {myinclude[mym]=1
          }
          myaddcomment<-paste(". Manual override",myoverride,"trial",mym)
          mycomment<-paste(mycomment,myaddcomment)}
      }
      
    } #next epoch
    
    triallist[mysub,2:(trialsperrun+1)] <- as.list(myinclude)
    
    #--------------------------------------------------------
    # Remove deleted epochs (originals in origdata; 
    # myepoched updated so only has retained epochs)
    #--------------------------------------------------------
    nspikes=length(badpoints) # add number of spiking/dropout points to table for saving
   
    keepmarkers=which(myinclude==1)
    origdata=myepoched #keep this so can reconstruct
    myepoched=myepoched[keepmarkers,,,] #file with only accepted epochs
    nmarkers2=length(keepmarkers)
    
    #---------------------------------------------------------
    # Normalise to mean of 100 (see Deppe et al, 2004)
    # Multiply by 100 and divide by overall mean value
    # ensures results are independent of angle of insonation
    #----------------------------------------------------------
    meanL=mean(myepoched[,,1,1], na.rm="TRUE")
    meanR=mean(myepoched[,,2,1], na.rm="TRUE")
    myepoched[,,1,2]=(100*myepoched[,,1,1])/meanL #last dim of myepoched is 2 for the normalised data
    myepoched[,,2,2]=(100*myepoched[,,2,1])/meanR
    
    # Short final epochs have NAs in them. Replace with zero to avoid errors.
    for (x in 1:length(myepoched[nmarkers2,,1,2])){
      
      if (is.na(myepoched[nmarkers2,x,1,2])){
        cat("Recording ended early. Replacing NAs with zeros", "\n")
        myepoched[nmarkers2,x,1,2] = 0 # Left hemisphere
        myepoched[nmarkers2,x,2,2] = 0 # Right hemisphere
      }
    }
    
    #---------------------------------------------------------
    # See plots of normed epochs
    #---------------------------------------------------------
    if(initialdatacheck1==1)
    {for(mym in 1:nmarkers2)
    {plot(timeline+premarker,myepoched[mym,,1,2],type="n",xlab="time (secs)",ylab="velocity")
      lines(timeline+premarker,myepoched[mym,,1,2],col="pink")
      lines(timeline+premarker,myepoched[mym,,2,2],col="lightblue")
      title("After normalization")
      cat("Press [enter] to continue")
      line <- readline()
    }
    }
    
    #----------------------------------------------------------
    # Find heart beat markers and put corrected values in col 3
    # of 4th dimension of myepoched
    #----------------------------------------------------------
    #Find peaks with moving window, looking for troughs in heartbeat
    
    for(mym in 1:nmarkers2)
    {peaklist=numeric(0)
    pdiff=numeric(0)
    badp=numeric(0)
    thisbit=myepoched[mym,,1,2]
    mypts=length(na.omit(thisbit))
    
    for(i in seq(6,mypts-6,2))
    {if(
      (thisbit[i] > thisbit[i-5])      # Check that ith value is greater than the value 5 back
      && (thisbit[i-1] > thisbit[i-5]) # Check that the previous value is greater than the value 5 back
      && (thisbit[i] > thisbit[i+5])   # Check that the ith value is greater than the value 5 ahead
      && (thisbit[i+1]>thisbit[i+5]))  # Check that the next value is greater than the value 5 ahead
    {peaklist=c(peaklist,i)
    }
    }
    
    # Check that the heartbeats are spaced by far enough!
    pdiff <- peaklist[2:length(peaklist)]-peaklist[1:(length(peaklist)-1)] # pdiff is a list of the number of samples between peaks
    badp<-which(pdiff<peakdiffmin) # badp is a list of the pdiff values that are less than peakdiffmin
    if (length(badp) != 0)
    {peaklist<-peaklist[-(badp+1)] # update peaklist, removing peaks identified by badp
    }
    peaklist=c(1,peaklist,mypts) #top and tail the list 
    
    peakn=length(peaklist)
    for (p in 1:(peakn-1))
    {myrange=seq(peaklist[p],peaklist[p+1]) # the indices where the heartbeat will be replaced
    thisheart1=mean(myepoched[mym,myrange,1,2]) # the new values that will be replaced
    thisheart2=mean(myepoched[mym,myrange,2,2])
    myepoched[mym,myrange,1,3]=thisheart1
    myepoched[mym,myrange,2,3]=thisheart2
    }
    }
    
    #----------------------------------------------------------
    # See plot after heartbeat correction
    #----------------------------------------------------------
    if (initialdatacheck2==1)
    {for(mym in 1:nmarkers2 )
    {plot(timeline+premarker,myepoched[mym,,1,3],type="n",xlab="time (secs)",ylab="velocity")
      lines(timeline+premarker,myepoched[mym,,1,3],col="pink")
      lines(timeline+premarker,myepoched[mym,,2,3],col="lightblue")
      mytitle=paste("Trial after heart beat correction, trial =", mym)
      title(mytitle)
      cat ("Press [enter] to continue")
      line <- readline()
    }
    }
    
    #----------------------------------------------------------
    # Find mean for baseline and subtract this.
    # This amounts to baseline correction...
    #----------------------------------------------------------
    nepochbase=nmarkers2
    basepoints=(basestartpoint-prepoints):(baseendpoint-prepoints) #all baseline points within epoch
    if (baselinecorrect==1)
    {for (mym in 1:nmarkers2)
    {basemeanL=mean(myepoched[mym,basepoints,1,3]) #last dim is 3, which is HB corrected
    basemeanR=mean(myepoched[mym,basepoints,2,3])
    myepoched[mym,,1,4]=100+myepoched[mym,,1,3]-basemeanL #last dim 4 is HB and baseline
    myepoched[mym,,2,4]=100+myepoched[mym,,2,3]-basemeanR
    }
    }
    
    #--------------------------------------------------------
    # Plot after HB correction and baseline correction
    #--------------------------------------------------------
    if(initialdatacheck3==1)
    {for(mym in 1:nmarkers2 )
    {plot(timeline+premarker,myepoched[mym,,1,4],type="n",xlab="time (secs)",ylab="velocity")
      lines(timeline+premarker,myepoched[mym,,1,4],col="red")
      lines(timeline+premarker,myepoched[mym,,2,4],col="blue")
      mytitle=paste("Trial after baseline correction, trial =",mym)
      title(mytitle)
      text(-5,110,"blue=R\n red=L\n",cex=.75)
      cat ("Press [enter] to continue")
      line <- readline()
    }
    }
    
    #--------------------------------------------------------
    # Find and exclude epochs with extreme values in period 
    # between start of baseline and end of POI
    #---------------------------------------------------------
    keepepoch=rep(1,nmarkers2) #initialise for inclusions
    for(mym in 1:nmarkers2)
    {extremerange=c(which(myepoched[mym,1:(poiendpoints-basestartpoint),1:2,4]>extremehi),which(myepoched[mym,1:(poiendpoints-basestartpoint),1:2,4]<extremelo))
    if(length(extremerange)>0 )
    {keepepoch[mym]=0
    }
    if(mym==1)
    {allextreme=extremerange
    }
    if(mym>1)
    {allextreme=c(allextreme,extremerange) #keeps record of extreme values across trials
    }
    }
    acceptableepochs=which(keepepoch==1)
    
    nextreme=length(allextreme) #report number of extreme values across trials
    
    #--------------------------------------------------------
    # Get grand average
    #--------------------------------------------------------
    finalepochs=myepoched[acceptableepochs,,,4]
    myN=dim(finalepochs)[1] #initialise vectors for laterality stats
    
    # Average over all included trials
    Lmean <- apply(finalepochs[,,1], c(2), mean)
    Rmean <- apply(finalepochs[,,2], c(2), mean)
    LRdiff=Lmean-Rmean
    
    # Specify period of interest (POI)
    baseoffset=-premarker*samplingrate
    rangestart=baseoffset+poistartpoints
    rangeend=baseoffset+poiendpoints

    #--------------------------------------------------------
    # Calculate peak LI stats
    #--------------------------------------------------------    
    # Identify peak 
    mymax=max(LRdiff[rangestart:rangeend])
    mymin=min(LRdiff[rangestart:rangeend])
    myside=1
    mylatpeak=mymax
    if(-mymin>mymax)
    {myside=-1 #R biased LI
    mylatpeak=mymin
    } #R peak > L peak
    
    mytimepeak=first(which(LRdiff==mylatpeak))
    peak_latency=(mytimepeak-baseoffset)/samplingrate #need to subtract points for baseline
    mypeakrange=seq(mytimepeak-25,mytimepeak+25) #actual points ie includes baseline
    
    # Calculate peak LI
    peak_LI=round(mean(LRdiff[mypeakrange]),3)
    
    # Extract trial-by-trial data for peak SE
    indLI=numeric(0) #initialise null vector
    indLI_mean=numeric(0)
    for (m in 1:myN)
    {indLI=c(indLI,mean(finalepochs[m,mypeakrange,1]-finalepochs[m,mypeakrange,2]))
    inddiff <- finalepochs[m,rangestart:rangeend,1] - finalepochs[m,rangestart:rangeend,2]
    indLI_mean =c(indLI_mean,mean(inddiff)) ## This is now correct!!!
    }
    peak_sd <- sd(indLI)
    peak_se <- round(peak_sd/sqrt(myN), 3)
    
    # Calculate whether laterality is significantly left, right or neither (bilateral)
    latdir=c("R","bilat","L")
    lowCI=as.numeric(format(peak_LI-myside*peak_se*1.96,digits=3))
    lateralised=myside
    if((myside*lowCI)<0) {lateralised=0}
    peak_laterality=latdir[lateralised+2]
    
    # Calculate peak LI from odd or even trials only
    odds<-seq(from=1,to=myN,by=2)
    evens<-seq(from=2,to=myN,by=2)
    Lmeanodd<-apply(finalepochs[odds,,1],c(2),mean)
    Lmeaneven<-apply(finalepochs[evens,,1],c(2),mean)
    Rmeanodd<-apply(finalepochs[odds,,2],c(2),mean)
    Rmeaneven<-apply(finalepochs[evens,,2],c(2),mean)
    LRdiffodd<-Lmeanodd-Rmeanodd
    LRdiffeven<-Lmeaneven-Rmeaneven
    peak_odd <- round(mean(LRdiffodd[mypeakrange]), 3)
    peak_even <- round(mean(LRdiffeven[mypeakrange]), 3) #NB LI for even and odd computed at same peak as full LI
    
    #--------------------------------------------------------
    # Calculate mean LI stats
    #--------------------------------------------------------  
    # Calculate mean LI
    mean_LI <- round(mean(LRdiff[rangestart:rangeend]), 3)
    
    # SE for mean LI
    mean_sd <- sd(indLI_mean)
    mean_se <- round(mean_sd/sqrt(myN), 3)
    
    # Calculate whether laterality is signficantly left, right or neither (bilateral)
    myside=1
    if(mean_LI < 0)
    {myside=-1} #R biased LI
    lowCI=as.numeric(format(mean_LI-myside*mean_se*1.96,digits=3))
    lateralised=myside
    if((myside*lowCI)<0) {lateralised=0}
    mean_laterality=latdir[lateralised+2]
    
    # Calculate mean LI from odd or even trials only
    mean_odd=round(mean(LRdiffodd[rangestart:rangeend]), 3)
    mean_even=round(mean(LRdiffeven[rangestart:rangeend]), 3)
    

    #----------------------------------------------------------
    #Plot and save overall  laterality curve
    #----------------------------------------------------------
    timelinelong=rawdata$sec[1:(postmarker*25-prepoints+1)]+premarker
    
    if (initialdatacheck4==1)
    {
    plot(timelinelong,Lmean, type="n",ylab="mean blood flow",xlab="time(s)",ylim=c(90,120)) #set up plot - doesn"t actually plot anything
    lines(timelinelong,Lmean,col="red")
    lines(timelinelong,Rmean,col="blue")
    lines(timelinelong,(100+LRdiff),col="black")
    abline(v = poistart, lty = 2, col = "green")
    abline(v = poiend, lty = 2, col = "green")
    abline(v = basestart, lty = 2)
    abline(v = baseend, lty = 2)
    text(-4,110,"blue=R\n red=L\n black=(L-R) +100",cex=.75)
    mytitle=paste("BL",mysubname," L", language, "_", tasks[task], sep = "")
    title(mytitle)

    cat ("Press [enter] to continue")
    line <- readline()
    
    png(filename=paste0(plotdir,"/LI_Plot_","BL",mysubname,"_L", language, "_", tasks[task],".png"))

    plot(timelinelong,Lmean, type="n",ylab="mean blood flow",xlab="time(s)",ylim=c(90,120)) #set up plot - doesn"t actually plot anything
    lines(timelinelong,Lmean,col="red")
    lines(timelinelong,Rmean,col="blue")
    lines(timelinelong,(100+LRdiff),col="black")
    abline(v = poistart, lty = 2, col = "green")
    abline(v = poiend, lty = 2, col = "green")
    abline(v = basestart, lty = 2)
    abline(v = baseend, lty = 2)
    text(-4,105,"blue=R\n red=L\n black=(L-R) +100",cex=.75)
    mytitle=paste("BL",mysubname," L", language, "_", tasks[task], sep = "")
    title(mytitle)

    dev.off()
    }

    #------------------------------------------------------
    # Writes data to file.
    #------------------------------------------------------
    
    savedata <- c(norigmarkers, nspikes, nextreme, myN,
                  peak_LI, peak_latency, peak_se, peak_laterality, peak_odd, peak_even,
                  mean_LI, mean_se, mean_laterality, mean_odd, mean_even)
    
    if (language == 1){
      if (task == 1){
        results1[mysub,2]      <- mycomment
        results1[mysub,3:17]   <-  savedata
        triallist_L1_Phon[mysub,2:(trialsperrun+1)] <- triallist[mysub,2:(trialsperrun+1)]
      }
      if (task ==2){
        results1[mysub,19]      <- mycomment
        results1[mysub,20:34]   <-  savedata
        triallist_L1_Sem[mysub,2:(trialsperrun+1)] <- triallist[mysub,2:(trialsperrun+1)]
      }
    }
    if (language == 2){
      if (task == 1){
        results2[mysub,2]      <- mycomment
        results2[mysub,3:17]   <-  savedata
        triallist_L2_Phon[mysub,2:(trialsperrun+1)] <- triallist[mysub,2:(trialsperrun+1)]
      }
      if (task ==2){
        results2[mysub,19]      <- mycomment
        results2[mysub,20:34]   <-  savedata
        triallist_L2_Sem[mysub,2:(trialsperrun+1)] <- triallist[mysub,2:(trialsperrun+1)]
      }
    }
    
    # Print averaged epoch data to file
    averageddata <- data.frame("Sent_L" = Lmean,
                               "Sent_R" = Rmean)
    
    if(length(keepmarkers)>=8){
      mymeanLR<-data.frame(matrix(ncol=5,nrow=mypts)) 
      mymeanLR[,1]<-as.integer(mysub)
      alltime<-seq(from=premarker, to=postmarker, by=.04)
      mymeanLR[,2]<-alltime
      mymeanLR[,3]<-Lmean
      mymeanLR[,4]<-Rmean
      mymeanLR[,5]<-Lmean-Rmean
      colnames(mymeanLR)<-c("ID", "time", "Lmean", "Rmean", "meanDiff")

      csvFile2<-paste(rawmeansdir, "/BL",mysubname,"_L", language, "_", tasks[task], ".csv",sep="")
      write.csv(mymeanLR, csvFile2, row.names=F)
    }
    } # End loop through tasks (Phon and Sem)

  } # End loop through L1 and L2
} # End loop through subjects

# Print results file
write.csv(results1, file = resultsloc1, row.names=F)
write.csv(results2, file = resultsloc2, row.names=F)

# Print triallist files
sheets <- list('L1_Phon' = triallist_L1_Phon, 'L2_Phon' = triallist_L2_Phon, 
               'L1_Sem' = triallist_L1_Sem, 'L2_Sem' = triallist_L2_Sem)
write_xlsx(sheets, 'Study2_filelist.xlsx')
  