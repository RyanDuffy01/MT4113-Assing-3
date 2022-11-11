set.seed(4596)

#creates simulated data 

##Inputs##

#NOS = a list of two integers that give Number Of Surveys for online and telephone (telephone first)
#SS =  a list of numbers (with telephone sample sizes first) of sample sizes of surveys
#TP =  a list of 4 probabilities (telephone probabilities listed first, and first probability of 2 is remain) 
#      being true probabilities a person is remain and leave for each category 

#Output##
#simulateddata = a 3 x length of SS dataframe with a column containing the type of study, 
# the sample sizes and the spread (remain proportions-leave proportions)

simulationfunction <- function(NOS,SS,TP){
  simulateddata <- data.frame(
    poll_type=c(rep("Telephone",NOS[1]),rep("Online",NOS[2])),
    samplesize=SS,
    spread=rep(0,NOS[1]+NOS[2])
  )
  for (i in 1:length(SS)){
    if (i <= NOS[1]){
      simmultinomdata <- rmultinom(1,as.integer(SS[i]),c(TP[1],TP[2],(1-TP[1]-TP[2])))
      remainprop <- simmultinomdata[1]/SS[i]
      leaveprop <- simmultinomdata[2]/SS[i]
      undecidedprop <- simmultinomdata[3]/SS[i]
      spreadprop <- remainprop-leaveprop
      simulateddata[i,3] <- spreadprop
    }else{
      simmultinomdata <- rmultinom(1,as.integer(SS[i]),c(TP[3],TP[4],(1-TP[3]-TP[4])))
      remainprop <- simmultinomdata[1]/SS[i]
      leaveprop <-simmultinomdata[2]/SS[i]
      undecidedprop <- simmultinomdata[3]/SS[i]
      spreadprop <- remainprop-leaveprop
      simulateddata[i,3] <- spreadprop
    }
  }
  return(simulateddata)
}



Averagesimulation <- simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.4,0.3,0.4,0.3))

par(mfrow=c(1,2))
hist(Averagesimulation$spread[1:55],xlab="Spread",main="Histogram of a General Simulation: Telephone Data")
hist(Averagesimulation$spread[55:(55+55)],xlab="Spread",main="Histogram of a General Simulation: Online Data")




#T-test
#performs a 2 sample students t test on the inputed data
ttestfunction <- function(data){
  Onlinedata <- data[which(data$poll_type=="Online"),]
  Telephonedata <- data[which(data$poll_type=="Telephone"),]
  meanOnlinespread <- mean(Onlinedata$spread)
  meanTelephonespread <- mean(Telephonedata$spread)
  varianceOnlinespread <- sd(Onlinedata$spread)^2
  VarianceTelephonespread <- sd(Telephonedata$spread)^2
  ntelephone <- length(Telephonedata$spread)
  nonline <- length(Onlinedata$spread)
  pooledvariance <- ((nonline-1)*varianceOnlinespread+(ntelephone-1)*VarianceTelephonespread)/(nonline+ntelephone-2)
  Teststat <- (meanTelephonespread-meanOnlinespread)/(pooledvariance*sqrt((1/nonline)+(1/ntelephone)))
  pvalue <- pt(Teststat,nonline+ntelephone-2)
  if (pvalue > 0.5){
    pvalue <- 1-pvalue
  }
  pvalue <- 2*pvalue
  if (pvalue <= 0.05){
    return("Reject H0")
  }
  if(pvalue > 0.05){
    return("Accept H0")
  }
}

ttestfunction(simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.4,0.3,0.4,0.3)))


#performs a randomization test on the inputed data
randomizationtestfunction <- function(data,noofrand=1000){
  Onlinedata <- data[which(data$poll_type=="Online"),]
  Telephonedata <- data[which(data$poll_type=="Telephone"),]
  allspreaddata <- data$spread
  ntelephone <- length(Telephonedata$spread)
  nonline <- length(Onlinedata$spread)
  nall <- length(allspreaddata)
  teststatorig <- mean(Telephonedata$spread)-mean(Onlinedata$spread)
  teststats <- rep(0,noofrand+1)
  for (i in 1:noofrand){
    randreorders <- sample(allspreaddata,nall,replace=F)
    randteststat <- mean(randreorders[1:ntelephone])-mean(randreorders[ntelephone+1:nonline])
    teststats[i] <- randteststat
  }
  teststats[noofrand+1] <- teststatorig
  pvalue <- length(teststats[teststats>=teststatorig])/length(teststats)
  if (pvalue>0.5){
    pvalue <- length(teststats[teststats<=teststatorig])/length(teststats)
  }
  pvalue <- 2*pvalue
  if (pvalue <= 0.05){
    return("Reject H0")
  }
  if(pvalue > 0.05){
    return("Accept H0")
  }
}






#The following code calculates the size and power of these tests by simulating data
#from different scenarios (listed below) 

#The first example below is commented and all others follow a similiar structure 

randomizations <- 70

#Large Sample Sizes 

#creates lists to store results of tests
resultsofttests_LargeSampleSizesTrue <- c()
resultsofttests_LargeSampleSizesFalse <- c()
resultsofrandomizationtests_LargeSampleSizesTrue <- c()
resultsofrandomizationtests_LargeSampleSizesFalse <- c()

for (i in 0:randomizations){
  #generates data where probabilites of remain and leave are the same  
  dataH0true <- simulationfunction(c(55,55),as.integer(rnorm(55+55,8000,100)),c(0.4,0.3,0.4,0.3))
  #generates data where probabilites of remain and leave are different 
  dataH0false <- simulationfunction(c(55,55),as.integer(rnorm(55+55,8000,100)),c(0.8,0.1,0.4,0.3))
  #runs the different tests on data and stores results
  resultsofttests_LargeSampleSizesTrue <- c(resultsofttests_LargeSampleSizesTrue,ttestfunction(dataH0true))
  resultsofttests_LargeSampleSizesFalse <- c(resultsofttests_LargeSampleSizesFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_LargeSampleSizesTrue <- c(resultsofrandomizationtests_LargeSampleSizesTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_LargeSampleSizesFalse<- c(resultsofrandomizationtests_LargeSampleSizesFalse,randomizationtestfunction(dataH0false))
}

#takes proportions of results of tests to find power and size for different tests
powerofttest_LargeSampleSizes <- length(which(resultsofttests_LargeSampleSizesFalse=="Reject H0"))/length(resultsofttests_LargeSampleSizesFalse)
sizeofttest_LargeSampleSizes <- length(which(resultsofttests_LargeSampleSizesTrue=="Reject H0"))/length(resultsofttests_LargeSampleSizesTrue)
powerofrandomizationtest_LargeSampleSizes <- length(which(resultsofrandomizationtests_LargeSampleSizesFalse=="Reject H0"))/length(resultsofrandomizationtests_LargeSampleSizesFalse)
sizeofrandomizationtest_LargeSampleSizes <- length(which(resultsofrandomizationtests_LargeSampleSizesTrue=="Reject H0"))/length(resultsofrandomizationtests_LargeSampleSizesTrue)


#Small Sample Sizes 
resultsofttests_SmallSampleSizesTrue <- c()
resultsofttests_SmallSampleSizesFalse <- c()
resultsofrandomizationtests_SmallSampleSizesTrue <- c()
resultsofrandomizationtests_SmallSampleSizesFalse <- c()

for (i in 0:randomizations){
  dataH0true <- simulationfunction(c(55,55),as.integer(rnorm(55+55,25,4)),c(0.4,0.3,0.4,0.3))
  dataH0false <- simulationfunction(c(55,55),as.integer(rnorm(55+55,25,4)),c(0.8,0.1,0.4,0.3))
  resultsofttests_SmallSampleSizesTrue <- c(resultsofttests_SmallSampleSizesTrue,ttestfunction(dataH0true))
  resultsofttests_SmallSampleSizesFalse <- c(resultsofttests_SmallSampleSizesFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_SmallSampleSizesTrue <- c(resultsofrandomizationtests_SmallSampleSizesTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_SmallSampleSizesFalse<- c(resultsofrandomizationtests_SmallSampleSizesFalse,randomizationtestfunction(dataH0false))
}

powerofttest_SmallSampleSizes <- length(which(resultsofttests_SmallSampleSizesFalse=="Reject H0"))/length(resultsofttests_SmallSampleSizesFalse)
sizeofttest_SmallSampleSizes <- length(which(resultsofttests_SmallSampleSizesTrue=="Reject H0"))/length(resultsofttests_SmallSampleSizesTrue)
powerofrandomizationtest_SmallSampleSizes <- length(which(resultsofrandomizationtests_SmallSampleSizesFalse=="Reject H0"))/length(resultsofrandomizationtests_SmallSampleSizesFalse)
sizeofrandomizationtest_SmallSampleSizes <- length(which(resultsofrandomizationtests_SmallSampleSizesTrue=="Reject H0"))/length(resultsofrandomizationtests_SmallSampleSizesTrue)



#Large Amount of Studies
resultsofttests_LargeStudiesTrue <- c()
resultsofttests_LargeStudiesFalse <- c()
resultsofrandomizationtests_LargeStudiesTrue <- c()
resultsofrandomizationtests_LargeStudiesFalse <- c()

for (i in 0:randomizations){
  dataH0true <- simulationfunction(c(70,70),as.integer(rnorm(70+70,400,100)),c(0.4,0.3,0.4,0.3))
  dataH0false <- simulationfunction(c(70,70),as.integer(rnorm(70+70,400,100)),c(0.8,0.1,0.4,0.3))
  resultsofttests_LargeStudiesTrue <- c(resultsofttests_LargeStudiesTrue,ttestfunction(dataH0true))
  resultsofttests_LargeStudiesFalse <- c(resultsofttests_LargeStudiesFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_LargeStudiesTrue <- c(resultsofrandomizationtests_LargeStudiesTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_LargeStudiesFalse<- c(resultsofrandomizationtests_LargeStudiesFalse,randomizationtestfunction(dataH0false))
}

powerofttest_LargeStudies <- length(which(resultsofttests_LargeStudiesFalse=="Reject H0"))/length(resultsofttests_LargeStudiesFalse)
sizeofttest_LargeStudies <- length(which(resultsofttests_LargeStudiesTrue=="Reject H0"))/length(resultsofttests_LargeStudiesTrue)
powerofrandomizationtest_LargeStudies <- length(which(resultsofrandomizationtests_LargeStudiesFalse=="Reject H0"))/length(resultsofrandomizationtests_LargeStudiesFalse)
sizeofrandomizationtest_LargeStudies <- length(which(resultsofrandomizationtests_LargeStudiesTrue=="Reject H0"))/length(resultsofrandomizationtests_LargeStudiesTrue)


#Small Amount of Studies
resultsofttests_SmallStudiesTrue <- c()
resultsofttests_SmallStudiesFalse <- c()
resultsofrandomizationtests_SmallStudiesTrue <- c()
resultsofrandomizationtests_SmallStudiesFalse <- c()

for (i in 0:randomizations){
  dataH0true <- simulationfunction(c(20,20),as.integer(rnorm(40,400,100)),c(0.4,0.3,0.4,0.3))
  dataH0false <- simulationfunction(c(20,20),as.integer(rnorm(40,400,100)),c(0.8,0.1,0.4,0.3))
  resultsofttests_SmallStudiesTrue <- c(resultsofttests_SmallStudiesTrue,ttestfunction(dataH0true))
  resultsofttests_SmallStudiesFalse <- c(resultsofttests_SmallStudiesFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_SmallStudiesTrue <- c(resultsofrandomizationtests_SmallStudiesTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_SmallStudiesFalse<- c(resultsofrandomizationtests_SmallStudiesFalse,randomizationtestfunction(dataH0false))
}

powerofttest_SmallStudies <- length(which(resultsofttests_SmallStudiesFalse=="Reject H0"))/length(resultsofttests_SmallStudiesFalse)
sizeofttest_SmallStudies <- length(which(resultsofttests_SmallStudiesTrue=="Reject H0"))/length(resultsofttests_SmallStudiesTrue)
powerofrandomizationtest_SmallStudies <- length(which(resultsofrandomizationtests_SmallStudiesFalse=="Reject H0"))/length(resultsofrandomizationtests_SmallStudiesFalse)
sizeofrandomizationtest_SmallStudies <- length(which(resultsofrandomizationtests_SmallStudiesTrue=="Reject H0"))/length(resultsofrandomizationtests_SmallStudiesTrue)


#Large Differences in Probability for Online and Telephone
resultsofttests_LargeProbTrue <- c()
resultsofttests_LargeProbFalse <- c()
resultsofrandomizationtests_LargeProbTrue <- c()
resultsofrandomizationtests_LargeProbFalse <- c()

for (i in 0:randomizations){
  dataH0true <- simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.4,0.3,0.4,0.3))
  dataH0false <- simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.8,0.1,0.1,0.8))
  resultsofttests_LargeProbTrue <- c(resultsofttests_LargeProbTrue,ttestfunction(dataH0true))
  resultsofttests_LargeProbFalse <- c(resultsofttests_LargeProbFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_LargeProbTrue <- c(resultsofrandomizationtests_LargeProbTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_LargeProbFalse<- c(resultsofrandomizationtests_LargeProbFalse,randomizationtestfunction(dataH0false))
}

powerofttest_LargeProb <- length(which(resultsofttests_LargeProbFalse=="Reject H0"))/length(resultsofttests_LargeProbFalse)
sizeofttest_LargeProb <- length(which(resultsofttests_LargeProbTrue=="Reject H0"))/length(resultsofttests_LargeProbTrue)
powerofrandomizationtest_LargeProb <- length(which(resultsofrandomizationtests_LargeProbFalse=="Reject H0"))/length(resultsofrandomizationtests_LargeProbFalse)
sizeofrandomizationtest_LargeProb <- length(which(resultsofrandomizationtests_LargeProbTrue=="Reject H0"))/length(resultsofrandomizationtests_LargeProbTrue)

#Small Differences in Probability for Online and Telephone
resultsofttests_SmallProbTrue <- c()
resultsofttests_SmallProbFalse <- c()
resultsofrandomizationtests_SmallProbTrue <- c()
resultsofrandomizationtests_SmallProbFalse <- c()

for (i in 0:randomizations){
  dataH0true <- simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.4,0.3,0.4,0.3))
  dataH0false <- simulationfunction(c(55,55),as.integer(rnorm(55+55,400,100)),c(0.45,0.3,0.3,0.45))
  resultsofttests_SmallProbTrue <- c(resultsofttests_SmallProbTrue,ttestfunction(dataH0true))
  resultsofttests_SmallProbFalse <- c(resultsofttests_SmallProbFalse,ttestfunction(dataH0false))
  resultsofrandomizationtests_SmallProbTrue <- c(resultsofrandomizationtests_SmallProbTrue,randomizationtestfunction(dataH0true))
  resultsofrandomizationtests_SmallProbFalse<- c(resultsofrandomizationtests_SmallProbFalse,randomizationtestfunction(dataH0false))
}

powerofttest_SmallProb <- length(which(resultsofttests_LargeProbFalse=="Reject H0"))/length(resultsofttests_LargeProbFalse)
sizeofttest_SmallProb <- length(which(resultsofttests_LargeProbTrue=="Reject H0"))/length(resultsofttests_LargeProbTrue)
powerofrandomizationtest_SmallProb <- length(which(resultsofrandomizationtests_SmallProbFalse=="Reject H0"))/length(resultsofrandomizationtests_SmallProbFalse)
sizeofrandomizationtest_SmallProb <- length(which(resultsofrandomizationtests_SmallProbTrue=="Reject H0"))/length(resultsofrandomizationtests_SmallProbTrue)

