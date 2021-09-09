### SETUP
# search for and install all necessary packages
packages = c("dplyr", "data.table", "bookdown", "tinytex", "tidyr", "psych", "kableExtra", "ggplot2", "ggpubr", "corrplot", "yarrr", "apaTables", 
             "knitr", "car", "gridExtra", "pacman") #list of necessary packages
for (i in packages){
  if( !is.element(i, .packages(all.available=TRUE))) {
    install.packages(i, depencies=TRUE, repos = "http://cran.us.r-project.org")
    
  }
  library(i, character.only=TRUE)
}
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!require("papaja")) devtools::install_github("crsh/papaja")

library("wordcountaddin")
library("papaja")

### DATA IMPORT
#data imported via Qualtrics as .csv using numeric values, missing seen values recoded to -99
#read both datasets
data_p <- read.csv("data/DB2Data_Prolific.csv") #prolific dataset
data_s <- read.csv("data/DB2Data_Student.csv") #student dataset
TableStim <- read.csv("data/Estimation_Questions.csv") #stimuli
TablePerils <- read.csv("data/PerilsStimuli.csv") #perils stimuli

data_p <- data_p[-c(1,2),] #remove first two rows
data_s <- data_s[-c(1,2),] #remove first two rows
#rename columns
poldnames <- c("X1_Ambivalence1T1_8",
               "X2_Ambivalence1T1_8",
               "OMC_7")
pnewnames <- c("AC1",
               "AC2",
               "AC3")
setnames(data_p, poldnames, pnewnames, skip_absent=TRUE)


data_p$AC1 <- ifelse(data_p$F10==1,
                     7,
                     data_p$AC1)
data_p$AC2 <- ifelse(data_p$F10==1,
                     7,
                    data_p$AC2)
#student sample did not have attention checks - score as passed
#rename columns
poldnames <- c("X1_Ambivalence1T1_8",
               "X2_Ambivalence1T1_8",
               "OMC_7")
pnewnames <- c("AC1",
               "AC2",
               "AC3")
setnames(data_s, poldnames, pnewnames, skip_absent=TRUE)
data_s$AC1 <- 7 
data_s$AC2 <- 7
data_s$AC3 <- 1

## Merge datasets
#create variable to distinguish between student and prolific samples
data_s$Source <- 1 #student sample
data_p$Source <- 2 #prolific sample
variables <- c("Progress", #select variables necessary for analysis
               "Duration..in.seconds.",
               "Finished",
               "ResponseId",
               grep("Consent", names(data_p), value=TRUE),
               grep("Content", names(data_p), value=TRUE),
               grep("Ambivalence", names(data_p), value=TRUE),
               grep("Certainty", names(data_p), value=TRUE),
               grep("FE", names(data_p), value=TRUE),
               grep("RE", names(data_p), value=TRUE),
               grep("DB", names(data_p), value=TRUE),
               grep("OMC", names(data_p), value=TRUE),
               grep("NFC", names(data_p), value=TRUE),
               "AC1",
               "AC2",
               "AC3",
               "Age",
               "Gender",
               "Nationality",
               "Education",
               "Manipulation.Check",
               "Purpose",
               "Comments",
               "Source")
data_pa <- data_p[variables]
data_sa <- data_s[variables]
data <- rbind(data_pa, data_sa)

variables <- c("Progress", #identify numeric variables
               "Duration..in.seconds.",
               "Finished",
               grep("Consent", names(data_p), value=TRUE),
               grep("Content", names(data_p), value=TRUE),
               grep("Ambivalence", names(data_p), value=TRUE),
               grep("Certainty", names(data_p), value=TRUE),
               grep("FE", names(data_p), value=TRUE),
               grep("RE", names(data_p), value=TRUE),
               grep("DB", names(data_p), value=TRUE),
               grep("OMC", names(data_p), value=TRUE),
               grep("NFC", names(data_p), value=TRUE),
               "AC1",
               "AC2",
               "AC3",
               "Age",
               "Nationality",
               "Education",
               "Manipulation.Check",
               "Source")
data[variables] <- lapply(data[variables], as.numeric) #change numeric variables to numbers
data$Duration..in.seconds. <- data$Duration..in.seconds./60
data$ScaledDuration <- scale(data$Duration..in.seconds.)
setnames(data, "Duration..in.seconds.", "Duration", skip_absent=TRUE) #rename to reflect unit change (secs -> minutes)

setnames(data, "REPrac_1", "REPrac", skip_absent=TRUE) #rename for consistency with DB
setnames(data, "DBPrac_4", "DBPrac", skip_absent=TRUE) #rename for consistency with RE


#count number of failed attention checks (+1 to AChecks for each failure)
data$AChecks <- 0 #initally 3
data$AChecks <- ifelse(data$AC1 != 7,
                                    data$AChecks+1, 
                                    data$AChecks)
data$AChecks <- ifelse(data$AC2 != 7, 
                                    data$AChecks+1, 
                                    data$AChecks)
data$AChecks <- ifelse(data$AC3 != 1, 
                                    data$AChecks+1, 
                                    data$AChecks)
data$AChecks <- as.numeric(data$AChecks)
data_raw <- data #save copy of raw data

## Exclusions
data <- subset(data, 
               Consent_1==1 & Consent_2==1 & Consent_3==1 
               & Consent_4==1 & Consent_5==1 & Consent_6==1
               & Consent_7==1 & Consent_8==1
               & (Nationality!=0 | is.na(Nationality)) #identify as British
               & Progress==100 & Finished==1 #did not fish
               & (AChecks<=1 | is.na(AChecks)) #failed 1 or fewer attention checks
               & between(ScaledDuration, -3, 3)
               & Manipulation.Check!=0) #not outlier in terms of duration) 
data <- select(data, -c(FE5_1, FE6_1)) #remove removed first estimates

## Convert second estimates to "long" form and add condition
SEVariables <- c("ResponseId", #variables necessary to transform Second Estimates
                 grep("RE", names(data), value=TRUE),
                 grep("DB", names(data), value=TRUE))
data_se <- data[SEVariables] #pull necessary variables
qID <- c("1_1", "2_1", "3_1", "4_1", "5_1", "6_1", "7_1", "8_1", "9_1", "10_1",
         "11_1", "12_1", "13_1", "14_1", "15_1", "16_1", "17_1", "18_1", "19_1", "20_1", "21_1")
#create dataframe (data_ses) that stores second estimates
data_se2 <- data.frame(data_se$ResponseId, data_se$REPrac, data_se$DBPrac)
data_se3 <- gather(data_se2, Condition, SEPrac, 2:3)
data_ses <- subset(data_se3, SEPrac != "")
setnames(data_ses, "data_se.ResponseId", "ResponseId", skip_absent=TRUE) #rename for consistency with data_se3
#loop merging second estimates from both conditions into one variable matched to participants
for (i in qID){
  RE <- paste("RE", i, sep="")
  DB <- paste("DB", i, sep="")
  data_se2 <- data.frame(data_se$ResponseId, data_se[, RE], data_se[, DB])
  data_se3 <- gather(data_se2, Condition, SE, 2:3)
  data_se3 <- subset(data_se3, SE != "")
  data_se3 <- data_se3[-c(2)]
  setnames(data_se3, "data_se.ResponseId", "ResponseId", skip_absent=TRUE) #rename for consistency with data_ses
  data_ses <- merge(data_ses, data_se3, by = "ResponseId")
}
#rename columns
newnames <- c("ResponseId", "Condition", "SEPrac", "SE1_1", "SE2_1", "SE3_1", "SE4_1", "SE5_1", "SE6_1", "SE7_1", "SE8_1", "SE9_1",
              "SE10_1", "SE11_1", "SE12_1", "SE13_1", "SE14_1", "SE15_1", "SE16_1", "SE17_1", "SE18_1", "SE19_1", "SE20_1", "SE21_1") 
names(data_ses) <- newnames
data <- merge(data_ses, data, by="ResponseId") #merge with main dataset
data$Condition <- factor(data$Condition,
                    levels = c("data_se.DBPrac", "data_se.REPrac"),
                    labels = c("Dialectical Bootstrapping", "Repeated Estimates"))

### COMBINING VARIABLES
## Reverse Score
columnsToReverse <- c("OMC_1", "OMC_2", "OMC_3",
                      "X1_Ambivalence1T1_3", "X1_Ambivalence1T1_5", "X1_Ambivalence1T1_7",
                      "X2_Ambivalence1T1_3", "X2_Ambivalence1T1_5", "X2_Ambivalence1T1_7",
                      "X1_Ambivalence1T2_3", "X1_Ambivalence1T2_5", "X1_Ambivalence1T2_7",
                      "X2_Ambivalence1T2_3", "X2_Ambivalence1T2_5", "X2_Ambivalence1T2_7")
data[,columnsToReverse] <- 8-data[,columnsToReverse]

## Compute Alphas
# by time and attitude
X1Ambiv <- data.frame(data[,grep("X1_Ambivalence", names(data))]) #create dataset with just X1 Ambiv measures
aX1_AmbivalenceT1 <- psych::alpha(data.frame(X1Ambiv[,grep("T1", names(X1Ambiv))])) #subset T1 and calc alpha
aX1_AmbivalenceT2 <- psych::alpha(data.frame(X1Ambiv[,grep("T2", names(X1Ambiv))])) #subset T2 and calc alpha
X2Ambiv <- data.frame(data[,grep("X2_Ambivalence", names(data))]) #create dataset with just X2 Ambiv measures
aX2_AmbivalenceT1 <- psych::alpha(data.frame(X1Ambiv[,grep("T1", names(X1Ambiv))])) #subset T1 and calc alpha
aX2_AmbivalenceT2 <- psych::alpha(data.frame(X1Ambiv[,grep("T2", names(X1Ambiv))])) #subset T2 and calc alpha
aX1_CertaintyT1 <- psych::alpha(data.frame(data[,grep("X1_CertaintyT1", names(data))]))
aX1_CertaintyT2 <- psych::alpha(data.frame(data[,grep("X1_CertaintyT2", names(data))]))
aX2_CertaintyT1 <- psych::alpha(data.frame(data[,grep("X2_CertaintyT1", names(data))]))
aX2_CertaintyT2 <- psych::alpha(data.frame(data[,grep("X2_CertaintyT2", names(data))]))
#entire scales
aAmbivalence <- psych::alpha(data.frame(data[,grep("Ambivalence", names(data))]))
aCertainty <- psych::alpha(data.frame(data[,grep("Certainty", names(data))]))
aOMC <- psych::alpha(data.frame(data[,grep("OMC", names(data))]))
aNFCS <- psych::alpha(data.frame(data[,grep("NFC", names(data))]))


alphas <- c(aX1_AmbivalenceT1$total$raw_alpha, aX1_CertaintyT1$total$raw_alpha,
            aX1_AmbivalenceT2$total$raw_alpha, aX1_CertaintyT2$total$raw_alpha,
            aX2_AmbivalenceT1$total$raw_alpha, aX2_CertaintyT1$total$raw_alpha,
            aX2_AmbivalenceT2$total$raw_alpha, aX2_CertaintyT2$total$raw_alpha,
            aOMC$total$raw_alpha, aNFCS$total$raw_alpha)
variables <- c("T1 Ambivalence (HR)", "T1 Certainty (HR)",
               "T2 Ambivalence (HR)", "T2 Certainty (HR)",
               "T1 Ambivalence (XR)", "T1 Certainty (XR)",
               "T2 Ambivalence (XR)", "T2 Certainty (XR)",
               "OMC", "NFCS")
items <- c("9", "4",
           "9", "4",
           "9", "4",
           "9", "4",
           "6", "15")

table_alphas <- data.frame(items, variables, alphas)

#1 how many do you think do not affiliate themselves with any religion?	45
#2 how many do you think are immigrants to this country (i.e. not born in this country?)	13
#3 how many do you think said they personally believe that homosexuality is morally unacceptable?	16
#4 how many do you think are Muslim?	5.2
#5 how many do you think said they personally believe that having an abortion is morally unacceptable?	25
#6 how many do you think die as a result of suicide?	1
#7 how many do you think die as a result of terrorism or conflict?	0.05
#8 how many do you think die as a result of cancer?	29.6
#9 how many do you think said their own health was very good or good?	74
#10	how many do you think said they personally believe that sex between unmarried adults is morally unacceptable?	11
#11	how many do you think are aged 14 or younger?	17
#12	how many do you think die due to disorders such as drug or alcohol addiction?	0.4
#13	how many do you think die as a result of interpersonal violence such as homicide or murder?	0.1
#14	how many do you think own a smartphone?	90.5
#15	how many do you think say they believe in hell?	21
#16	how many do you think say they believe in God?	39
#17	about how many working age people do you think are unemployed and looking for work?	4
#18	how many do you think have a Facebook account (who are old enough to have one, i.e. 13 and over)?	76
#19	how many do you think have access to the internet at home either through a computer or mobile device?	96
#20	how many said that, taking all things together, they are very happy or rather happy?	91.23
#21	how many do you think are either overweight or obese (excluding children)?	63
ListStim <- c("Religion (1)", "Immigrants (2)", "Homosexuality (3)", "Muslim (4)", "Terrorism/ Conflict (7)", "Cancer (8)",
              "Health (9)", "Pre-Marital Sex (10)", "14 or Younger (11)", "Drug Deaths (12)", "Murders (13)",
              "Smartphone (14)", "Believe in Hell (15)", "Believe in God (16)", "Unemployed (17)", "Facebook (18)",
              "Internet Access (19)", "Happy (20)", "Overweight (21)")

Actual_Answers <- as.integer(TableStim$A) #value with actual answers
Actual_Answers <- Actual_Answers[-c(5, 6)] #remove stimuli for removed answers
#select questions, removing stimuli 5 and 6
qID <- c("1_1", "2_1", "3_1", "4_1", "7_1", "8_1", "9_1", "10_1", 
         "11_1", "12_1", "13_1", "14_1", "15_1", "16_1", "17_1", "18_1", "19_1", "20_1", "21_1")

### RQ1: Does DB improve accuracy?
## Error of FE Calculation per q
j <- 1 #set counter
for (i in qID){
  FE <- paste("FE", qID[j], sep="") #identify FE
  FE_E <- paste("FE_Error", qID[j], sep="") #compute new variable (error) name
  data[FE_E] <- abs(Actual_Answers[j]-data[FE]) #absolute value of correct value vs first estimate per ppt
  j <- j+1
}
# Median FE Error per ppt (median in line with Herzog (2009))
data$MedFE_Error <- apply(data[ grep("FE_Error", names(data))],1, median)
data$MeanFE_Error <- apply(data[ grep("FE_Error", names(data))],1, mean)

## Error of SE Calculation per q
j <- 1 #reset counter
for (i in qID){
  SE <- paste("SE", qID[j], sep="") #identify FE
  SE_E <- paste("SE_Error", qID[j], sep="") #compute new variable (error) name
  data[SE_E] <- abs(Actual_Answers[j]-data[SE]) #absolute value of correct value vs first estimate per ppt
  j <- j+1
}
# Median SE Error per ppt (median in line with Herzog (2009))
data$MedSE_Error <- apply(data[ grep("SE_Error", names(data))],1, median)
data$MeanSE_Error <- apply(data[ grep("SE_Error", names(data))],1, mean)

## Mean Estimate (FE+SE) Calculation per q
j <- 1 #reset counter
cols <- NULL
for (i in qID){
  FE <- paste("FE", qID[j], sep="") #FE
  SE <- paste("SE", qID[j], sep="") #SE
  ME <- paste("ME", qID[j], sep="") #SE 
  data[ME] <- ((data[FE]+data[SE])/2)
  j <- j+1
  cols <- append(cols, ME)
}

data[,cols] <- lapply(data[,cols], as.integer)

## Error of Mean Estimate Calculation Per Q
j <- 1 #reset counter
for (i in qID){
  ME <- paste("ME", qID[j], sep="") #identify FE
  ME_E <- paste("ME_Error", qID[j], sep="") #compute new variable (error) name
  data[ME_E] <- abs(Actual_Answers[j]-data[ME]) #absolute value of correct value vs first estimate per ppt
  j <- j+1
}
# Median ME Error per ppt (median in line with Herzog (2009))
data$MedME_Error <- apply(data[ grep("ME_Error", names(data))],1, median)
data$MeanME_Error <- apply(data[ grep("ME_Error", names(data))],1, mean)

## Accuracy Gain per Q (Herzog, 2009)
j <- 1 #reset counter
for (i in qID){
  FE_E <- paste("FE_Error", qID[j], sep="") #compute new variable (error) name
  ME_E <- paste("ME_Error", qID[j], sep="") #identify ME error
  AG <- paste("AG", qID[j], sep="") #create AG variable per q
  data[AG] <- data[FE_E]-data[ME_E] #calculate AG per q
  j <- j+1
}
# Median Accuracy Gain per participant
data$MedAG <- apply(data[ grep("AG", names(data))],1, median)
data$MeanAG <- apply(data[ grep("AG", names(data))],1, mean)

### RQ2: Does DB make people change estimates more?
j <- 1 #reset counter
for (i in qID){
  FE <- paste("FE", qID[j], sep="") #FE Error[question]
  SE <- paste("SE", qID[j], sep="") #SE Error[question]
  EC <- paste("E_Change", qID[j], sep="") #SE Error[question]
  data[EC] <- abs(data[FE]-data[SE])
  j <- j+1
}
data$MedE_Change <- apply(data[ grep("E_Change", names(data))],1, median)
data$MeanE_Change <- apply(data[ grep("E_Change", names(data))],1, mean)


### RQ3: Is attitude ambivalence/ certainty change bigger in the DB condition?
poldnames <- c("X1_Ambivalence2T1_1",
               "X1_Ambivalence2T1_2",
               "X1_Ambivalence2T2_1",
               "X1_Ambivalence2T2_2",
               "X2_Ambivalence2T1_1",
               "X2_Ambivalence2T1_2",
               "X2_Ambivalence2T2_1",
               "X2_Ambivalence2T2_2")
pnewnames <- c("X1_Ambivalence1T1_8",
               "X1_Ambivalence1T1_9",
               "X1_Ambivalence1T2_8",
               "X1_Ambivalence1T2_9",
               "X2_Ambivalence1T1_8",
               "X2_Ambivalence1T1_9",
               "X2_Ambivalence1T2_8",
               "X2_Ambivalence1T2_9")
setnames(data, poldnames, pnewnames, skip_absent=TRUE)

setnames(data, "X1_ContentT1", "HRContT1", skip_absent=TRUE) #rename content column for consistency
setnames(data, "X1_ContentT2", "HRContT2", skip_absent=TRUE) #rename content column for consistency
data$HRAmbivT1 <- apply(data[,grep("X1_Ambivalence1T1", names(data))],1, mean)
data$HRAmbivT2 <- apply(data[,grep("X1_Ambivalence1T2", names(data))],1, mean)
data$HRStrT1 <- apply(data[,grep("X1_CertaintyT1", names(data))],1, mean)
data$HRStrT2 <- apply(data[,grep("X1_CertaintyT2", names(data))],1, mean)
setnames(data, "X2_ContentT1", "XRContT1", skip_absent=TRUE) #rename content column for consistency
setnames(data, "X2_ContentT2", "XRContT2", skip_absent=TRUE) #rename content column for consistency
data$XRAmbivT1 <- apply(data[,grep("X2_Ambivalence1T1", names(data))],1, mean)
data$XRAmbivT2 <- apply(data[,grep("X2_Ambivalence1T2", names(data))],1, mean)
data$XRStrT1 <- apply(data[,grep("X2_CertaintyT1", names(data))],1, mean)
data$XRStrT2 <- apply(data[,grep("X2_CertaintyT2", names(data))],1, mean)

### Combine OMC and NFCS items
data$OMC <- apply(data[,grep("OMC", names(data))],1, mean)
data$NFCS <- apply(data[,grep("NFC", names(data))],1, mean)
data$OpenMind <- (data$OMC+(8-data$NFCS))/2

#reorder dataset to match Qualtrics
variables <- c("ResponseId", #select variables necessary for analyses
               grep("Duration", names(data), value=TRUE),
               "Condition",
               grep("HR", names(data), value=TRUE),
               grep("XR", names(data), value=TRUE),
               grep("FE", names(data), value=TRUE),
               grep("SE", names(data), value=TRUE),
               grep("ME", names(data), value=TRUE),
               grep("AG", names(data), value=TRUE),
               grep("E_Change", names(data), value=TRUE),
               "OMC",
               "NFCS",
               "OpenMind",
               "Age",
               "Gender",
               "Education",
               "Manipulation.Check",
               "Purpose",
               "Comments",
               "Source")
data <- data[, variables] #necessary variables for analysis
data[data == -99] <- NA