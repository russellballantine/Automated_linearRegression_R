#set up the environment
setwd("C:\\Users\\ballanr\\Desktop\\Data_Science\\Edinburgh_University\\DATA SCIENCE MSc\\Probability and Statistics\\Final_Assignment")


########################### DATA PREPARATION(WRANGLING) SECTION ##########################################


#read and attach the Tab delimited data into R as dataframes.Suppress row numbers
data_biomarkers <- read.table("Biomarkers.txt",header=TRUE)
data_covariates <- read.table("Covariates.txt",header=TRUE)
attach(data_biomarkers)
attach(data_covariates)

#sort both dataframes by patient id number
data_biomarkers[order("Biomarker"), ]
data_covariates[order("PatientID"), ]

#Check each observation(row) in dataframes are complete
complete.cases(data_biomarkers)
complete.cases(data_covariates)

#Don't need the "6week" data entries so remove these from the data_biomarkers dataframe
data_biomarkers2 <- data_biomarkers[!grepl("6weeks", data_biomarkers$Biomarker),]

#Seperate the inclusion and 12month results from Biomarker into different dataframes.
index <- data_biomarkers2$Biomarker
index2 <- grep("-0weeks",index)         #observation ids for inclusion
index3 <- grep("-12months",index)       #observation ids at 12 months

biomarkers_inclusion <- data_biomarkers2[index2,] #dataframe of inclusion biomarkers
biomarkers_12months <- data_biomarkers2[index3,]  #dataframe of biomarkers at 12 months

#Now need to sort the dataframes by patient id
#BIOMARKERS at inclusion sort
biomarkers_inclusion[order(Biomarker),]
matchlist <- c("126-0weeks","127-0weeks","128-0weeks","129-0weeks","130-0weeks","131-0weeks","132-0weeks","133-0weeks","134-0weeks", "135-0weeks", "136-0weeks","137-0weeks","138-0weeks", "139-0weeks","140-0weeks","141-0weeks","142-0weeks","143-0weeks",
               "149-0weeks","150-0weeks","151-0weeks","152-0weeks")

x <- as.character(biomarkers_inclusion$Biomarker) %in% matchlist
biomarkers_inclusion <- rbind(biomarkers_inclusion[!x,], biomarkers_inclusion[x,])
biomarkers_inclusion #check


#BIOMARKERS at 12 months sort
biomarkers_12months[order(as.character(grep("^[1-9]{1,3}",biomarkers_12months$Biomarker,value=T)),decreasing=FALSE),]

matchlist2 <- c("100-12months","101-12months","102-12months","104-12months","105-12months","108-12months","109-12months","111-12months",
                "112-12months","113-12months","114-12months","118-12months","119-12months","121-12months","124-12months","127-12months",
                "128-12months","129-12months","130-12months","131-12months","132-12months","133-12months","134-12months","135-12months",
                "136-12months","137-12months","138-12months","139-12months","140-12months","141-12months","142-12months","143-12months",
                "149-12months","150-12months","151-12months","152-12months")
x <- as.character(biomarkers_12months$Biomarker) %in% matchlist2
biomarkers_12months <- rbind(biomarkers_12months[!x,], biomarkers_12months[x,])
biomarkers_12months

#Sort out the rownames
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months)
rownames(biomarkers_inclusion) <- 1:nrow(biomarkers_inclusion)
rownames(data_covariates) <- 1:nrow(data_covariates)


#Sort out remaining rows
#Function for inserting a row at r+1 and renumbering accordingly
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

###Put this in loop if have time ######## 
backup <- biomarkers_12months[1:11,]
biomarkers_12months[1:11,] <- biomarkers_12months[15:25,] 
biomarkers_12months[15:25,] <- backup[1:11,]  
biomarkers_12months

backup <- biomarkers_12months[65,]
biomarkers_12months[65,] <- biomarkers_12months[12,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 66)
biomarkers_12months <- biomarkers_12months[-c(12),]
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months)

backup <- biomarkers_12months[76,]
biomarkers_12months[76,] <- biomarkers_12months[12,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 77)
biomarkers_12months <- biomarkers_12months[-c(12),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again


backup <- biomarkers_12months[19,]
biomarkers_12months[19,] <- biomarkers_12months[12,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 20)
biomarkers_12months <- biomarkers_12months[-c(12),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[27,]
biomarkers_12months[27,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 28)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[30,]
biomarkers_12months[30,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 31)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[33,]
biomarkers_12months[33,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 34)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[36,]
biomarkers_12months[36,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 37)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[39,]
biomarkers_12months[39,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 40)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[44,]
biomarkers_12months[44,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 45)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[48,]
biomarkers_12months[48,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 49)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[51,]
biomarkers_12months[51,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 52)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[54,]
biomarkers_12months[54,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 55)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[57,]
biomarkers_12months[57,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 58)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[60,]
biomarkers_12months[60,] <- biomarkers_12months[13,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 61)
biomarkers_12months <- biomarkers_12months[-c(13),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[79,]
biomarkers_12months[79,] <- biomarkers_12months[97,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 80)
biomarkers_12months <- biomarkers_12months[-c(98),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[80,]
biomarkers_12months[80,] <- biomarkers_12months[98,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 81)
biomarkers_12months <- biomarkers_12months[-c(99),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[81,]
biomarkers_12months[81,] <- biomarkers_12months[99,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 82)
biomarkers_12months <- biomarkers_12months[-c(100),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[82,]
biomarkers_12months[82,] <- biomarkers_12months[100,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 83)
biomarkers_12months <- biomarkers_12months[-c(101),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[83,]
biomarkers_12months[83,] <- biomarkers_12months[101,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 84)
biomarkers_12months <- biomarkers_12months[-c(102),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[94,]
biomarkers_12months[94,] <- biomarkers_12months[102,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 95)
biomarkers_12months <- biomarkers_12months[-c(103),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[98,]
biomarkers_12months[98,] <- biomarkers_12months[103,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 99)
biomarkers_12months <- biomarkers_12months[-c(104),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[101,]
biomarkers_12months[101,] <- biomarkers_12months[104,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 101)
biomarkers_12months <- biomarkers_12months[-c(105),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[80,]
biomarkers_12months[80,] <- biomarkers_12months[105,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 81)
biomarkers_12months <- biomarkers_12months[-c(106),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[81,]
biomarkers_12months[81,] <- biomarkers_12months[106,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 82)
biomarkers_12months <- biomarkers_12months[-c(107),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[82,]
biomarkers_12months[82,] <- biomarkers_12months[107,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 83)
biomarkers_12months <- biomarkers_12months[-c(108),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[84,]
biomarkers_12months[84,] <- biomarkers_12months[108,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 85)
biomarkers_12months <- biomarkers_12months[-c(109),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[85,]
biomarkers_12months[85,] <- biomarkers_12months[109,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 86)
biomarkers_12months <- biomarkers_12months[-c(110),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[86,]
biomarkers_12months[86,] <- biomarkers_12months[110,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 87)
biomarkers_12months <- biomarkers_12months[-c(111),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[88,]
biomarkers_12months[88,] <- biomarkers_12months[111,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 89)
biomarkers_12months <- biomarkers_12months[-c(112),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[89,]
biomarkers_12months[89,] <- biomarkers_12months[112,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 90)
biomarkers_12months <- biomarkers_12months[-c(113),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[91,]
biomarkers_12months[91,] <- biomarkers_12months[113,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 92)
biomarkers_12months <- biomarkers_12months[-c(114),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

backup <- biomarkers_12months[92,]
biomarkers_12months[92,] <- biomarkers_12months[114,]
biomarkers_12months <- insertRow(biomarkers_12months, backup, 93)
biomarkers_12months <- biomarkers_12months[-c(115),] #remove the duplicate row
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months) #correct rownames again

###################END OF LOOP############################

biomarkers_12months #Check

#Check ordered dataframe sizes ahead of joining
dim(biomarkers_12months)
dim(biomarkers_inclusion)
dim(data_covariates)

#Write out to files for visual inspection
write.table(biomarkers_inclusion, file = "C:\\Users\\ballanr\\Desktop\\Data_Science\\Edinburgh_University\\DATA SCIENCE MSc\\Probability and Statistics\\Final_Assignment\\biomarkers_inclusion_out.txt", sep = "")
write.table(biomarkers_12months, file = "C:\\Users\\ballanr\\Desktop\\Data_Science\\Edinburgh_University\\DATA SCIENCE MSc\\Probability and Statistics\\Final_Assignment\\biomarkers_12months_out.txt", sep = "")
write.table(data_covariates, file = "C:\\Users\\ballanr\\Desktop\\Data_Science\\Edinburgh_University\\DATA SCIENCE MSc\\Probability and Statistics\\Final_Assignment\\data_covariates_out.txt", sep = "")
#Looks like 114 patients made it through to the end of the trial.

#From visual inspection, observations of patient IDs 40,42,49,51,117,122 and 126 are not present across all datasets
#so we can remove these observations from the data frames. 
biomarkers_12months <- biomarkers_12months[-c(31,33,40),] 
biomarkers_inclusion <- biomarkers_inclusion[-c(32,38,40,90,94,96),]
data_covariates <- data_covariates[-c(31,38,89,93,95),]

#Check ordered dataframe sizes ahead of joining
dim(biomarkers_12months)
dim(biomarkers_inclusion)
dim(data_covariates)

#Sort out the finalised rownames so DFs can be merged on rowname
rownames(biomarkers_12months) <- 1:nrow(biomarkers_12months)
rownames(biomarkers_inclusion) <- 1:nrow(biomarkers_inclusion)
rownames(data_covariates) <- 1:nrow(data_covariates)

#Merge dataframes on patientID, Vas.12months and Vas.inclusion
#To create two data frames one for inclusion data and one for 12months data
merged_DF_inclusion <- merge(biomarkers_inclusion,data_covariates,by='row.names',all=TRUE,sort=FALSE)
variable_drop_inclusion <- c("Age","Sex.1.male.2.female.","Smoker.1.yes.2.no.","Vas.12months")
biomarkers_inclusion_final <- merged_DF_inclusion[,!(names(merged_DF_inclusion) %in% variable_drop_inclusion)]

merged_DF_12months <- merge(biomarkers_12months,data_covariates,by='row.names',all=TRUE,sort=FALSE)
variable_drop_12months <- c("Age","Sex.1.male.2.female.","Smoker.1.yes.2.no.","VAS.at.inclusion")
biomarkers_12months_final <- merged_DF_12months[,!(names(merged_DF_12months) %in% variable_drop_12months)]

#Check the data frames
biomarkers_inclusion_final
biomarkers_12months_final
str(biomarkers_inclusion_final)
str(biomarkers_12months_final)

########################### END DATA PREPARATION(WRANGLING) SECTION ##########################################



#===========================Exploratory Data Analysis(EDA) ================================================================

#Display summary statistics of the inclusion and 12months dataframes.
#From these we can identify the mean values for each Biomarker at inclusion and 12 month period.
#These mean values will form the measures of center for each random variable(protein).
#We assume each random variable is normally distributed, N(mu, sigma^2). We can use these for
#subsequent hypothesis testing.

summary(biomarkers_inclusion_final)
summary(biomarkers_12months_final)
boxplot(biomarkers_12months_final[,3:11], main = "summary of random variables,12 months", ylab = "Protein Level summary 12months")
Mean_IL8_12months = mean(biomarkers_12months_final$IL8)
points(Mean_IL8_12months,col="red",pch="+",cex=2)
Mean_VEGFA_12months = mean(biomarkers_12months_final$VEGFA)
points(Mean_VEGFA_12months,col="red",pch="+",cex=2,x=2)
Mean_OPG_12months = mean(biomarkers_12months_final$OPG)
points(Mean_OPG_12months,col="red",pch="+",cex=2,x=3)
Mean_TGFbeta1_12months = mean(biomarkers_12months_final$TGFbeta1)
points(Mean_TGFbeta1_12months,col="red",pch="+",cex=2,x=4)
Mean_IL6_12months = mean(biomarkers_12months_final$IL6)
points(Mean_IL6_12months,col="red",pch="+",cex=2,x=5)
Mean_CXCL9_12months = mean(biomarkers_12months_final$CXCL9)
points(Mean_CXCL9_12months,col="red",pch="+",cex=2,x=6)
Mean_CXCL1_12months = mean(biomarkers_12months_final$CXCL1)
points(Mean_CXCL1_12months,col="red",pch="+",cex=2,x=7)
Mean_IL18_12months = mean(biomarkers_12months_final$IL18)
points(Mean_IL18_12months,col="red",pch="+",cex=2,x=8)
Mean_CSF1_12months = mean(biomarkers_12months_final$CSF1)
points(Mean_CSF1_12months,col="red",pch="+",cex=2,x=9)

boxplot(biomarkers_inclusion_final[,3:11], main = "summary of random variables,inclusion", ylab = "Protein Level summary inclusion")
Mean_IL8_inclusion = mean(biomarkers_inclusion_final$IL8)
points(Mean_IL8_inclusion,col="red",pch="+",cex=2)
Mean_VEGFA_inclusion = mean(biomarkers_inclusion_final$VEGFA)
points(Mean_VEGFA_inclusion,col="red",pch="+",cex=2,x=2)
Mean_OPG_inclusion = mean(biomarkers_inclusion_final$OPG)
points(Mean_OPG_inclusion,col="red",pch="+",cex=2,x=3)
Mean_TGFbeta1_inclusion = mean(biomarkers_inclusion_final$TGFbeta1)
points(Mean_TGFbeta1_inclusion,col="red",pch="+",cex=2,x=4)
Mean_IL6_inclusion = mean(biomarkers_inclusion_final$IL6)
points(Mean_IL6_inclusion,col="red",pch="+",cex=2,x=5)
Mean_CXCL9_inclusion = mean(biomarkers_inclusion_final$CXCL9)
points(Mean_CXCL9_inclusion,col="red",pch="+",cex=2,x=6)
Mean_CXCL1_inclusion = mean(biomarkers_inclusion_final$CXCL1)
points(Mean_CXCL1_inclusion,col="red",pch="+",cex=2,x=7)
Mean_IL18_inclusion = mean(biomarkers_inclusion_final$IL18)
points(Mean_IL18_inclusion,col="red",pch="+",cex=2,x=8)
Mean_CSF1_inclusion = mean(biomarkers_inclusion_final$CSF1)
points(Mean_CSF1_inclusion,col="red",pch="+",cex=2,x=9)

#Create scatter plots or histograms of each variable.Looking for random scatter plots to suggest normal distributed data.
plot(biomarkers_inclusion_final$IL8, xlab = 'Patient ID', ylab = "IL8 Protein Level")
plot(biomarkers_inclusion_final$VEGFA, xlab = 'Patient ID', ylab = "VEGFA Protein Level")
plot(biomarkers_inclusion_final$OPG, xlab = 'Patient ID', ylab = "OPG Protein Level") 
plot(biomarkers_inclusion_final$TGFbeta1, xlab = 'Patient ID', ylab = "TGFbeta1 Protein Level")
plot(biomarkers_inclusion_final$IL6, xlab = 'Patient ID', ylab = "IL6 Protein Level")
plot(biomarkers_inclusion_final$CXCL9, xlab = 'Patient ID', ylab = "CXCL9 Protein Level")
plot(biomarkers_inclusion_final$CXCL1, xlab = 'Patient ID', ylab = "CXCL1 Protein Level")
plot(biomarkers_inclusion_final$IL18, xlab = 'Patient ID', ylab = "IL18 Protein Level")
plot(biomarkers_inclusion_final$CSF1, xlab = 'Patient ID', ylab = "CSF1 Protein Level")


#PLot the dataframes looking for correlated effects between the variables.
#resulting plot is a correlation matrix of columns
#Shows strong positive linear correlation between some variables(e.g. TGFbeta1 and VEGFA)
plot(biomarkers_inclusion_final, main = "Correlation Matrix-inclusion")
plot(biomarkers_12months_final, main = "Correlation Matrix-12months")
  
############### Hypothesis testing ######################################################################
#
#We assume H_0: mean_inclusion = mean_12months and alternative hypothesis, H_1: #mean_inclusion =/= mean_12months. 
#
#We use Welch's parametric paired t-test which does not require
#equal variance between groups.We extract the p-value and adjust for multiple comparisons. 

test.IL8 <- t.test(biomarkers_inclusion_final$IL8,biomarkers_12months_final$IL8,paired=TRUE)
p.adjust(test.IL8$p.value,"bonferroni",9)
test.VEGFA <-t.test(biomarkers_inclusion_final$VEGFA,biomarkers_12months_final$VEGFA,paired=TRUE)
p.adjust(test.VEGFA$p.value,"bonferroni",9)
test.OPG <- t.test(biomarkers_inclusion_final$OPG,biomarkers_12months_final$OPG,paired=TRUE)
p.adjust(test.OPG$p.value,"bonferroni",9)
test.TGFbeta1 <- t.test(biomarkers_inclusion_final$TGFbeta1,biomarkers_12months_final$TGFbeta1,paired=TRUE)
p.adjust(test.TGFbeta1$p.value,"bonferroni",9)
test.IL6<- t.test(biomarkers_inclusion_final$IL6,biomarkers_12months_final$IL6,paired=TRUE)
p.adjust(test.IL6$p.value,"bonferroni",9)
test.CXCL9 <- t.test(biomarkers_inclusion_final$CXCL9,biomarkers_12months_final$CXCL9,paired=TRUE)
p.adjust(test.CXCL9$p.value,"bonferroni",9)
test.CXCL1 <- t.test(biomarkers_inclusion_final$CXCL1,biomarkers_12months_final$CXCL1,paired=TRUE)
p.adjust(test.CXCL1$p.value,"bonferroni",9)
test.IL18 <- t.test(biomarkers_inclusion_final$IL18,biomarkers_12months_final$IL18,paired=TRUE)
p.adjust(test.IL18$p.value,"bonferroni",9)
test.CSF1 <- t.test(biomarkers_inclusion_final$CSF1,biomarkers_12months_final$CSF1,paired=TRUE)
p.adjust(test.CSF1$p.value,"bonferroni",9)

#The results of the t-test above show that we should reject the Null Hypothesis 
#in favour of the alternative hypothesis that the mean parameter of each random variable(protein level)
#at inclusion and 12months thereafter differ significantly at the alpha = 0.05(default) significance level.
# NB include the results of the hypothesis test in LaTeX formatted tables in the report
#(overleaf.com/learn/latex/Tables#creating_a_simple_table_in_LaTeX)

#From the EDA boxplots, visual inspection suggests the mean at 12 months is less that at inclusion in most cases.
#We will change our alternative hypothesis to investigate if the Null hypothesis is still rejected in preference 
#to this alternative.
t.test(biomarkers_inclusion_final$IL8,biomarkers_12months_final$IL8,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$VEGFA,biomarkers_12months_final$VEGFA,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$OPG,biomarkers_12months_final$OPG,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$TGFbeta1,biomarkers_12months_final$TGFbeta1,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$IL6,biomarkers_12months_final$IL6,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$CXCL9,biomarkers_12months_final$CXCL9,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$CXCL1,biomarkers_12months_final$CXCL1,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$IL18,biomarkers_12months_final$IL18,alternative = "greater",paired=TRUE)
t.test(biomarkers_inclusion_final$CSF1,biomarkers_12months_final$CSF1,alternative = "greater",paired=TRUE)

#Check for normality on the paired differences to verify the validity of the paired t-test. 
Before = biomarkers_inclusion_final$IL8
After = biomarkers_12months_final$IL8
Difference_IL8 = After - Before
hist(Difference_IL8)
Before = biomarkers_inclusion_final$VEGFA
After = biomarkers_12months_final$VEGFA
Difference_VEGFA = After - Before
hist(Difference_VEGFA)
Before = biomarkers_inclusion_final$OPG
After = biomarkers_12months_final$OPG
Difference_OPG = After - Before
hist(Difference_OPG)
Before = biomarkers_inclusion_final$TGFbeta1
After = biomarkers_12months_final$TGFbeta1
Difference_tgfbeta1 = After - Before
hist(Difference_tgfbeta1)
Before = biomarkers_inclusion_final$IL6
After = biomarkers_12months_final$IL6
Difference_IL6 = After - Before
hist(Difference_IL6)
Before = biomarkers_inclusion_final$CXCL9
After = biomarkers_12months_final$CXCL9
Difference_CXCL9 = After - Before
hist(Difference_CXCL9)
Before = biomarkers_inclusion_final$CXCL1
After = biomarkers_12months_final$CXCL1
Difference_CXCL1 = After - Before
hist(Difference_CXCL1)
Before = biomarkers_inclusion_final$IL18
After = biomarkers_12months_final$IL18
Difference_IL18 = After - Before
hist(Difference_IL18)
Before = biomarkers_inclusion_final$CSF1
After = biomarkers_12months_final$CSF1
Difference_CSF1 = After - Before
hist(Difference_CSF1)



#################### Linear Regression Analysis ###############################################


#Create dataframe for linear regression
DF_for_regression <- data.frame("Biomarker" = biomarkers_inclusion_final$Biomarker,
                                "IL8" = biomarkers_inclusion_final$IL8,
                                "VEGFA" = biomarkers_inclusion_final$VEGFA,
                                "OPG" = biomarkers_inclusion_final$OPG,
                                "TGFbeta1" = biomarkers_inclusion_final$TGFbeta1,
                                "IL6" = biomarkers_inclusion_final$IL6,
                                "CXCL9" = biomarkers_inclusion_final$CXCL9,
                                "CXCL1" = biomarkers_inclusion_final$CXCL1,
                                "IL18" = biomarkers_inclusion_final$IL18,
                                "CSF1" = biomarkers_inclusion_final$CSF1,
                                "Vas_12months" = biomarkers_12months_final$Vas.12months,
                                "Age" = data_covariates$Age,
                                "Sex" = data_covariates$Sex.1.male.2.female.,
                                "Smoker" = data_covariates$Smoker.1.yes.2.no.)

#THe regression model

#Split Training and testing data 80:20
Training_Data <- DF_for_regression[1:88,]
Predict_Data <- DF_for_regression[89:111,]

#Train a model using Training_Data
X_model <- lm(Vas_12months ~ (IL8+VEGFA+OPG+TGFbeta1+IL6+CXCL9+CXCL1+IL18+CSF1+Age+Sex+Smoker) , Training_Data)

# Calculate predictions and 95 % prediction and confidence intervals 
model_prediction <- predict(X_model, Predict_Data)
prediction_interval <- predict(X_model, int="prediction", Predict_Data)
confidence_interval <- predict(X_model, int="confidence", Predict_Data)

# Graphics

plot(residuals(X_model),main="scatter plot of model residuals",xlab="patientID") # Random point cloud suggests a normal distribution 
hist(residuals(X_model)) #Histogram shows normaly distributed residuals

#Predictive results

model_prediction #Predicted Vas_12months values for patient IDs 89 - 111
confidence_interval # Lower and upper bound using 95% CI

plot(model_prediction, col='red',pch=16,xlim=c(0,25),ylim=c(-10,10))
points(mean(model_prediction), col="red", lwd=3, lty=2)
points(Predict_Data$Vas_12months, col='blue',pch=16)
difference = model_prediction - Predict_Data$Vas_12months
points(difference,col="green",pch=16)
legend(1, -5, legend=c("predicted", "actual","difference"),
       col=c("red", "blue","green"), lty=4:4,cex=0.8)

#Comparing the mean values of predicted actual and difference seems to suggest some predictive capability.
mean(model_prediction)
mean(difference)
mean(Predict_Data$Vas_12months) 