#A.

library(Hmisc) #Note: Cannot have both Hmisc and e1071 libraries activated or else it won't work.

#Import breast cancer data set. 
uciwd = "https://archive.ics.uci.edu/ml/machine-learning-databases/"
mldata = paste0(uciwd,"breast-cancer-wisconsin/breast-cancer-wisconsin.data")
bcancer = read.csv(mldata, header = F)

#Rename data frame columns.
colnames(bcancer) = c("ID", "clump_thick", "cell_size", "cell_shape", "marginal", "epithelial", "nuclei",
                    "chromatin", "nucleoli", "mitoses", "class")
str(bcancer)
summary(bcancer)

#Replace "?" in nuclei variable with NAs and change it to a numeric variable.
table(bcancer$nuclei)
bcancer$nuclei = as.numeric(gsub("\\?", "NA", bcancer$nuclei))

#Use imputation method to replace NAs in nuclei variable.
bcancer$nuclei = impute(bcancer$nuclei, mean) #Replaces NAs with mean value.

#Run logistic regression on the data set.
#Change class variable so that a "2" represents "benign" and a "4" represents "malignant". Also change class variable to a factor.
bcancer$class = ifelse(bcancer$class =="2", "benign","malignant")
bcancer$class = as.factor(bcancer$class)
glm.fit = glm(class ~ ., data = bcancer, family = binomial)
summary(glm.fit) #AIC = 138.29.

#B.

library(dplyr)
library(readxl) 
library(tidyr)
library(writexl)

#Set working directory.
setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 3/gig economy project")

#Use for loop to import non-employer data from 2016-2018.
datalist = list.files(pattern = "*us.txt")
for (i in 1:length(datalist)) { 
  data = read.csv(datalist[i]) 
  assign(paste0("nemp", i+15), data)
}

#Import non-employer data from 2005-2015 from the U.S. Census website.
censuswd ="https://www2.census.gov/programs-surveys/nonemployer-statistics/datasets/"

nonemp15 = paste(censuswd,"2015/historical-datasets/nonemp15us.txt", sep="")
nemp15 = read.csv(nonemp15)   
nonemp14 = paste(censuswd,"2014/historical-datasets/nonemp14us.txt", sep="")
nemp14 = read.csv(nonemp14)  
nonemp13 = paste(censuswd,"2013/historical-datasets/nonemp13us.txt", sep="")
nemp13 = read.csv(nonemp13)   
nonemp12 = paste(censuswd,"2012/historical-datasets/nonemp12us.txt", sep="")
nemp12 = read.csv(nonemp12)  
nonemp11 = paste(censuswd,"2011/historical-datasets/nonemp11us.txt", sep="")
nemp11 = read.csv(nonemp11)   
nonemp10 = paste(censuswd,"2010/historical-datasets/nonemp10us.txt", sep="")
nemp10 = read.csv(nonemp10)  
nonemp9 = paste(censuswd,"2009/historical-datasets/nonemp09us.txt", sep="")
nemp9 = read.csv(nonemp9)   
nonemp8 = paste(censuswd,"2008/historical-datasets/nonemp08us.txt", sep="")
nemp8 = read.csv(nonemp8) 
nonemp7 = paste(censuswd,"2007/historical-datasets/nonemp07us.txt", sep="")
nemp7 = read.csv(nonemp7)   
nonemp6 = paste(censuswd,"2006/historical-datasets/nonemp06us.txt", sep="")
nemp6 = read.csv(nonemp6)  
nonemp5 = paste(censuswd,"2005/historical-datasets/nonemp05us.txt", sep="")
nemp5 = read.csv(nonemp5)   

#Data cleaning: For each year of the non-employer data, select the NAICS, ESTAB, and RCPTOT variables when the ST == 0, LFO == "-", and RCPTOT_SIZE == 1.
#Calculate income variable and add it to subset data frame.
#Change NAICS variable to a character.
#Only include NAICS data for the ride-sharing industry (NAICS code: 4853).
#Rename columns in data frame to reflect the year the data is from.
la18 = subset(nemp18, ST==0 & LFO == "-" & RCPTOT_SIZE ==1, select=c(NAICS, ESTAB, RCPTOT))
la18$income = 1000*la18$RCPTOT/la18$ESTAB
la18$NAICS = as.character(la18$NAICS)
la18.d2 = la18 %>% subset(NAICS == 4853) %>% rename(est18=ESTAB) %>% rename(income18=income) %>% mutate(RCPTOT=NULL)

la17 = subset(nemp17, ST==0 & LFO == "-" & RCPTOT_SIZE ==1, select=c(NAICS, ESTAB, RCPTOT))
la17$income = 1000*la17$RCPTOT/la17$ESTAB
la17$NAICS = as.character(la17$NAICS)
la17.d2 = la17 %>% subset(NAICS == 4853) %>% rename(est17=ESTAB) %>% rename(income17=income) %>% mutate(RCPTOT=NULL)

la16 = subset(nemp16, ST==0 & LFO == "-" & RCPTOT_SIZE ==1, select=c(NAICS, ESTAB, RCPTOT))
la16$income = 1000*la16$RCPTOT/la16$ESTAB
la16$NAICS = as.character(la16$NAICS)
la16.d2 = la16 %>% subset(NAICS == 4853) %>% rename(est16=ESTAB) %>% rename(income16=income) %>% mutate(RCPTOT=NULL)

# Variable names become lower case for 2008-2015.
la15 = subset(nemp15, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la15$income = 1000*la15$rcptot/la15$estab
la15$naics = as.character(la15$naics)
la15.d2 = la15 %>% subset(naics == 4853) %>% rename(est15=estab) %>% rename(income15=income) %>% mutate(rcptot=NULL)

la14 = subset(nemp14, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la14$income = 1000*la14$rcptot/la14$estab
la14$naics = as.character(la14$naics)
la14.d2 = la14 %>% subset(naics == 4853) %>% rename(est14=estab) %>% rename(income14=income) %>% mutate(rcptot=NULL)

la13 = subset(nemp13, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la13$income = 1000*la13$rcptot/la13$estab
la13$naics = as.character(la13$naics)
la13.d2 = la13 %>% subset(naics == 4853) %>% rename(est13=estab) %>% rename(income13=income) %>% mutate(rcptot=NULL)

la12 = subset(nemp12, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la12$income = 1000*la12$rcptot/la12$estab
la12$naics = as.character(la12$naics)
la12.d2 = la12 %>% subset(naics == 4853) %>% rename(est12=estab) %>% rename(income12=income) %>% mutate(rcptot=NULL)

la11 = subset(nemp11, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la11$income = 1000*la11$rcptot/la11$estab
la11$naics = as.character(la11$naics)
la11.d2 = la11 %>% subset(naics == 4853) %>% rename(est11=estab) %>% rename(income11=income) %>% mutate(rcptot=NULL)

la10 = subset(nemp10, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la10$income = 1000*la10$rcptot/la10$estab
la10$naics = as.character(la10$naics)
la10.d2 = la10 %>% subset(naics == 4853) %>% rename(est10=estab) %>% rename(income10=income) %>% mutate(rcptot=NULL)

la9 = subset(nemp9, st==0 & lfo == "-" & rcptot_size ==1, select=c(naics, estab, rcptot))
la9$income = 1000*la9$rcptot/la9$estab
la9$naics = as.character(la9$naics)
la9.d2 = la9 %>% subset(naics == 4853) %>% rename(est9=estab) %>% rename(income9=income) %>% mutate(rcptot=NULL)

la8 = subset(nemp8, st==0 & lfo == "-", select=c(naics, estab, rcptot))
la8$income = 1000*la8$rcptot/la8$estab
la8$naics = as.character(la8$naics)
la8.d2 = la8 %>% subset(naics == 4853) %>% rename(est8=estab) %>% rename(income8=income) %>% mutate(rcptot=NULL)

la7 = subset(nemp7, ST==0, select=c(NAICS, ESTAB, RCPTOT))
la7$income = 1000*la7$RCPTOT/la7$ESTAB
la7$NAICS = as.character(la7$NAICS)
la7.d2 = la7 %>% subset(NAICS == 4853) %>% rename(est7=ESTAB) %>% rename(income7=income) %>% mutate(RCPTOT=NULL)

la6 = subset(nemp6, ST==0, select=c(NAICS, ESTAB, RCPTOT))
la6$income = 1000*la6$RCPTOT/la6$ESTAB
la6$NAICS = as.character(la6$NAICS)
la6.d2 = la6 %>% subset(NAICS == 4853) %>% rename(est6=ESTAB) %>% rename(income6=income) %>% mutate(RCPTOT=NULL)

la5 = subset(nemp5, ST==0, select=c(NAICS, ESTAB, RCPTOT))
la5$income = 1000*la5$RCPTOT/la5$ESTAB
la5$NAICS = as.character(la5$NAICS)
la5.d2 = la5 %>% subset(NAICS == 4853) %>% rename(est5=ESTAB) %>% rename(income5=income) %>% mutate(RCPTOT=NULL)

# Make "NAICS" consistent across the whole data sample.
la8.d2 = la8.d2 %>% rename(NAICS=naics)
la9.d2 = la9.d2 %>% rename(NAICS=naics)
la10.d2 = la10.d2 %>% rename(NAICS=naics)
la11.d2 = la11.d2 %>% rename(NAICS=naics)
la12.d2 = la12.d2 %>% rename(NAICS=naics)
la13.d2 = la13.d2 %>% rename(NAICS=naics)
la14.d2 = la14.d2 %>% rename(NAICS=naics)
la15.d2 = la15.d2 %>% rename(NAICS=naics)

#Left join all data frames by "NAICS" variable.
us.nemp.rideshare = left_join(la5.d2, la6.d2, by="NAICS") %>% left_join(la7.d2,by="NAICS", keep=F) %>% left_join(la8.d2,by="NAICS", keep=F) %>% 
  left_join(la9.d2,by="NAICS", keep=F) %>%  left_join(la10.d2,by="NAICS", keep=F) %>% left_join(la11.d2,by="NAICS", keep=F) %>% left_join(la12.d2,by="NAICS", keep=F) %>% 
  left_join(la13.d2,by="NAICS", keep=F) %>% left_join(la14.d2,by="NAICS", keep=F) %>% left_join(la15.d2,by="NAICS", keep=F) %>% left_join(la16.d2,by="NAICS", keep=F) %>% 
  left_join(la17.d2,by="NAICS", keep=F) %>% left_join(la18.d2,by="NAICS", keep=F)

#Organize column order of data frame.
us.nemp.rideshare = us.nemp.rideshare[c("NAICS","est5","est6","est7","est8","est9","est10","est11","est12","est13","est14","est15","est16","est17","est18",
                    "income5","income6","income7","income8","income9","income10","income11","income12","income13","income14","income15","income16","income17","income18")]

#Export data frame to Excel file.
outputDir = "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 3/gig economy project"
write_xlsx(us.nemp.rideshare, paste0(outputDir, "us.nemp.rideshare.xlsx"))

#C.

library(leaps)

#Set working directory.
setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Week 3/Script & Data Week 3")

#Source D03c_zillow code.
source("D03c_zillow.R", echo = TRUE)

#Change standard of being good_features from with missing_pct to < 0.25.
good_features = filter(missing_values, missing_pct<0.25)
good_features
gfeature = as.vector(good_features[,1])

#For logerror:
zdata = cor_tmp %>% dplyr::select(logerror, gfeature)

#For abs_logerror:
zdata3 = cor_tmp %>% dplyr::select(abs_logerror, gfeature)

#Data cleaning:
#Remove certain variables from zdata and zdata3 data sets because they are either geographic info and ID, one value, or pure linear combination of other variables.
zdata = zdata[,-c(2, 12, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 28)]
zdata3 = zdata3[,-c(2, 12, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 28)]

#Check for correlations among the variables in the zdata and zdata3 data frames and remove highly correlated variables.
zdata1 = na.omit(zdata) #Remove NAs so correlation can be calculates.
cor.zdata1 = cor(zdata1)
cor.zdata1
corrplot(cor(zdata1),type="lower", method="number")
zdata1 = zdata1[,-c(4, 8, 9, 14)] #Remove area_live_finished, num_bathroom_calc, num_bath, and tax_property.

zdata2 = na.omit(zdata3) #Remove NAs so correlation can be calculated.
cor.zdata2 = cor(zdata2)
cor.zdata2
corrplot(cor(zdata2),type="lower", method="number")
zdata2 = zdata2[,-c(4, 8, 9, 14)] #Remove area_live_finished, num_bathroom_calc, num_bath, and tax_property.

#Convert the 2 integer variables to factor variables.
str(zdata1)
str(zdata2)

zdata1$region_county = as.factor(zdata1$region_county)
zdata1$zoning_landuse = as.factor(zdata1$zoning_landuse)

zdata2$region_county = as.factor(zdata2$region_county)
zdata2$zoning_landuse = as.factor(zdata2$zoning_landuse)

#Run linear regression for dependent variable logerror.
lm.fit.log = lm(zdata1$logerror ~ ., data = zdata1)
summary(lm.fit.log) #Adjusted R^2 is 0.004.

#Use regsubsets function to find best model.
lm.fit.log1 = regsubsets(zdata1$logerror ~ ., data = zdata1, nvmax = 10)
sum.log = summary(lm.fit.log1)
which.max(sum.log$adjr2) #Model with 10 variables has highest adjr2.
which.min(sum.log$cp) #Model with 8 variables has the lowest cp.
which.min(sum.log$bic) #Model with 6 variables has the lowest bic.

#Run linear regression for dependent variable abs_logerror.
lm.fit.abslog = lm(zdata2$abs_logerror ~ ., data = zdata2)
summary(lm.fit.abslog) #Adjusted R^2 is 0.02.

#Use regsubsets function to find best model.
lm.fit.abslog.1 = regsubsets(zdata2$abs_logerror ~ ., data = zdata2, nvmax = 10)
sum.abs = summary(lm.fit.abslog.1)
which.max(sum.abs$adjr2) #Model with 10 variables has highest adjr2.
which.min(sum.abs$cp) #Model with 10 variables has the lowest cp.
which.min(sum.abs$bic) #Model with 10 variables has the lowest bic.

#Briefly explain the results.
#For logerror as the dependent variable, depending on whether you use adjusted R^2, cp, or bic to choose the best model,
#the model with 10 variables, 8 variables, or 6 variables is the best to use respectively. For abs_logerror as the dependent
#variable, the adjusted R^2, cp, and bic all suggest that the model wtih 10 variables is the best to use.