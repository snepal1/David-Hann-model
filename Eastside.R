rm(list=ls())
################################### Load in libraries ###########################################################
library(dplyr)
#library(tidyverse)
#library(minpack.lm)
library(nlme)
#library(lme4)
###################### Bring in all the necessary data ######################################################################
data_merch<-read.csv("D:\\Height_diameter\\Diameter_Merch_height\\Training.csv")
summary(data_merch)
#data_merch<-read.csv("D:\\Height_diameter\\Clean_plots\\Merch_clean1.csv")
#summary(data_merch)
data_height<-read.csv("D:\\Height_diameter\\Clean_plots1\\Clean2_round2.csv")
summary(data_height)

database<-read.csv("D:\\Height_diameter\\Diameter_Merch_height\\test.csv")

############################ Initialize the function for non-linear model ################################

model<-function(data) {
  ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                      minFactor =1/2048,
                      printEval = T)
  
  nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                       #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                       start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                       fixed=b1+b2+b3~1,random=u1~1|elev,
                       data=data,
                       control=ctrl)
  return(nls17_Height)
  
}

################################## Function to manipulate the data ##########################################

data<-function(data){
  clean_pp_Height<-subset(data_height,Tree_Speci=="PP"& Region=="Eastside"& dbh>=6.5 & ratio>=2.6
                          &dbh<=50)
  #plot(clean_pp_Height$height~clean_pp_Height$dbh)
  clean_pp_Height$elev<- round(clean_pp_Height$elev/100)*100
  
  clean_pp_Height$elev[clean_pp_Height$elev>6600] <- "6700"
  
  return(clean_pp_Height)
}

data_clean<-data(data_height)
summary(data_clean)
################################################################################################################

############### Subset the database############################################################################
data<-subset(database,Species=="PP")

###################### Appply the model function to the cleaned data ##########################################
my_model<-model(data_clean)
summary(my_model)
coef(my_model)
height<-predict(my_model,data)
data$height<-height
str(data)
str(data)
summary(data)

########## Using the merch height modelling for ponderosa pine SSierra#################################################

data3a<-subset(data_merch,Tree_Speci=="PP" & Region=="Eastside"  & Merch_Heig>=1 &
                 dbh>=6.5 & Ratio>=3.5 & dbh<60 & height<=200 & M.ratio > 0 &
                 M.ratio<0.99)

data3<-data3a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

data3$elev<- round(data3$elev/100)*100
summary(data3)
#data3$elev[data3$elev>7200] <- 7300
#data3$elev[data3$elev<2300] <- 2200

model<-lm(Merch_Heig~height, data=data3)
summary(model)
plot(model)

Merch<-predict(model,data)
data$Merch<-Merch
write.csv(data,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_PP_lm.csv")
#####################################################################################

######################### For Douglas fir####################################

############### Subset the database#############################
data5<-subset(database,Species=="DF")
################################################
########################################### douglas fir cleaning#################

data_df<-function(data){
  clean_DF_Height<-subset(data_height,Tree_Speci=="DF"& Region=="Eastside")
  clean_DF_Height$elev<- round(clean_DF_Height$elev/100)*100
  clean_DF_Height$elev[clean_DF_Height$elev<5200] <- "5100"
  clean_DF_Height$elev[clean_DF_Height$elev>6000] <- "6100"
  return(clean_DF_Height)
}

data_clean_df<-data_df(data_height)

################################## apply the model function #######################################

my_model_df<-model(data_clean_df)

summary(my_model_df)
coef(my_model_df)
height<-predict(my_model_df,data5)
data5$height<-height
str(data5)
str(data5)
summary(data5)

################## David Hann model for Height douglas fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_DF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data5)
data5$height<-height

##########cleaning for douglas fir SSierra####################################################

#data4a<-subset(data_merch,Tree_Speci=="DF"& Region=="Eastside" & Merch_Heig>=1 &
                 #dbh>=6.5 & dbh<60 & height<=200)
#summary(data4a)
#data_df<-data4a %>%
  #group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  #summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            #elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

#data_df$elev<- round(data_df$elev/100)*100
#summary(data_df)
#data_df$elev[data_df$elev>7200] <- 7300
#data_df$elev[data_df$elev<2300] <- 2200

#aggregate(dbh ~ elev, data = data_df, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                          #count=length(x)))

#plot(data_df$Merch_Heig~data_df$height)

#model_df<-lm(Merch_Heig~height, data=data_df)
#summary(model_df)
#plot(model_df)

Merch<-predict(model,data5)
data5$Merch<-Merch
write.csv(data5,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_DF_lm.csv")
#####################################################################################

############################ For white fir#####################################################
########################################################################

############### Subset the database#############################
data6<-subset(database,Species=="WF")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_WF_Height<-subset(data_height,Tree_Speci=="WF"& Region=="Eastside"&dbh>=6.5&ratio>=2.5)

plot(clean_WF_Height$height~clean_WF_Height$dbh)
clean_WF_Height$elev<- round(clean_WF_Height$elev/100)*100
summary(clean_WF_Height)

################################# elevation for whitefir ###############
#clean_WF_Height$elev[clean_WF_Height$elev < 3400] <- "3300"			

################################################################
#clean_WF_Height$elev[clean_WF_Height$elev >6900] <- "7000"
aggregate(dbh ~ elev, data = clean_WF_Height, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                                  count=length(x)))
#########################################################################


################## David Hann model for Height White fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_WF_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data6)
data6$height<-height

##########cleaning for wite fir SSierra####################################################

data5a<-subset(data_merch,Tree_Speci=="WF"& Region=="Eastside"  & Merch_Heig>=1 &
                 dbh>=6.5 & Ratio>=3.5 & dbh<60 & height<=200)
summary(data5a)
data_wf<-data5a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

data_wf$elev<- round(data_wf$elev/100)*100
summary(data_wf)
#data_wf$elev[data_wf$elev>7200] <- 7300
#data_wf$elev[data_wf$elev<2300] <- 2200

#aggregate(dbh ~ elev, data = data_wf, FUN = function(x) c(mean = mean(x), sd = sd(x), 
#count=length(x)))

plot(data_wf$Merch_Heig~data_wf$height)

model_wf<-lm(Merch_Heig~height, data=data_wf)
summary(model_wf)
plot(model_wf)

Merch<-predict(model_wf,data6)
data6$Merch<-Merch
write.csv(data6,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_WF_lm.csv")
#####################################################################################
############################ For Incense_cedar#####################################################
########################################################################

############### Subset the database#############################
data7<-subset(database,Species=="IC")
################################################
##########cleaning and subsetting height data for###############
clean_IC_Height<-subset(data_height,Tree_Speci=="IC"& Region=="Eastside"  &dbh>=6.5&ratio>=2.2)

plot(clean_IC_Height$height~clean_IC_Height$dbh)
clean_IC_Height$elev<- round(clean_IC_Height$elev/100)*100
summary(clean_IC_Height)

################################# elevation for whitefir ###############
#clean_WF_Height$elev[clean_WF_Height$elev < 3400] <- "3300"			

################################################################
clean_IC_Height$elev[clean_IC_Height$elev >6300] <- "6400"
aggregate(dbh ~ elev, data = clean_IC_Height, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                                  count=length(x)))
#########################################################################


################## David Hann model ####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_IC_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data7)
data7$height<-height

##########cleaning for ####################################################

data6a<-subset(data_merch,Tree_Speci=="IC"& Region=="Eastside"  & Merch_Heig>=1 &
                 dbh>=6.5 & Ratio>=3.5 & dbh<60 & height<=200)
summary(data6a)
data_ic<-data6a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

data_ic$elev<- round(data_ic$elev/100)*100
summary(data_ic)
#data_wf$elev[data_wf$elev>7200] <- 7300
#data_wf$elev[data_wf$elev<2300] <- 2200

aggregate(dbh ~ elev, data = data_ic, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                          count=length(x)))

plot(data_ic$Merch_Heig~data_ic$height)

model_ic<-lm(Merch_Heig~height, data=data_ic)
summary(model_ic)
plot(model_ic)

Merch<-predict(model_ic,data7)
data7$Merch<-Merch
write.csv(data7,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_IC_lm.csv")
#####################################################################################
#####################################################################################
############################ For Si=ugar pine#####################################################
########################################################################

############### Subset the database#############################
data8<-subset(database,Species=="SP")
################################################
##########cleaning and subsetting height data for ponderosa pine SSeirra###############
clean_SP_Height<-subset(data_height,Tree_Speci=="SP"& Region=="Eastside"&dbh>=6.5)

plot(clean_SP_Height$height~clean_SP_Height$dbh)
clean_SP_Height$elev<- round(clean_SP_Height$elev/100)*100
summary(clean_SP_Height)

################################# elevation for whitefir ###############
#clean_WF_Height$elev[clean_WF_Height$elev < 3400] <- "3300"			

################################################################
clean_SP_Height$elev[clean_SP_Height$elev <4400] <- "4300"
aggregate(dbh ~ elev, data = clean_SP_Height, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                                  count=length(x)))
#########################################################################


################## David Hann model for Height White fir####################################

ctrl <- nls.control(maxiter =150,tol=1.49e-08,
                    minFactor =1/2048,
                    printEval = T)


nls17_Height <- nlme(height ~ 4.5 + exp((b1+u1)+ b2*(dbh^b3)),
                     #start = c(b1 =6.58172, b2 =-7.58311,b3=-0.41703),#df,wf,PP
                     start = c(b1 =6.58172  , b2 =-7.58311 ,b3=-0.41703),# WF_NSIERRA
                     fixed=b1+b2+b3~1,random=u1~1|elev,
                     data=clean_SP_Height,
                     control=ctrl)

summary(nls17_Height)
coef(nls17_Height)
height<-predict(nls17_Height,data8)
data8$height<-height

##########cleaning for wite fir SSierra####################################################

data7a<-subset(data_merch,Tree_Speci=="SP"& Region=="Eastside"  & Merch_Heig>=1 &
                 dbh>=6.5 & Ratio>=3.5 & dbh<60 & height<=200)
summary(data7a)
data_sp<-data7a %>%
  group_by(Region,PLOT_ID,Tree_Numbe,Tree_Speci,Name)%>%
  summarise(dbh=mean(dbh), height=mean(height), ratio=mean(Ratio), 
            elev=mean(elev),  Merch_Heig=mean(Merch_Heig))

data_sp$elev<- round(data_sp$elev/100)*100
summary(data_sp)
#data_wf$elev[data_wf$elev>7200] <- 7300
#data_wf$elev[data_wf$elev<2300] <- 2200

aggregate(dbh ~ elev, data = data_sp, FUN = function(x) c(mean = mean(x), sd = sd(x), 
                                                          count=length(x)))

plot(data_sp$Merch_Heig~data_sp$height)

model_sp<-lm(Merch_Heig~height, data=data_sp)
summary(model_sp)
plot(model_sp)

Merch<-predict(model_sp,data8)
data8$Merch<-Merch
write.csv(data8,file="D:\\Height_diameter\\Diameter_Merch_height\\Merch_Height_SP_lm.csv")
#####################################################################################

