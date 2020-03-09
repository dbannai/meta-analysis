############################################################################################################
############################################################################################################
###### META REGRESSION #####################################################################################
library(meta)
library(metafor)
library(ggplot2)

data = read.csv("~/Dropbox/Retinal imaging articles/RNFL thickness meta analysis/Retinal imaging meta analysis.csv")

data$Controls_mean = as.numeric(as.character(data$Controls_mean)); data$Controls_sd = as.numeric(as.character(data$Controls_sd));
data$Patient_mean = as.numeric(as.character(data$Patient_mean)); data$Patient_sd = as.numeric(as.character(data$Patient_sd));

#for categorical meta regression covariates, use this line of code
data$Device = as.numeric(data$Device)
data$NOS.score_DB = as.integer(data$NOS.score_DB)
data$Disease.duration_mean = as.numeric(as.character(data$Disease.duration_mean))

#Narrow down the data.
ma = data[which(data$Region=='RNFL' & data$Eye %in% c('B', 'B*') 
                 & data$Location1 %in% c('overall')
             #    & data$Location2 %in% c('inferior')
                & data$Group %in% c('SZ','BP')), ]

#Run the meta-analysis.
m = metacont(Patients_Eye_n, Patient_mean, Patient_sd, Controls_Eye_n, Controls_mean, Controls_sd,
             data=ma, sm="SMD", method.smd="Cohen", studlab=paste(Study),comb.fixed=TRUE, 
             comb.random=TRUE,hakn=TRUE)


############################################################################################################
############################################################################################################
#set up your meta regression variables.
MR1 = metareg(m, NOS.score_DB + NOS.score_PL); 
#two covariates
summary(MR1)


weights = m$w.random/sum(m$w.random)*100

#Define the data frames for plotting. Use second line if plotting two sets of data.
mr = data.frame("Group" = ma$Group, "weights" = weights, "SMD" = MR1$yi.f,
                 "Covar" = ma$NOS.score_DB, "RNFL" = ma$Patient_mean,
                 "Covar_HC" = ma$Controls_Women, "RNFL_HC" = ma$Controls_mean)

############################################################################################################
############################################################################################################
#PLOT ONE DATA SET 

p = ggplot(data=mr, aes(x=Covar, y=SMD, color=factor(Group))) + 
  geom_point(size=mr$weights, alpha=I(0.5)) + 
  geom_smooth(method=lm, se=FALSE, color = "#48a0b6", size=0.5) +

  #Change x axis labels.
  #scale_x_continuous(breaks=seq(4,6,1), labels=c("Spectralis", "", "DRI Triton OCT")) + 
  
  #Label all the things! 
  labs(x = "% Women", 
       y = "SMD",
       color = "Population") +
  
  #Set ya colors! 
  scale_color_manual(values=c("#1e7c88", "#7fa6c9")) 



p


############################################################################################################
############################################################################################################
#PLOT MULTIPLE DATA SETS

#SZ+BP group
p = ggplot(data=mr, aes(x=Covar, y=RNFL, color=factor(Group)))  + 
  geom_point(size=mr$weights, alpha=I(0.5)) + 
  geom_smooth(method=lm, se=FALSE, color = "#48a0b6", size=0.5) +
  
  #Control group
  #Now add a second layer.
  geom_point(aes(x=Covar_HC, y=RNFL_HC, color="HC"), 
             size=mr$weights, alpha = I(0.6)) + 
  geom_smooth(aes(x=Covar_HC, y=RNFL_HC),
              method=lm, se=FALSE, color = "#98b08b", size=0.6) +
  
  #Change x axis labels.
  #scale_x_continuous(breaks=seq(4,6,1), labels=c("Spectralis", "", "DRI Triton OCT")) + 
  
  #Label all the things! 
  labs(x = "% Women",
       y = expression(paste("RNFL Overall Thickness (", mu,"m)")),
       color = "Population" )  +
  
  #Set ya colors! 
  scale_color_manual(values=c("#1e7c88", "#98b08b", "#7fa6c9")) #48a0b6
        #Done in alphabetical order 

p


