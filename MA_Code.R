# need to fix line 6 (it is changing the values for some reason)

############################################################################################################
#LOADING THE DATA

#POOLED DATA
data = read.csv("~/Dropbox/Retinal imaging articles/RNFL thickness meta analysis/Retinal imaging meta analysis.csv")

data$Controls_mean = as.numeric(as.character(data$Controls_mean)); 
data$Controls_sd = as.numeric(as.character(data$Controls_sd));

data$Patient_mean = as.numeric(as.character(data$Patient_mean)); 
data$Patient_sd = as.numeric(as.character(data$Patient_sd));


############################################################################################################
############################################################################################################
##### META ANALYSIS ########################################################################################

library(meta)
library(metafor)

#Narrow down for parameters/regions of interest
ma = data[which(data$Group %in% c('SZ', 'BP')
               & data$Region %in% c('GCL')
            #   & data$Location1=='outer'
               & !data$Location2 %in% c('superior', 'inferior') 
            #   & data$Location3 %in% c('temporal')
                & data$Eye %in% c('B', 'B*')),]


#META-ANALYSIS, average choroid
m = metacont(Patients_Eye_n, Patient_mean, Patient_sd, 
             Controls_Eye_n, Controls_mean, Controls_sd, 
             data=ma, sm="SMD", method.smd="Cohen", studlab=paste(Study),
             comb.fixed=TRUE, comb.random=TRUE,hakn=TRUE)


summary(m)

#forest plot, divided by disease group
par(mar=c(5,5,1,2))
forest(update(m, byvar=c(1,2), bylab="Group"), 
       digits.mean = 1L,digits.se = 1L)

#funnel plot
par(mar=c(5.5,5,2,3))
funnel(m, comb.fixed = TRUE, comb.random = TRUE, level = 0.95, pch=16,
       col.random = "#3cba54", lwd.random = 2)


#SUB GROUP ANALYSIS
m1 = update(m, byvar = ma$Group)
m2 = update(m, byvar = ma$Location2) 
m3 = update(m, byvar = ma$Location3)

m_new = metabind(m2, m3, pooled="random")     #find both fixed and random effect models
m_new
forest(m_new)




############################################################################################################
############################################################################################################
############# PLOTS ########################################################################################

#FUNNEL PLOT (PUBLICATION BIAS)
#Results that dont have findings dont get published 

#ADJUST FOR BIAS: TRIM AND FILL METHOD
tf1 <- trimfill(m, comb.fixed = FALSE, comb.random = TRUE)
summary(tf1)
funnel(tf1)
funnel(tf1, pch=ifelse(tf1$trimfill, 1, 16),
       level=0.9, comb.random=TRUE)

#LEAVE ONE OUT METHOD
metainf(m, pooled="fixed")
forest(metainf(m, pooled="fixed"))

metainf(m, pooled="random")
forest(metainf(m, pooled="random"))


############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
#GROUPS PLOT BY SPECIFIC VARIABLE
summary(update(m, byvar=c(1,2,3), bylab="xxxx"))

forest(update(m, byvar=c(1,2,3), bylab="GROUP"),
       label.right="Increase in cases", col.label.right="blue",
       label.left="Decrease in cases", col.label.left="red",
       prediction=TRUE)

#CONTRIBUTION TO HETEROGENEITY
baujat(m)
baujat(m, pos=1, xlim=c(0, 50), ylim = c(-10,40))

#RADIAL PLOT
radial(m, level=0.95)
oldpar <- par(mfrow=c(2, 2))


#FOR K>10, IS THE FUNNEL PLOT ASYMMETRIC? (IS THERE BIAS?)
metabias(m)
metabias(m, plotit=TRUE)
metabias(m, method.bias="rank")
metabias(m, method.bias="rank", correct=TRUE)
