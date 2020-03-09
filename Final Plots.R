#INITIALIZE YOUR DATA ###############################################################################
library(ggplot2)
library(meta)
library(metafor)

data = read.csv("~/Dropbox/Retinal imaging articles/RNFL thickness meta analysis/Retinal imaging meta analysis.csv")

data$Controls_mean = as.numeric(as.character(data$Controls_mean)); data$Controls_sd = as.numeric(as.character(data$Controls_sd));
data$Patient_mean = as.numeric(as.character(data$Patient_mean)); data$Patient_sd = as.numeric(as.character(data$Patient_sd));

ma = data[which(data$Group %in% c('SZ', 'BP') & data$Eye %in% c('B', 'B*')),]

#REGIONS
rnfl_o = ma[which(ma$Region=='RNFL' & ma$Location1=='overall'),]
rnfl_s = ma[which(ma$Region=='RNFL' & ma$Location2=='superior' & !ma$Location3 %in% c('nasal', 'temporal')),]
rnfl_i = ma[which(ma$Region=='RNFL' & ma$Location2=='inferior' & !ma$Location3 %in% c('nasal', 'temporal')),]
rnfl_n = ma[which(ma$Region=='RNFL' & ma$Location3=='nasal' & !ma$Location2 %in% c('superior', 'inferior')),]
rnfl_t = ma[which(ma$Region=='RNFL' & ma$Location3=='temporal' & !ma$Location2 %in% c('superior', 'inferior')),]
rnfl = rbind(rnfl_i, rnfl_o, rnfl_s, rnfl_n, rnfl_t)
mv = ma[which(ma$Region=='MV' & !ma$Study=='Polo et al 2018'),]
mt = ma[which(ma$Region=='MT' & ma$Location1=='average'),]
gcl = ma[which(ma$Region=='GCL'),]
gcl_ipl = ma[which(ma$Region=='GCL-IPL'),]
gcc = ma[which(ma$Region=='GCC' & !ma$Location2 %in% c('superior', 'inferior')),]
choroid = ma[which(ma$Region=='Choroid' & !ma$Location1=='central'),]

#THROW IT BACK TOGETHER
ma = list(rnfl_o, rnfl_s, rnfl_i, rnfl_n, rnfl_t, gcl, gcc, gcl_ipl, mv, mt, choroid)





#GRAPHICAL FIGURE: DATA########

#set up ya variables + constants 
fregions = factor();
regions = c("RNFL Overall", "RNFL Superior", "RNFL Inferior","RNFL Nasal", "RNFL Temporal", 
            "GCL", "GCC", "GCL-IPL","MV", "MT", "CT")

i = 1; j = 0; nstudies = 11;

sz = numeric(); sz_l = numeric(); sz_u = numeric(); 
bp = numeric(); bp_l = numeric(); bp_u = numeric();
sz.bp = numeric(); sz.bp_l = numeric(); sz.bp_u = numeric();

for (i in 1:nstudies){
  
  m = metacont(ma[[i]][[30]], ma[[i]][[33]], ma[[i]][[34]], 
               ma[[i]][[36]], ma[[i]][[39]], ma[[i]][[40]],
               data=ma[i], sm="SMD", method.smd="Cohen", studlab=paste(ma[[i]][[1]]),
               comb.fixed=TRUE, comb.random=TRUE, hakn=TRUE)
  
  m1 = update(m, byvar = t(m$data[[1]][19]))
  
  lsz = length(which(m$data[[1]][[19]]=='SZ'))        #number of SZ studies
  lbp = length(which(m$data[[1]][[19]]=='BP'))        #number of BP studies
  
  #fgroups = unlist(list(fgroups, meta$data[[1]][[18]]))  #adding group factorization
  
  #Add disease group SMD ONLY if there are both SZ, BP studies! 
  if (lsz > 1 & lbp > 1){

    if (!is.na(m$I2) & m$I2 > 0.5) {  #random effects
      sz.bp[i] = m$TE.random 
      sz.bp_l[i] = m$lower.random; sz.bp_u[i] = m$upper.random
    }  else {    #fixed effects
      sz.bp[i] = m$TE.fixed
      sz.bp_l[i] = m$lower.fixed; sz.bp_u[i] = m$upper.fixed
    }
  }
  
  #SCHIZOPHRENIA
  if (lsz > 0) {
    r = rep(regions[i], lsz)
    fregions = unlist(list(fregions, r))
    if (!is.na(head(m1$I2.w, n=1)) & head(m1$I2.w, n=1) > 0.5){ #RANDOM
      sz[i] = head(m1$TE.random.w, n=1)                                   
      sz_l[i] = head(m1$lower.random.w, n=1); sz_u[i] = head(m1$upper.random.w, n=1)
    }  else { 
      sz[i] = head(m1$TE.fixed.w, n=1)
      sz_l[i] = head(m1$lower.fixed.w, n=1); sz_u[i] = head(m1$upper.fixed.w, n=1)
    }                          
  } 
  
  #BIPOLAR DISORDER
  if (lbp > 0) { 
    r = rep(regions[i], lbp)
    fregions = unlist(list(fregions, r))
    if (!is.na(tail(m1$I2.w, n=1)) & tail(m1$I2.w, n=1) > 0.5){ 
      bp[i] = tail(m1$TE.random.w, n=1)  
      bp_l[i] = tail(m1$lower.random.w, n=1); bp_u[i] = tail(m1$upper.random.w, n=1)
    }  else { 
      bp[i] = tail(m1$TE.fixed.w, n=1)
      bp_l[i] = tail(m1$lower.fixed.w, n=1); bp_u[i] = tail(m1$upper.fixed.w, n=1)
    }  
  }
  
  j = j + length(m$data[[4]])
  
}

#put it all together to graph it! 
box.smd = numeric();
box.smd = append(sz, bp, after=length(sz)); box.smd = append(box.smd, sz.bp, after=length(box.smd))

box.smd_l = numeric();
box.smd_l = append(sz_l, bp_l, after=length(sz_l)); 
box.smd_l = append(box.smd_l, sz.bp_l, after=length(box.smd_l))

box.smd_u = numeric();
box.smd_u = append(sz_u, bp_u, after=length(sz_u)); 
box.smd_u = append(box.smd_u, sz.bp_u, after=length(box.smd_u)) 

box.groups = cbind(append(rep("SZ", length(sz)), rep("BD", length(bp)) , after = length(sz)))
box.groups = cbind(append(box.groups, rep("BD+SZ", length(sz.bp)), after = length(box.groups)))

#GRAPHICAL FIGURE: GGPLOT#######

overall = data.frame("smd" = box.smd, "lower" = box.smd_l, "upper" = box.smd_u,
                     "groups" = box.groups,"regions" = rep(regions, 3) )

#change order of regions
overall$regions = factor(overall$regions, levels=c("MV", "MT", "CT", "GCC", "GCL", "GCL-IPL",
                                                   "RNFL Inferior", "RNFL Superior", 
                                                   "RNFL Temporal", "RNFL Nasal", "RNFL Overall"))

#overall = overall[ !(overall$regions %in% c("MV", "MT", "CT", "GCC", "GCL", "GCL-IPL",
#                                            "RNFL Overall")), ]


#change order of diseases
overall$groups = factor(overall$groups, levels=c("BD", "SZ", "BD+SZ"))


# PLOT IT ###################################
dodge = position_dodge(0.5)

ggplot()+ 
  geom_point(data=overall, aes(x=regions, y=smd, color=factor(groups)), 
             position=dodge, size=1.6) +
  geom_errorbar(data=overall,
                aes(x=regions, ymin=lower, ymax=upper, color=factor(groups)),
                size = 0.8,
                width = 0.4, 
                position=dodge) +
  theme_minimal() + coord_flip(ylim=c(-1.5,1.5)) + 
  
  theme(legend.position = "bottom") + 
 # theme(text = element_text(size=18)) + 

  
  geom_hline(yintercept = 0, linetype="dashed") + 
  
  labs(x = "Retinal Layer",
       y = "SMD (Cohen's d)",
       color = "Groups")  +

  scale_color_manual(values=c("#3cba54","#4885ed","#F4c20d"), 
                     labels=c("BD", "SZ", "Proband"))

#Done in alphabetical order c("#3cba54","#F4c20d","#4885ed")

# #3cba54 - green
# f4c20d - yellow
# 4885ed - blue
