library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(remotes)
library(dsHelper)

#install_github("datashield/dsMediationClient") #NB: newest version is now v0.0.3-dev! (Demetris at LC GA 19-05-22)
library(dsMediationClient)


#DSI::datashield.logout(connections)


#################################### THE ANALYSIS ################################

#Model inputs:
data_table = "D2"
outcome = 'poisson_medall'
exposure = 'bin_edu_m'
mediator0="bin_edu_m0"
mediator1="bin_edu_m1"

#Preg_smk
mediator = 'preg_smk'
name = 'smk'

#Add adverse reproductive outcomes
mediator = 'preg_smk*adverse_rep_outcomes'
name='ad_rep'

#Add breastfeeding
mediator = 'preg_smk * adverse_rep_outcomes * breastfedcat'
name='bf'

#Add ETS
mediator = 'preg_smk * adverse_rep_outcomes * breastfedcat * passivesmoke2y'
name='ets'


for (i in c(1,3,5)) {
  if(i==1){
    covariates=c("asthma_m", "agebirth_m_y", "matagesqu",  "asthma_bf") #DNBC
  }else if (i == 3){
    covariates=c("asthma_m", "agebirth_m_y",  "asthma_bf") #ALSPAC
  }else if (i==6) {
    covariates=c("asthma_m", "agebirth_m_y", "matagesqu", "ethn3_m",  "asthma_bf", "parity2") #Gen R
  }else {
    covariates=c("asthma_m", "agebirth_m_y",  "asthma_bf") #MoBa and EDEN
  }
 # NOTE: PARITY AND ETHNICITY REMOVED FROM SOME COHORTS 
  #--------------------------------------------------------------------------
  
  #Build formula:
  to_eval = paste0("fmla1_",i," <- as.formula(paste(outcome,' ~ ', paste0(c(exposure, mediator, 
                                                                  covariates), collapse= '+')))")
  eval(parse(text=to_eval))
  
  
  #Fit working model
  to_eval = paste0("impFit.DS_",name,"_",i," <- ds.glmSLMA(formula = fmla1_",i,", 
                  family = 'poisson', dataName = data_table, newobj ='impFit.DS',
                        combine.with.metafor = FALSE, datasources = connections[",i,"])")
  eval(parse(text=to_eval))
  
  #--------------------------------------------------------------------------
  
  #Impute the nested counterfactual outcomes
  
  eval(parse(text=paste0("ds.neImpute(object = 'impFit.DS', newobj = 'expData', datasources = connections[",i,"])")))
  #check the names of the two imputed counterfactual outcomes
  x<-eval(parse(text=paste0("ds.colnames('expData', datasources = connections[",i,"])")))
  print(x)
  
  #--------------------------------------------------------------------------
  
  #Fit natural effect model to the imputed dataset
  
  
  to_eval = paste0("fmla2_",i," <- as.formula(paste(outcome,' ~ ', paste0(c(mediator0, mediator1, 
                                                                  covariates), collapse= '+')))")
  
  eval(parse(text=to_eval))
  to_eval = paste0("neMod.DS_",name,"_",i," <- ds.neModel(formula = fmla2_",i,", 
                  family = 'poisson', expData ='expData', newobj ='neMod.DS',
                        datasources = connections[",i,"])")
  eval(parse(text=to_eval))
  
  #--------------------------------------------------------------------------
  
  #Obtain effect estimates
  
  to_eval = paste0("effdecomp_",name,"_",i," <- ds.neEffdecomp(model = 'neMod.DS', 
                        datasources = connections[",i,"])")
  eval(parse(text=to_eval))
  
  
}

#Plots:-------------------------------------------------------------------------

#Graphs by cohort:--------------------------------------------------------------

#Divide the screen in 1 line and 4 columns
par(
  mfrow=c(1,3), 
  oma = c(0, 0, 2, 0)
) 

#ALSPAC

yi_alspac = c(effdecomp_smk_3$alspac$coefficients[1,1], effdecomp_smk_3$alspac$coefficients[2,1], effdecomp_smk_3$alspac$coefficients[3,1],
           effdecomp_ad_rep_3$alspac$coefficients[1,1], effdecomp_ad_rep_3$alspac$coefficients[2,1], effdecomp_ad_rep_3$alspac$coefficients[3,1],
           effdecomp_bf_3$alspac$coefficients[1,1], effdecomp_bf_3$alspac$coefficients[2,1], effdecomp_bf_3$alspac$coefficients[3,1],
           effdecomp_ets_3$alspac$coefficients[1,1], effdecomp_ets_3$alspac$coefficients[2,1], effdecomp_ets_3$alspac$coefficients[3,1]) 
sei_alspac = c(effdecomp_smk_3$alspac$coefficients[1,2], effdecomp_smk_3$alspac$coefficients[2,2], effdecomp_smk_3$alspac$coefficients[3,2],
            effdecomp_ad_rep_3$alspac$coefficients[1,2], effdecomp_ad_rep_3$alspac$coefficients[2,2], effdecomp_ad_rep_3$alspac$coefficients[3,2],
            effdecomp_bf_3$alspac$coefficients[1,2], effdecomp_bf_3$alspac$coefficients[2,2], effdecomp_bf_3$alspac$coefficients[3,2],
            effdecomp_ets_3$alspac$coefficients[1,2], effdecomp_ets_3$alspac$coefficients[2,2], effdecomp_ets_3$alspac$coefficients[3,2])

forest(yi_alspac, sei = sei_alspac, atransf=exp,)

#DNBC
effdecomp_smk_1 #preg_smk
effdecomp_ad_rep_1 # adverse reproductive outcomes
effdecomp_bf_1 #  breastfeeding
effdecomp_ets_1 # ETS

yi_dnbc = c(effdecomp_smk_1$dnbc$coefficients[1,1], effdecomp_smk_1$dnbc$coefficients[2,1], effdecomp_smk_1$dnbc$coefficients[3,1],
          effdecomp_ad_rep_1$dnbc$coefficients[1,1], effdecomp_ad_rep_1$dnbc$coefficients[2,1], effdecomp_ad_rep_1$dnbc$coefficients[3,1],
          effdecomp_bf_1$dnbc$coefficients[1,1], effdecomp_bf_1$dnbc$coefficients[2,1], effdecomp_bf_1$dnbc$coefficients[3,1],
          effdecomp_ets_1$dnbc$coefficients[1,1], effdecomp_ets_1$dnbc$coefficients[2,1], effdecomp_ets_1$dnbc$coefficients[3,1])
sei_dnbc = c(effdecomp_smk_1$dnbc$coefficients[1,2], effdecomp_smk_1$dnbc$coefficients[2,2], effdecomp_smk_1$dnbc$coefficients[3,2],
            effdecomp_ad_rep_1$dnbc$coefficients[1,2], effdecomp_ad_rep_1$dnbc$coefficients[2,2], effdecomp_ad_rep_1$dnbc$coefficients[3,2],
            effdecomp_bf_1$dnbc$coefficients[1,2], effdecomp_bf_1$dnbc$coefficients[2,2], effdecomp_bf_1$dnbc$coefficients[3,2],
            effdecomp_ets_1$dnbc$coefficients[1,2], effdecomp_ets_1$dnbc$coefficients[2,2], effdecomp_ets_1$dnbc$coefficients[3,2])

forest(yi_dnbc, sei = sei_dnbc, atransf=exp,)


#EDEN

yi_eden = c(effdecomp_smk_5$eden$coefficients[1,1], effdecomp_smk_5$eden$coefficients[2,1], effdecomp_smk_5$eden$coefficients[3,1],
           effdecomp_ad_rep_5$eden$coefficients[1,1], effdecomp_ad_rep_5$eden$coefficients[2,1], effdecomp_ad_rep_5$eden$coefficients[3,1],
           effdecomp_bf_5$eden$coefficients[1,1], effdecomp_bf_5$eden$coefficients[2,1], effdecomp_bf_5$eden$coefficients[3,1],
           effdecomp_ets_5$eden$coefficients[1,1], effdecomp_ets_5$eden$coefficients[2,1], effdecomp_ets_5$eden$coefficients[3,1]) 
sei_eden = c(effdecomp_smk_5$eden$coefficients[1,2], effdecomp_smk_5$eden$coefficients[2,2], effdecomp_smk_5$eden$coefficients[3,2],
            effdecomp_ad_rep_5$eden$coefficients[1,2], effdecomp_ad_rep_5$eden$coefficients[2,2], effdecomp_ad_rep_5$eden$coefficients[3,2],
            effdecomp_bf_5$eden$coefficients[1,2], effdecomp_bf_5$eden$coefficients[2,2], effdecomp_bf_5$eden$coefficients[3,2],
            effdecomp_ets_5$eden$coefficients[1,2], effdecomp_ets_5$eden$coefficients[2,2], effdecomp_ets_5$eden$coefficients[3,2])

forest(yi_eden, sei = sei_eden, atransf=exp,)


#####################Plot graphs################################################

effects <- data.frame(cbind(
  c("NDE", "NIE", "TE", "NDE", "NIE", "TE", 
    "NDE", "NIE", "TE", "NDE", "NIE", "TE")))
colnames(effects) = c('Measure')


setwd("~/LC_resp_inequal/Graphs")

png(
  file = "mediation_mat_ed_model_vers2.png", 
  width = 24, 
  height = 10.25, 
  units = "cm",
  res = 500)

par(
  mfrow=c(1,3), 
  oma = c(0, 0, 2, 0)
) 

#par()              # view current settings
     # make a copy of current settings

#opar <- par() 
forest(yi_alspac, sei=sei_alspac, psize = 1, ylim=c(0, 17), xlim = c(-1,1.75),
       rows=c(14:12, 10:8, 6:4, 2:0),  at=log(c(0.80, 1, 3.0)), atransf=exp, showweights=FALSE,
       xlab="Risk Ratio", refline=log(1), slab = NA,
       ilab=effects$Measure, ilab.xpos=-0.15, ilab.pos=2, mlab="ALSPAC")

#Add labels:
par(cex=0.65, font=2)
text(c(0),     16.5, c("ALSPAC"))
par(cex=0.6, font=2)
text(1.25, 15.5, "Risk ratio [95% CI]")
#par(cex=0.6, font=2)
text(-1, 15.5, "Mediator", pos=4)
#Add smoking label:
par(cex=0.5, font=2)
text(-1, 13.75, "Smoking", pos=4)
text(-1, 13, "during", pos=4)
text(-1, 12.25, "pregnancy", pos=4)
#Add adverse reproductive outcome label:
text(-1, 9.75, "Adverse", pos=4)
text(-1, 9, "reproductive", pos=4)
text(-1, 8.25, "outcomes", pos=4)
#Breastfeeding label:
text(-1, 5.3725, "Breastfeeding", pos=4)
text(-1, 4.6275, "duration", pos=4)
#ETS:
text(-1, 1, "ETS", pos=4)

par(cex=0.66, font=1) 

#par(op)

forest(yi_dnbc, sei=sei_dnbc, psize = 1, ylim=c(0, 17), xlim = c(-1,1.75),
       rows=c(14:12, 10:8, 6:4, 2:0),  at=log(c(0.80, 1, 3.0)), atransf=exp, showweights=FALSE,
       xlab="Risk Ratio", refline=log(1), slab = NA,
       ilab=effects$Measure, ilab.xpos=-0.15, ilab.pos=2, mlab="DNBC")

#Add labels:
par(cex=0.65, font=2)
text(c(0),     16.5, c("DNBC"))
par(cex=0.6, font=2)
text(1.25, 15.5, "Risk ratio [95% CI]")
#par(cex=0.6, font=2)
text(-1, 15.5, "Mediator", pos=4)
#Add smoking label:
par(cex=0.5, font=2)
text(-1, 13.75, "Smoking", pos=4)
text(-1, 13, "during", pos=4)
text(-1, 12.25, "pregnancy", pos=4)
#Add adverse reproductive outcome label:
text(-1, 9.75, "Adverse", pos=4)
text(-1, 9, "reproductive", pos=4)
text(-1, 8.25, "outcomes", pos=4)
#Breastfeeding label:
text(-1, 5.3725, "Breastfeeding", pos=4)
text(-1, 4.6275, "duration", pos=4)
#ETS:
text(-1, 1, "ETS", pos=4)


par(cex=0.66, font=1) 

forest(yi_eden, sei=sei_eden, psize = 1, ylim=c(0, 17), xlim = c(-1,1.75),
       rows=c(14:12, 10:8, 6:4, 2:0),  at=log(c(0.80, 1, 3.0)), atransf=exp, showweights=FALSE,
       xlab="Risk Ratio", refline=log(1), slab = NA,
       ilab=effects$Measure, ilab.xpos=-0.15, ilab.pos=2, mlab="EDEN")

#Add labels:
par(cex=0.65, font=2)
text(c(0),     16.5, c("EDEN"))
par(cex=0.6, font=2)
text(1.25, 15.5, "Risk ratio [95% CI]")
#par(cex=0.6, font=2)
text(-1, 15.5, "Mediator", pos=4)
#Add smoking label:
par(cex=0.5, font=2)
text(-1, 13.75, "Smoking", pos=4)
text(-1, 13, "during", pos=4)
text(-1, 12.25, "pregnancy", pos=4)
#Add adverse reproductive outcome label:
text(-1, 9.75, "Adverse", pos=4)
text(-1, 9, "reproductive", pos=4)
text(-1, 8.25, "outcomes", pos=4)
#Breastfeeding label:
text(-1, 5.3725, "Breastfeeding", pos=4)
text(-1, 4.6275, "duration", pos=4)
#ETS:
text(-1, 1, "ETS", pos=4)





dev.off()

# Graphs by mediator:-----------------------------------------------------------

# See income mediation file