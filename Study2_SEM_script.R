library(lavaan)
library(readr)
library(dplyr)

ED <-  read_csv("Study2_Exercise.csv")
LD <-  read_csv("Study2_Leisure.csv")


##################################
####Exercise measurement model####
##################################

ED_CFA <-'

Inhibition  =~  1*EI_1  + EI_2 +  EI_4 
Flexibility =~ 1*EF_1  + EF_2 +  EF_3  + EF_4 + EF_5 + EM_2 +  EM_3

Inhibition ~~ Flexibility 
'

ED_CFA_Result<- cfa(ED_CFA, data=ED, estimator = "ML")
summary(ED_CFA_Result, standardized=TRUE, fit.measures=TRUE)

##########################
####Exercise SEM model####
##########################

ED_SEM <-'

Inhibition  =~  1*EI_1  + EI_2 +  EI_4 
Flexibility =~ 1*EF_1  + EF_2 +  EF_3  + EF_4 + EF_5 + EM_2 +  EM_3

Inhibition ~~ Flexibility 

SSRT_Mean  ~ Inhibition + Flexibility + Dynamic + E_history + E_duration + E_intensity 
MeanTrailRT  ~  Inhibition + Flexibility + Dynamic + E_history + E_duration + E_intensity 
SumTrailError ~ Inhibition + Flexibility + Dynamic + E_history + E_duration + E_intensity 

SSRT_Mean ~~ MeanTrailRT + SumTrailError
MeanTrailRT ~~ SumTrailError

Dynamic ~~ E_history + E_duration + E_intensity
E_history ~~ E_duration + E_intensity
E_duration ~~ E_intensity
'
ED_SEM_Result<- sem(ED_SEM, data=ED, estimator = "ML")
summary(ED_SEM_Result, standardized=TRUE,fit.measures=TRUE, ci = TRUE)


################################
###Leisure measurement model####
################################

LD_CFA <-'

Inhibition  =~  1*LI_1  + LI_2 +  LI_4 
Flexibility =~ 1*LF_1  + LF_2 +  LF_3  + LF_4 + LF_5 + LM_2 +  LM_3

Inhibition ~~ Flexibility 
'

LD_CFA_Result<- cfa(LD_CFA, data=LD, estimator = "ML")
summary(LD_CFA_Result, standardized=TRUE, fit.measures=TRUE)

#########################
####Leisure SEM model####
#########################

LD_SEM <-'

Inhibition  =~  1*LI_1  + LI_2 +  LI_4 
Flexibility =~ 1*LF_1  + LF_2 +  LF_3  + LF_4 + LF_5 + LM_2 +  LM_3

Inhibition ~~ Flexibility 

SSRT_Mean  ~ Inhibition + Flexibility + Active + L_history + L_duration + L_intensity 
MeanTrailRT  ~  Inhibition + Flexibility + Active + L_history + L_duration + L_intensity 
SumTrailError ~ Inhibition + Flexibility + Active + L_history + L_duration + L_intensity 

SSRT_Mean ~~ MeanTrailRT + SumTrailError
MeanTrailRT ~~ SumTrailError

Active ~~ L_history + L_duration + L_intensity 
L_history ~~ L_duration + L_intensity 
L_duration ~~ L_intensity
'

LD_SEM_Result<- sem(LD_SEM, data=LD, estimator = "ML")
summary(LD_SEM_Result, standardized=TRUE, fit.measures=TRUE)


