library(readr)
library(lavaan)
library(dplyr)

ED <-  read_csv("Study1_Exercise.csv")
LD <-  read_csv("Study1_Leisure.csv")

###################################
####Exercise measurement models####
###################################

ED_CFA1 <-'

Inhibition    =~ 1*EI_1 + EI_2r +  
                   EI_3 + EI_4r + 
                   EI_5 + EI_6r + 
                   EI_7 + EI_8r + 
                   EI_9 + EI_10r

Flexibility   =~ 1*EF_1 + EF_2r +  
                   EF_3 + EF_4r + 
                   EF_5 + EF_6r + 
                   EF_7 + EF_8r + 
                   EF_9 + EF_10r

Memory        =~ 1*EM_1 + EM_2r + 
                   EM_3 + EM_4r + 
                   EM_5 + EM_6r + 
                   EM_7 + EM_8r + 
                   EM_9 + EM_10r
'

ED_CFA1_Result<- cfa(ED_CFA1, data=ED, estimator = "ML")
summary(ED_CFA1_Result, standardized=TRUE,fit.measures=TRUE)

ED_CFA2 <-'

Inhibition    =~ 1*EI_1 + EI_3 +  
                   EI_5 + EI_7 + 
                   EI_9 

Flexibility   =~ 1*EF_1 + EF_3 +
                   EF_5 + EF_7 + 
                   EF_9 

Memory        =~ 1*EM_1 + EM_3 +
                   EM_5 + EM_7 +
                   EM_9 
'

ED_CFA2_Result<- cfa(ED_CFA2, data=ED, estimator = "ML")
summary(ED_CFA2_Result, standardized=TRUE,fit.measures=TRUE)

ED_CFA3 <-'

Inhibition    =~ 1*EI_1 + EI_3 +  
                   EI_7 

Flexibility   =~ 1*EF_1 + EF_3 +
                   EF_5 + EF_7 + 
                   EF_9 + EM_3 + 
                   EM_5 
'

ED_CFA3_Result<- cfa(ED_CFA3, data=ED, estimator = "ML", )
summary(ED_CFA3_Result, standardized=TRUE, fit.measures=TRUE)


##########################
####Exercise SEM model####
##########################

ED_SEM <-'

Inhibition    =~ 1*EI_1 + EI_3 +  
                   EI_7 

Flexibility   =~ 1*EF_1 + EF_3 +
                   EF_5 + EF_7 + 
                   EF_9 + EM_3 + 
                   EM_5 
  
WM.Capacity      ~ Inhibition + Flexibility + Dynamic + 
                E_history + E_duration + E_intensity    
               
Flanker.Inhibition.Acc         ~ Inhibition + Flexibility + Dynamic + 
                E_history + E_duration + E_intensity     
               
Flanker.Inhibition.RT          ~ Inhibition + Flexibility + Dynamic + 
                E_history + E_duration + E_intensity    

Inhibition    ~~ Flexibility

WM.Capacity      ~~ Flanker.Inhibition.Acc  + Flanker.Inhibition.RT
Flanker.Inhibition.Acc        ~~ Flanker.Inhibition.RT

Dynamic       ~~ E_history + E_duration + E_intensity 
E_history     ~~ E_duration + E_intensity
E_duration    ~~ E_intensity
'

ED_SEM_Result<- sem(ED_SEM, data=ED, estimator = "ML")
summary(ED_SEM_Result, fit.measures=TRUE, standardized=TRUE, ci = TRUE)

#################################
####Leisure measurement model####
#################################

LD_CFA <-'

Inhibition    =~ 1*LI_1 + LI_3 +  
                   LI_7 

Flexibility   =~ 1*LF_1 + LF_3 +
                   LF_5 + LF_7 + 
                   LF_9 + LM_3 + 
                   LM_5 
'

LD_CFA_Result<- cfa(LD_CFA, data=LD, estimator = "ML", missing = "fiml")
summary(LD_CFA_Result, standardized=TRUE,fit.measures=TRUE)


#########################
####Leisure SEM model####
#########################

LD_SEM <-'

Inhibition    =~ 1*LI_1 + LI_3 +  
                   LI_7 

Flexibility   =~ 1*LF_1 + LF_3 +
                   LF_5 + LF_7 + 
                   LF_9 + LM_3 + 
                   LM_5 
  
WM.Capacity      ~ Inhibition + Flexibility + Active + 
                L_history + L_duration + L_intensity    
               
Flanker.Inhibition.Acc         ~ Inhibition + Flexibility + Active + 
                L_history + L_duration + L_intensity     
               
Flanker.Inhibition.RT          ~ Inhibition + Flexibility + Active + 
                L_history + L_duration + L_intensity    

Inhibition    ~~ Flexibility

WM.Capacity      ~~ Flanker.Inhibition.Acc + Flanker.Inhibition.RT
Flanker.Inhibition.Acc         ~~ Flanker.Inhibition.RT

Active        ~~ L_history + L_duration + L_intensity 
L_history     ~~ L_duration + L_intensity
L_duration    ~~ L_intensity
'

LD_SEM_Result<- sem(LD_SEM, data=LD, estimator = "ML", missing = "fiml")
summary(LD_SEM_Result, standardized=TRUE, ci = TRUE, fit.measures=TRUE)

