#' Lisa's Elixhauser comobidity score calculator
#'
#' This function calculates the mortality or readmission Elixhauser comorbidity score from individual comorbidity indicator variables.
#' Input a data frame and the variable names for each of the comorbidity indicator variables.
#' This function outputs a numeric vector of scores.
#' @param df Dataframe [REQUIRED].
#' @param score Character. Name of Elixhauser score to calculate ("mortality" or "readmission"). Default is "mortality".
#' @param aids Character. Name of comorbidity indicator variable in dataframe.   
#' @param alcohol Character. Name of comorbidity indicator variable in dataframe.    
#' @param anemdef Character. Name of comorbidity indicator variable in dataframe.   
#' @param arth Character. Name of comorbidity indicator variable in dataframe.       
#' @param bldloss Character. Name of comorbidity indicator variable in dataframe.    
#' @param chf Character. Name of comorbidity indicator variable in dataframe.        
#' @param chrnlung Character. Name of comorbidity indicator variable in dataframe.   
#' @param coag Character. Name of comorbidity indicator variable in dataframe.       
#' @param depress Character. Name of comorbidity indicator variable in dataframe.    
#' @param dm Character. Name of comorbidity indicator variable in dataframe.         
#' @param dmcx Character. Name of comorbidity indicator variable in dataframe.       
#' @param drug Character. Name of comorbidity indicator variable in dataframe.       
#' @param htn_c Character. Name of comorbidity indicator variable in dataframe.      
#' @param hypothy Character. Name of comorbidity indicator variable in dataframe.    
#' @param liver Character. Name of comorbidity indicator variable in dataframe.      
#' @param lymph Character. Name of comorbidity indicator variable in dataframe.      
#' @param lytes Character. Name of comorbidity indicator variable in dataframe.      
#' @param mets Character. Name of comorbidity indicator variable in dataframe.       
#' @param neuro Character. Name of comorbidity indicator variable in dataframe.      
#' @param obese Character. Name of comorbidity indicator variable in dataframe.      
#' @param para Character. Name of comorbidity indicator variable in dataframe.       
#' @param perivasc Character. Name of comorbidity indicator variable in dataframe.   
#' @param psych Character. Name of comorbidity indicator variable in dataframe.      
#' @param pulmcirc Character. Name of comorbidity indicator variable in dataframe.   
#' @param renlfail Character. Name of comorbidity indicator variable in dataframe.   
#' @param tumor Character. Name of comorbidity indicator variable in dataframe.   
#' @param ulcer Character. Name of comorbidity indicator variable in dataframe.      
#' @param valve Character. Name of comorbidity indicator variable in dataframe.      
#' @param wghtloss  Character. Name of comorbidity indicator variable in dataframe.   
#' @keywords Elixhauser comorbidity score index  
#' @export
niceelix <- function(df
                    ,score = "mortality"
                    
                    ,aids     = "cm_aids" 
                    ,alcohol  = "cm_alcohol"   
                    ,anemdef  = "cm_anemdef"   
                    ,arth     = "cm_arth"      
                    ,bldloss  = "cm_bldloss"   
                    ,chf      = "cm_chf"       
                    ,chrnlung = "cm_chrnlung"  
                    ,coag     = "cm_coag"      
                    ,depress  = "cm_depress"   
                    ,dm       = "cm_dm"        
                    ,dmcx     = "cm_dmcx"      
                    ,drug     = "cm_drug"      
                    ,htn_c    = "cm_htn_c"     
                    ,hypothy  = "cm_hypothy"   
                    ,liver    = "cm_liver"     
                    ,lymph    = "cm_lymph"     
                    ,lytes    = "cm_lytes"     
                    ,mets     = "cm_mets"      
                    ,neuro    = "cm_neuro"     
                    ,obese    = "cm_obese"     
                    ,para     = "cm_para"      
                    ,perivasc = "cm_perivasc"  
                    ,psych    = "cm_psych"     
                    ,pulmcirc = "cm_pulmcirc"  
                    ,renlfail = "cm_renlfail"  
                    ,tumor    = "cm_tumor"     
                    ,ulcer    = "cm_ulcer"     
                    ,valve    = "cm_valve"     
                    ,wghtloss = "cm_wghtloss" 
                    ){
      
      # Elixhauser readmission weights for calculating scores             
      assign(paste("erw", aids      , sep="_"),   19) 
      assign(paste("erw", alcohol   , sep="_"),    6) 
      assign(paste("erw", anemdef   , sep="_"),    9) 
      assign(paste("erw", arth      , sep="_"),    4) 
      assign(paste("erw", bldloss   , sep="_"),    3) 
      assign(paste("erw", chf       , sep="_"),   13) 
      assign(paste("erw", chrnlung  , sep="_"),    8) 
      assign(paste("erw", coag      , sep="_"),    7) 
      assign(paste("erw", depress   , sep="_"),    4) 
      assign(paste("erw", dm        , sep="_"),    6) 
      assign(paste("erw", dmcx      , sep="_"),    9) 
      assign(paste("erw", drug      , sep="_"),   14) 
      assign(paste("erw", htn_c     , sep="_"),   -1) 
      assign(paste("erw", hypothy   , sep="_"),    0) 
      assign(paste("erw", liver     , sep="_"),   10) 
      assign(paste("erw", lymph     , sep="_"),   16) 
      assign(paste("erw", lytes     , sep="_"),    8) 
      assign(paste("erw", mets      , sep="_"),   21) 
      assign(paste("erw", neuro     , sep="_"),    7) 
      assign(paste("erw", obese     , sep="_"),   -3) 
      assign(paste("erw", para      , sep="_"),    6) 
      assign(paste("erw", perivasc  , sep="_"),    4) 
      assign(paste("erw", psych     , sep="_"),   10) 
      assign(paste("erw", pulmcirc  , sep="_"),    5) 
      assign(paste("erw", renlfail  , sep="_"),   15) 
      assign(paste("erw", tumor     , sep="_"),   15) 
      assign(paste("erw", ulcer     , sep="_"),    0) 
      assign(paste("erw", valve     , sep="_"),    0) 
      assign(paste("erw", wghtloss  , sep="_"),   10) 
            
      # Elixhauser mortality weights for calculating scores    
      assign(paste("emw", aids      , sep="_"),    0) 
      assign(paste("emw", alcohol   , sep="_"),   -1) 
      assign(paste("emw", anemdef   , sep="_"),   -2) 
      assign(paste("emw", arth      , sep="_"),    0) 
      assign(paste("emw", bldloss   , sep="_"),   -3) 
      assign(paste("emw", chf       , sep="_"),    9) 
      assign(paste("emw", chrnlung  , sep="_"),    3) 
      assign(paste("emw", coag      , sep="_"),   11) 
      assign(paste("emw", depress   , sep="_"),   -5) 
      assign(paste("emw", dm        , sep="_"),    0) 
      assign(paste("emw", dmcx      , sep="_"),   -3) 
      assign(paste("emw", drug      , sep="_"),   -7) 
      assign(paste("emw", htn_c     , sep="_"),   -1) 
      assign(paste("emw", hypothy   , sep="_"),    0) 
      assign(paste("emw", liver     , sep="_"),    4) 
      assign(paste("emw", lymph     , sep="_"),    6) 
      assign(paste("emw", lytes     , sep="_"),   11) 
      assign(paste("emw", mets      , sep="_"),   14) 
      assign(paste("emw", neuro     , sep="_"),    5) 
      assign(paste("emw", obese     , sep="_"),   -5) 
      assign(paste("emw", para      , sep="_"),    5) 
      assign(paste("emw", perivasc  , sep="_"),    3) 
      assign(paste("emw", psych     , sep="_"),   -5) 
      assign(paste("emw", pulmcirc  , sep="_"),    6) 
      assign(paste("emw", renlfail  , sep="_"),    6) 
      assign(paste("emw", tumor     , sep="_"),    7) 
      assign(paste("emw", ulcer     , sep="_"),    0) 
      assign(paste("emw", valve     , sep="_"),    0) 
      assign(paste("emw", wghtloss  , sep="_"),    9) 
      
      evars <- c(aids    
                ,alcohol 
                ,anemdef 
                ,arth    
                ,bldloss 
                ,chf     
                ,chrnlung
                ,coag    
                ,depress 
                ,dm      
                ,dmcx    
                ,drug    
                ,htn_c   
                ,hypothy 
                ,liver   
                ,lymph   
                ,lytes   
                ,mets    
                ,neuro   
                ,obese   
                ,para    
                ,perivasc
                ,psych   
                ,pulmcirc
                ,renlfail
                ,tumor   
                ,ulcer   
                ,valve   
                ,wghtloss
                )
      
      emat <- matrix(NA, ncol = length(evars), nrow = nrow(df)) 
      if (score == "readmission"){
            for (i in 1:length(evars)) emat[,i] <- df[,evars[i]]*eval(as.symbol(paste("erw",evars[i], sep="_")))
            myscore <- rowSums(emat)
      }
      if (score == "mortality"){
            for (i in 1:length(evars)) emat[,i] <- df[,evars[i]]*eval(as.symbol(paste("emw",evars[i], sep="_")))
            myscore <- rowSums(emat)
      }
      
      return(myscore)
} 
