
library(dplyr)
library(flextable)
library(knitr)


library(Boruta) #Boruta

library(caret) # VarImp
library(RRF) # VarImp
library(rpart) # VarImp

library(glmnet) # Lasso

library(relaimpo) #Relative Importance

library(DALEX) #Dalex
library(randomForest) #Dalex

fun_corr<-function(corval,ind_var){
  #https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
  #https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
  tryCatch(
    { 
      
      varcor<-paste0(ind_var,"_cor")
      cat("  \n#### CORRELATION: Variables vs DIAGNOSIS \n")
      cat("  \n")      
      cat("  \nAbsolute Value greater than 0.7  \n")
      cat("  \n")
      
      dtx<-corval[  !is.na(corval[[varcor]]) &
                      ( corval[[varcor]]<=-0.7 | 
                          corval[[varcor]]>=0.7)
                    ,c("var.x",varcor)]
      cat("  \n")
      cat("  \n##### Correlation (top 10)  \n")
      cat("  \n")
      print(kable(head(dtx,10)))
      cat("  \n")
      cat("  \nAll Variables selected are shown in Summary Section  \n")
      cat("  \n")   
      
      features<-dtx[[varcor]]
      names(features)<-dtx$var.x
      list_featselalg[["Correlation"]]<<-features
      
      
      cat("  \n")
      cat("  \n#### CORRELATION: Variables versus DEPRESSION.  \n")
      cat("  \n")
      cat("  \nAbsolute Value greater than 0.7  \n")
      cat("  \n")
      
      dtx<-corval[!is.na(corval$depression_cor) & (
        corval$depression_cor<=-0.4 | 
          corval$depression_cor>=0.4)
        ,c("var.x","depression_cor")]
      cat("  \n")
      print(kable(dtx))
      cat("  \n")
      
      
      cat("  \n")
      cat("  \n#### CORRELATION: Variables versus HYPOMANIA  \n")
      cat("  \n")
      cat("  \nAbsolute Value greater than 0.7  \n")
      cat("  \n")
      
      dtx<-corval[!is.na(corval$hypomania_cor) & (
        corval$hypomania_cor<=-0.4 | 
          corval$hypomania_cor>=0.4)
        ,c("var.x","hypomania_cor")]
      cat("  \n")
      print(kable(dtx))
      cat("  \n")
      
      cat("  \n")
      cat("  \n#### CORRELATION: Variables versus EUTHYMIA  \n")
      cat("  \n")
      cat("  \nAbsolute Value greater than 0.7  \n")
      cat("  \n")
      
      dtx<-corval[!is.na(corval$euthymia_cor) & (
        corval$euthymia_cor<=-0.7 | 
          corval$euthymia_cor>=0.7)
        ,c("var.x","euthymia_cor")]
      cat("  \n")
      print(kable(dtx))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }    
  )
}

fun_chi <-function(dcorval,ind_var){
  #https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
  #https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
  tryCatch(
    { 
      varchi<-paste0(ind_var,"_chi")
      #https://support.minitab.com/es-mx/minitab/18/help-and-how-to/statistics/tables/how-to/chi-square-test-for-association/interpret-the-results/all-statistics/
      cat("  \n#### CHI-SQUARE: Variables vs DIAGNOSIS \n")
      cat("  \n")      
      cat("  \nX.var versus diagnosis  \n")
      cat("  \nH0:Independent variables  \n")
      cat("  \nH1:Dependent variables  \n")
      cat("  \nSelection of cases where P-value is greater than 0.05 for not to reject H0. It means, both variables are not dependent.  \n")  
      cat("  \n")
      
      
      dtx<-corval[ !is.na(corval[[varchi]]) & corval[[varchi]]>0.05 
                   ,c("var.x",varchi)]
      
      cat("  \n")
      cat("  \n##### Chi-square (top 10)  \n")
      cat("  \n")
      print(kable(head(dtx,10)))
      cat("  \n")
      cat("  \nAll Variables selected are shown in Summary Section  \n")
      cat("  \n")   
      
      features<-dtx[[varchi]]
      names(features)<-dtx$var.x
      list_featselalg[["Chi-square"]]<<-features
      
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }    
  )
}

fun_anova<-function(dcorval,ind_var){
  #https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
  #https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
  tryCatch(
    { 
      varano<-paste0(ind_var,"_anova")
      #https://www.r-bloggers.com/performing-anova-test-in-r-results-and-interpretation/
      cat("  \n#### ANOVA: Variables vs DIAGNOSIS \n")
      cat("  \n")      
      cat("  \nX.var versus diagnosis  \n")
      cat("  \nH0:All means are equals. There is no relation between x.var and diagnosis  \n")
      cat("  \nH1:Not all menas are equal. There is relation between x.var and diagnosis  \n")
      cat("  \nSelect cases where P-value is less than 0.05 to accept alternative hypotesis H1. It means, There is a relationship between variables.  \n")  
      cat("  \n")
      
      dtx<-corval[!is.na(corval[[varano]]) & corval[[varano]]<=0.05 
                  ,c("var.x",varano)]
      
      cat("  \n")
      cat("  \n##### Anova (top 10)  \n")
      cat("  \n")
      print(kable(head(dtx,10)))
      cat("  \n")
      cat("  \nAll Variables selected are shown in Summary Section  \n")
      cat("  \n")   
      
      features<-dtx[[varano]]
      names(features)<-dtx$var.x
      list_featselalg[["Anova"]]<<-features
      
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }    
  )
}




fun_boruta<-function(dfetsel,ind_var){
  #https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
  #https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
  tryCatch(
    { 
      
      
      formula<-as.formula(paste0(ind_var,"~."))
      boruta_output<-Boruta(formula,dfetsel,doTrace = 0)
      
      # TENTATIVE
      boruta_signif<-getSelectedAttributes(boruta_output, withTentative = TRUE)
      features<-boruta_signif
      list_featselalg[["btat"]]<<-boruta_signif
      
      cat("  \n#### BORUTA - TENTATIVES  \n")      
      cat("  \n")
      
      # Variable Importance Scores
      imps <- attStats(boruta_output)
      imps2 <- imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
      dfd<-head(imps2[order(-imps2$meanImp), ],5)  # descending sort
      
      cat("  \n##### Plot Importance Variables - Tentative  \n")      
      cat("  \n")
      
      plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
      cat("  \n")
      cat("  \n##### Variable Importance Tentative Scores (Top 5)  \n")      
      cat("  \n")
      print(kable(dfd))
      
      cat("  \n")
      cat("  \n##### Features Selected - Tentative  \n")      
      cat("  \n")
      print(kable(as.data.frame(features)))
      cat("  \n")
      
      # ROUGHFIX
      roughFixMod <- TentativeRoughFix(boruta_output)
      boruta_signif <- getSelectedAttributes(roughFixMod)
      features<-boruta_signif
      list_featselalg[["btar"]]<<-boruta_signif
      
      cat("  \n#### BORUTA -  ROUGHFIX (after the classification of tentative attributes)  \n")
      cat("  \n")
      
      # Variable Importance Scores
      imps <- attStats(roughFixMod)
      imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
      dfd<-head(imps2[order(-imps2$meanImp), ],5)  # descending sort      
      
      cat("  \n##### Plot Importance Variables - Roughfix  \n")      
      cat("  \n")
      
      plot(roughFixMod, cex.axis=.7, las=2, xlab="", main="Variable Importance")
      cat("  \n")
      cat("  \n##### Variable Importance Roughfix Scores (Top 5) \n")      
      cat("  \n")
      
      print(kable(dfd))
      
      cat("  \n")
      cat("  \n##### Features Selected - Roughfix  \n")      
      cat("  \n")
      print(kable(as.data.frame(features)))
      cat("  \n")
      
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }    
  )
}

fun_varimp<-function(dfetsel,ind_var){
  
  tryCatch(
    {      
      
      cat("  \n#### Variable Importance - RPART  \n")      
      cat("  \n")
      
      set.seed(100)
      
      formula<-as.formula(paste0(ind_var,"~."))
      
      
      repeat {
        rPartMod <- train(formula, data=dfetsel, method="rpart")
        rpartImp <- varImp(rPartMod)
        rpartImpimp <- rpartImp$importance
        overall<-rpartImpimp$Overall
        if(length(overall[!is.nan(overall)]) > 0 ){
          break
        }
      }
      
      cat("  \n")
      cat("  \n##### Variable Importance RPART Scores (Top 5) \n")      
      cat("  \n")
      print(kable(head(rpartImpimp,5)))
      
      rpartImpimp <- rpartImpimp[rpartImpimp$Overall>0,c("Overall"),drop=FALSE]
      rpartImpnames <-rownames(rpartImpimp)
      rpartImpimp <- as.numeric(rpartImpimp$Overall)
      names(rpartImpimp)<-rpartImpnames
      cat("  \n")
      print(plot(rpartImp, top = 20, main='Variable Importance'))
      cat("  \n")
      list_featselalg[["varImp_rpart"]]<<-rpartImpimp 
      
      cat("  \n")
      cat("  \n##### Features Selected RPART  \n")      
      cat("  \n")
      features<-rpartImpimp
      print(kable(as.data.frame(features)))
      cat("  \n")
      
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
    }
  )
  
  tryCatch(
    {
      
      cat("  \n#### Variable Importance - Random Forest  \n")
      cat("  \n")
      
      rrfMod <- train(formula, data=dfetsel, method="RRF")
      rrfImp <- varImp(rrfMod, scale=F)
      rrfImpimp <- rrfImp$importance
      
      cat("  \n")
      cat("  \n##### Variable Importance Random Forest Scores (Top 5) \n")
      cat("  \n")
      print(kable(head(rrfImpimp,5)))
      
      
      rrfImpimp <- rrfImpimp[rrfImpimp$Overall>0,c("Overall"),drop=FALSE]
      rrfImpnames <-rownames(rrfImpimp)
      rrfImpimp <- as.numeric(rrfImpimp$Overall)
      names(rrfImpimp)<-rrfImpnames
      cat("  \n")
      plot(rrfImp, top = 20, main='Variable Importance')
      list_featselalg[["varImp_rrf"]]<<-rrfImpimp
      
      cat("  \n")
      cat("  \n##### Features Selected Random Forest  \n")
      cat("  \n")
      features<-rrfImpimp
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)
      cat("  \n")
      
      return(NA)
    }
  )
  
}

fun_lasso<-function(dfetsel,ind_var){
  
  out<-tryCatch(
    {  
      
      cat("  \n#### Least Absolute Shrinkage and Selection Operator (LASSO)  \n")
      cat("  \n")      
      
      dat_laso<-dfetsel
      
      # Priorize Depression
      st_pr<--1
      
      
#      if (length(unique(dat_laso$final_diagnosis_numeric))>2){
#        dat_laso[dat_laso$final_diagnosis_numeric!=st_pr,c("final_diagnosis_numeric")]<-0;
#      }

      if (length(unique(dat_laso[[ind_var]]))>2){
        dat_laso[dat_laso[[ind_var]]!=st_pr,c(ind_var)]<-0;
      }
      
            
      x<-as.matrix(dat_laso[,!names(dat_laso) == ind_var]) # all X vars
      y<-as.double(as.matrix(dat_laso[, c(ind_var)])) 
      
      cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
      cat("  \n##### Plot Importance Variables - Lasso  \n")      
      cat("  \n")
      
      plot(cv.lasso)
      df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 5)
      
      vars<-df_coef[df_coef[, 1] != 0, ]
      
      cat("  \n")
      cat("  \n##### Contributing variables LASSO  \n")      
      cat("  \n")
      
      print(kable(vars))
      vars<-vars[names(vars)!="(Intercept)"]
      
      list_featselalg[["lasso"]]<<-vars
      
      cat("  \n")
      cat("  \n##### Features selected LASSO  \n")      
      cat("  \n")
      features<-vars
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )       
  
}


fun_stepwise<-function(dfetsel,ind_var){
  
  out<-tryCatch(
    {    
      
      cat("  \n#### Forward and Bakcward STEPWISE  \n")
      cat("  \n")      
      
      formula1<-as.formula(paste0(ind_var,"~ 1"))
      formula2<-as.formula(paste0(ind_var,"~ ."))
      
      # Step 1: Define base intercept only model
      base.mod <- lm(formula1 , data=dfetsel)
      
      # Step 2: Full model with all predictors
      all.mod <- lm(formula2 , data=dfetsel)
      
      # Step 3: Perform step-wise algorithm. direction='both' implies both forward and backward stepwise
      stepMod <- stats::step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  
      
      cat("  \n")
      cat("  \n##### Variable Importance STEPWISE  \n")
      cat("  \n")
      
      vars<-stepMod[[1]]
      print(kable(as.data.frame(vars)))
      vars<-vars[names(vars)!="(Intercept)"]
      
      list_featselalg[["stpw"]]<<-vars
      cat("  \n")
      cat("  \n##### Features selected STEPWISE  \n")      
      cat("  \n")
      print(kable(as.data.frame(vars)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )  
  
}

fun_relimp<-function(dfetsel,ind_var){
  out<-tryCatch(
    {
      cat("  \n#### RELATIVE IMPORTANCE FROM LINEAR REGRESSION  \n")
      cat("  \n")      
      
      formula1<-as.formula(paste0(ind_var,"~ ."))
      
      # Build linear regression model
      lmMod <- lm(formula1, data=dfetsel)
      
      # calculate relative importance
      # (Usually fails: Too few complete observations for estimating this model)
      relImportance <- calc.relimp(lmMod, type = "lmg", rela = F) 
      
      sort(round(relImportance$lmg, 5), decreasing=TRUE)
      
      list_featselalg[["relImp"]]<<-results$optVariables 
      cat("  \n")
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
}

fun_rfe<-function(dfetsel,ind_var){
  out<-tryCatch(
    {  
      
      cat("  \n#### Recursive Feature Elimination (RFE)  \n")
      cat("  \n")
      
      control<-rfeControl(functions=rfFuncs, method="cv", number=10)
      
      results <- rfe(dfetsel[,names(dfetsel) != ind_var], dfetsel[,c(ind_var)], sizes=c(1:ncol(dfetsel)-1),rfeControl=control)   
      
      cat("  \n")
      cat("  \n#### RFE results (top 10) \n")      
      cat("  \n")
      print(kable(head(results$results,10)))
      #print(results)
      cat("  \n")
      
      list_featselalg[["rfe"]]<<-results$optVariables  
      
      features<-results$optVariables
      cat("  \n")
      cat("  \n#### Features selected RFE  \n")      
      cat("  \n")
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
  
}

fun_genalg<-function(dfetsel,ind_var){
  out<-tryCatch(
    {  
      
      cat("  \n#### Genetic Algorithm  \n")
      cat("  \n")
      
      
      # Define control function
      ga_ctrl <- gafsControl(functions = rfGA,  # another option is `caretGA`.
                             method = "cv",
                             repeats = 3)
      
      # Genetic Algorithm feature selection
      set.seed(100)
      ga_obj <- gafs(x=dfetsel[, !names(dfetsel) == ind_var], 
                     y=dfetsel[, c(ind_var)], 
                     iters = 3,   # normally much higher (100+)
                     gafsControl = ga_ctrl)      
      
      
      cat("  \n")
      cat("  \n#### Genetic Algorithm results \n")      
      cat("  \n")
      print(ga_obj)
      #print(results)
      cat("  \n")
      
      list_featselalg[["GenAlg"]]<<-ga_obj$optVariables  
      features<-ga_obj$optVariables  
      cat("  \n")
      cat("  \n#### Features selected Genetic Algorithm  \n")      
      cat("  \n")
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
  
}

fun_simann<-function(dfetsel,ind_var){
  out<-tryCatch(
    {  
      
      cat("  \n#### Simulated Annealing  \n")
      cat("  \n")
      
      
      # Define control function
      sa_ctrl <- safsControl(functions = rfSA,  # another option is `caretGA`.
                             method = "repeatedcv",
                             repeats = 3,
                             improve = 5)
      
      # Simulated Annealing Algorithm feature selection
      set.seed(100)
      sa_obj <- safs(x=dfetsel[, names(dfetsel) != ind_var], 
                     y=dfetsel[, c(ind_var)],
                     safsControl = sa_ctrl)    
      
      cat("  \n")
      cat("  \n#### Simulated Annealing Algorithm results  \n")      
      cat("  \n")
      print(sa_obj)
      #print(results)
      cat("  \n")
      list_featselalg[["SimmAnn"]]<<-sa_obj$optVariables  
      features<-sa_obj$optVariables
      
      cat("  \n")
      cat("  \n#### Features selected SIMANN  \n")      
      cat("  \n")
      print(kable(as.data.frame(features)))
      cat("  \n")
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
  
}

fun_infval<-function(dfetsel,ind_var){
  out<-tryCatch(
    {  
      
      cat("  \n#### Information Value  \n")
      cat("  \n")
      
      
      dinfval<-dfetsel
      formula1<-as.formula(paste0(ind_var,"~ ."))
      
      # Priorize Depression
      st_pr<--1
      
      #if (length(unique(dinfval$final_diagnosis_numeric))>2){
      #  dinfval[dinfval$final_diagnosis_numeric!=st_pr,c("final_diagnosis_numeric")]<-0;
      #}    

      if (length(unique(dinfval[[ind_var]]))>2){
        dinfval[dinfval[[ind_var]]!=st_pr,c(ind_var)]<-0;
      }    
      
            
      # Choose Categorical Variables to compute Info Value.
      cat_vars<-names(dinfval)
      #cat_vars<-cat_vars[cat_vars!="final_diagnosis_numeric"]	  # get all categorical variables
      cat_vars<-cat_vars[cat_vars!=ind_var]	  # get all categorical variables
      
      df_iv <- data.frame(VARS=cat_vars, IV=numeric(length(cat_vars)), STRENGTH=character(length(cat_vars)), stringsAsFactors = F)  # init output dataframe
      
      
      for (factor_var in cat_vars){
        
        df_iv[df_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=dinfval[, factor_var], Y=dinfval$final_diagnosis_numeric)
        
        df_iv[df_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=dinfval[, factor_var], Y=dinfval$final_diagnosis_numeric), "howgood")
        
      }      
      
      # Sort
      df_iv <- df_iv[order(-df_iv$IV), ]
      
      reliv<-df_iv[df_iv$IV>0,]
      
      
      cat("  \n")
      cat("  \n##### Quatum of Information Values (Top 5)  \n")
      cat("  \n")
      
      print(kable(head(reliv,5)))
      
      
      namesiv<-reliv[,c("VARS")]
      valiv<-reliv[,c("IV")]
      names(valiv)<-namesiv
      
      list_featselalg[["InfVal"]]<<-valiv 
      
      
      cat("  \n")
      cat("  \n##### Features selected INFVAL  \n")      
      cat("  \n")
      features <- valiv
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
  
}


fun_dalex<-function(dfetsel,ind_var){
  #http://uc-r.github.io/dalex
  out<-tryCatch(
    {  
      
      cat("  \n#### Descriptive mAchine Learning EXplanations (DALEX) \n")
      cat("  \n")
      
      numvars_toshow<-20
      
      formula1<-as.formula(paste0(ind_var,"~ ."))
      
      # Train random forest model
      rf_mod <- randomForest(formula1, data=dfetsel, ntree=100)     
      
      # Variable importance with DALEX
      explained_rf <- explain(rf_mod, data=dfetsel, y=dfetsel[[ind_var]],verbose=FALSE)
      
      # Get the variable importances (use this for plotting)
      varimps = variable_importance(explained_rf, loss_function =loss_root_mean_square)
      
      
      cat("  \n")
      cat("  \n##### Plot Dalex Variable Importance  \n")
      cat("  \n")   
      print(plot(varimps))
      cat("  \n")   
      dfvimp<-as.data.frame(varimps[varimps$permutation==0,])
      dfvimp<-dfvimp[order(-dfvimp$dropout_loss),]
      dfvimp<-dfvimp[! dfvimp$variable %in% c("_baseline_","_full_model_"), ]
      dfvimp<-head(dfvimp,numvars_toshow)
      
      valimp<-dfvimp$dropout_loss
      namesvalimp<-dfvimp$variable
      names(valimp)<-namesvalimp
      
      list_featselalg[["Dalex"]]<<-valimp  
      
      cat("  \n")
      cat("  \n##### Features selected DALEX  \n")      
      cat("  \n")
      features<-valimp
      print(kable(as.data.frame(features)))
      cat("  \n")
      
    },
    error=function(cond){
      err<-paste0("  \n###### ERROR : ",cond,"  \n")
      cat("  \n")
      cat(err)      
      cat("  \n")
      
      return(NA) 
      
    }
  )
  
}
