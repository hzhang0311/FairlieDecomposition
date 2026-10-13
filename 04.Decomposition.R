f.decomp <- function(i){

  set.seed(i)

  # set.BRFSS_final <- f.svy_set(BRFSS_final)
  set.BRFSS_dum  <- f.svy_set(BRFSS_dum)
  # sub.BRFSS_final <- subset(set.BRFSS_final, flag_complete_UMN == 1)
  # sub.BRFSS_dum   <- subset(set.BRFSS_dum, flag_complete_UMN == 1)
  

  #### Step 1: 基准筛查率 ------------------------------------------------------

  LGBQ_UTD_unadj  <-  prop.table(svytable(~BC_UTD, subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 1)))[2]
  STRT_UTD_unadj  <-  prop.table(svytable(~BC_UTD, subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 0)))[2]
  
  LGBQ_UTD_n      <-  sum(table(BRFSS_dum$BC_UTD[BRFSS_dum$flag_complete_UMN==1 & BRFSS_dum$LGBQ_bin==1]))
  STRT_UTD_n      <-  sum(table(BRFSS_dum$BC_UTD[BRFSS_dum$flag_complete_UMN==1 & BRFSS_dum$LGBQ_bin==0]))

  
  #### Step 2: LGBQ Logit + predict --------------------------------------------
  
  LGBQ_reg        <-  svyglm(BC_UTD ~ EmoSupport_Yes + 
                                      EmployIns_Yes + 
                                      FoodIns_Yes +
                                      FoodStamp_Yes +
                                      HouseIns_Yes +
                                      TransIns_Yes +
                                      UtilityIns_Yes +
                                      Age + 
                                      Race_BL + Race_OT + Race_ML + Race_HI + Race_MI +
                                      Region_SO + Region_MW + Region_WT +
                                      Urban_No + Urban_MI +
                                      Edu_LHS + Edu_HSG + Edu_ASS  + Edu_MIS +
                                      Employ_No + Employ_MIS +
                                      Marital_Div + Marital_Nev + Marital_MIS +
                                      Inc_1 + Inc_2 + Inc_MIS +
                                      HLTHSTAT_POOR + HLTHSTAT_MIS +
                                      Chronic_Yes + Chronic_MIS +
                                      Ins_No + Ins_MIS +
                                      Reg_No + Reg_MIS +
                                      Wellness_No + Wellness_MIS +
                                      Med_Yes + Med_MIS,
                             design = subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 1),
                             family = quasibinomial(link = logit))
  
  #Identify cohort of just Hispanic Respondents 
  BRFSS_dum$LGBQ_cohort  <- ifelse(BRFSS_dum$flag_complete_UMN == 1 & BRFSS_dum$LGBQ_bin == 1, 1, 0)
  BRFSS_dum$outP_LGBQ    <- predict(LGBQ_reg, newdata = BRFSS_dum, type = "response")
  
  set.BRFSS_dum  <- f.svy_set(BRFSS_dum)
  p.LGBQ.out     <- svymean(~outP_LGBQ, design = subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  
  #Sort by predicted probability of testing
  BRFSS_dum      <-  BRFSS_dum[order( BRFSS_dum$outP_LGBQ),]
  
  #Assign just the LGBQ cohort members a rank value (1 is lowest probability of testing)     
  BRFSS_dum$LGBQ_risk <- NA
  BRFSS_dum$LGBQ_risk[BRFSS_dum$flag_complete_UMN == 1 & BRFSS_dum$LGBQ_bin == 1] <- seq(1:LGBQ_UTD_n)
  
  
  #### Step 3: Straight Logit + predict ----------------------------------------
  
  Straight_reg   <-  svyglm(BC_UTD ~ EmoSupport_Yes + 
                                     EmployIns_Yes + 
                                     FoodIns_Yes +
                                     FoodStamp_Yes +
                                     HouseIns_Yes +
                                     TransIns_Yes +
                                     UtilityIns_Yes +
                                     Age + 
                                     Race_BL + Race_OT + Race_ML + Race_HI + Race_MI +
                                     Region_SO + Region_MW + Region_WT +
                                     Urban_No + Urban_MI +
                                     Edu_LHS + Edu_HSG + Edu_ASS  + Edu_MIS +
                                     Employ_No + Employ_MIS +
                                     Marital_Div + Marital_Nev + Marital_MIS +
                                     Inc_1 + Inc_2 + Inc_MIS +
                                     HLTHSTAT_POOR + HLTHSTAT_MIS +
                                     Chronic_Yes + Chronic_MIS +
                                     Ins_No + Ins_MIS +
                                     Reg_No + Reg_MIS +
                                     Wellness_No + Wellness_MIS +
                                     Med_Yes + Med_MIS,
                             design = subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 0),
                             family = quasibinomial(link = logit))
  
  #Predict Straight Respondents with non-Hispanic Coefficients  
  BRFSS_dum$outP_STRAIGHT <- predict(Straight_reg, newdata = BRFSS_dum, type = "response")
  set.BRFSS_dum           <- f.svy_set(BRFSS_dum)
  p.STRAIGHT.out          <- svymean(~outP_STRAIGHT, design = subset(set.BRFSS_dum, flag_complete_UMN == 1 & LGBQ_bin == 0), na.rm=TRUE)
  
  #Sort by predicted probability of testing
  BRFSS_dum               <- BRFSS_dum[order(BRFSS_dum$outP_STRAIGHT),]
  
  #Assign just the STRAIGHT cohort members a rank value (1 is lowest probability of testing)     
  BRFSS_dum$STRAIGHT_risk <- NA
  BRFSS_dum$STRAIGHT_risk[BRFSS_dum$flag_complete_UMN == 1 & BRFSS_dum$LGBQ_bin == 0] <- seq(1:STRT_UTD_n)
  
  
  #### Step 4: 随机抽取同大小反事实组 ------------------------------------------
  
  #assign all NHW a random variable
  BRFSS_dum$rand          <- NA
  BRFSS_dum$rand[BRFSS_dum$flag_complete_UMN == 1 & BRFSS_dum$LGBQ_bin == 0] <- runif(STRT_UTD_n, 0, 1000)
  
  #sort by that variable    
  BRFSS_dum               <- BRFSS_dum[order(BRFSS_dum$rand),]
  
  #Assign those with the lower values an indicator that they are now our counterfactual cohort (same number of these as our actual LGBQ cohort)   
  BRFSS_dum$cntrfctl               <- 0
  BRFSS_dum$cntrfctl[1:LGBQ_UTD_n] <- 1
  
  set.BRFSS_dum           <- f.svy_set(BRFSS_dum)
  
  #Run a prediction with the new group 
  #(we can still use the predicted probabilities from before, just subset to the counterfactural group only)
  p.STRAIGHT.out1         <- svymean(~outP_STRAIGHT, design = subset(set.BRFSS_dum, cntrfctl==1), na.rm=TRUE)
  
  #Check the difference between this and the Hispanic version 
  diff_tot                <- (p.STRAIGHT.out1 - p.LGBQ.out)
  
  
  #### Step 5: 逐变量替换 ------------------------------------------------------
  
  # CREATE HISP COHORT WITH WHITE AGE & RUN HISP PREDICTION
  BRFSS_dum2              <- BRFSS_dum
  
  #sort the original by white risk and the new by hispanic risk         
  BRFSS_dum               <- BRFSS_dum[order(BRFSS_dum$STRAIGHT_risk),]
  BRFSS_dum2              <- BRFSS_dum2[order(BRFSS_dum2$LGBQ_risk),]
  
  ##### Age #####
  BRFSS_dum2$Age[BRFSS_dum2$LGBQ_cohort==1] <- BRFSS_dum$Age[BRFSS_dum$cntrfctl==1] 
  BRFSS_dum2$outP_STRAIGHT2 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out2               <- svymean(~outP_STRAIGHT2, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.age                  <- (p.LGBQ.out2 - p.LGBQ.out)
  
  ##### Emotional Support #####
  BRFSS_dum2$EmoSupport_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$EmoSupport_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT3 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out3               <- svymean(~outP_STRAIGHT3, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.EmoSupport           <- (p.LGBQ.out3 - p.LGBQ.out2)  
  
  ##### Employment Insurance #####
  BRFSS_dum2$EmployIns_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$EmployIns_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT4 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out4               <- svymean(~outP_STRAIGHT4, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.EmployIns            <- (p.LGBQ.out4 - p.LGBQ.out3)
  
  ##### Food Insecurity #####
  BRFSS_dum2$FoodIns_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$FoodIns_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT5 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out5               <- svymean(~outP_STRAIGHT5, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.FoodIns              <- (p.LGBQ.out5 - p.LGBQ.out4)
  
  ##### Food Stamp #####
  BRFSS_dum2$FoodStamp_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$FoodStamp_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT6 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out6               <- svymean(~outP_STRAIGHT6, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.FoodStamp            <- (p.LGBQ.out6 - p.LGBQ.out5)
  
  ##### Housing Insecurity #####
  BRFSS_dum2$HouseIns_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$HouseIns_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT7 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out7               <- svymean(~outP_STRAIGHT7, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.HouseIns             <- (p.LGBQ.out7 - p.LGBQ.out6)
  
  ##### Transportation #####
  BRFSS_dum2$TransIns_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$TransIns_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT8 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out8               <- svymean(~outP_STRAIGHT8, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.TransIns             <- (p.LGBQ.out8 - p.LGBQ.out7)
  
  ##### Utility #####
  BRFSS_dum2$UtilityIns_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$UtilityIns_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT9 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2            <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out9               <- svymean(~outP_STRAIGHT9, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.UtilityIns           <- (p.LGBQ.out9 - p.LGBQ.out8)
  
  ##### Race #####
  BRFSS_dum2$Race_BL[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Race_BL[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Race_OT[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Race_OT[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Race_ML[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Race_ML[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Race_HI[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Race_HI[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Race_MI[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Race_MI[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT10 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out10               <- svymean(~outP_STRAIGHT10, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Race                  <- (p.LGBQ.out10 - p.LGBQ.out9)
  
  ##### Region #####
  BRFSS_dum2$Region_SO[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Region_SO[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Region_MW[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Region_MW[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Region_WT[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Region_WT[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT11 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out11               <- svymean(~outP_STRAIGHT11, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Region                <- (p.LGBQ.out11 - p.LGBQ.out10)
  
  ##### Urban #####
  BRFSS_dum2$Urban_No[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Urban_No[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Urban_MI[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Urban_MI[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT12 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out12               <- svymean(~outP_STRAIGHT12, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Urban                 <- (p.LGBQ.out12 - p.LGBQ.out11)
  
  ##### Education #####
  BRFSS_dum2$Edu_LHS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Edu_LHS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Edu_HSG[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Edu_HSG[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Edu_ASS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Edu_ASS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Edu_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Edu_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT13 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out13               <- svymean(~outP_STRAIGHT13, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Edu                   <- (p.LGBQ.out13 - p.LGBQ.out12)
  
  ##### Employment #####
  BRFSS_dum2$Employ_No[BRFSS_dum2$LGBQ_cohort == 1]  <- BRFSS_dum$Employ_No[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Employ_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Employ_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT14 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out14               <- svymean(~outP_STRAIGHT14, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Employ                <- (p.LGBQ.out14 - p.LGBQ.out13)
  
  ##### Marital Status #####
  BRFSS_dum2$Marital_Div[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Marital_Div[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Marital_Nev[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Marital_Nev[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Marital_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Marital_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT15 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out15               <- svymean(~outP_STRAIGHT15, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Marital               <- (p.LGBQ.out15 - p.LGBQ.out14)
  
  ##### Income #####
  BRFSS_dum2$Inc_1[BRFSS_dum2$LGBQ_cohort == 1]   <- BRFSS_dum$Inc_1[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Inc_2[BRFSS_dum2$LGBQ_cohort == 1]   <- BRFSS_dum$Inc_2[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Inc_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Inc_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT16 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out16               <- svymean(~outP_STRAIGHT16, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Inc                   <- (p.LGBQ.out16 - p.LGBQ.out15)
  
  ##### Health Status #####
  BRFSS_dum2$HLTHSTAT_POOR[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$HLTHSTAT_POOR[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$HLTHSTAT_MIS[BRFSS_dum2$LGBQ_cohort == 1]  <- BRFSS_dum$HLTHSTAT_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT17 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out17               <- svymean(~outP_STRAIGHT17, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Health                <- (p.LGBQ.out17 - p.LGBQ.out16)
  
  ##### Chronic #####
  BRFSS_dum2$Chronic_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Chronic_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Chronic_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Chronic_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT18 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out18               <- svymean(~outP_STRAIGHT18, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Chronic               <- (p.LGBQ.out18 - p.LGBQ.out17)
  
  ##### Insurance #####
  BRFSS_dum2$Ins_No[BRFSS_dum2$LGBQ_cohort == 1]  <- BRFSS_dum$Ins_No[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Ins_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Ins_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT19 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out19               <- svymean(~outP_STRAIGHT19, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Ins                   <- (p.LGBQ.out19 - p.LGBQ.out18)
  
  ##### Regular Source of Care #####
  BRFSS_dum2$Reg_No[BRFSS_dum2$LGBQ_cohort == 1]  <- BRFSS_dum$Reg_No[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Reg_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Reg_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT20 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out20               <- svymean(~outP_STRAIGHT20, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Reg                   <- (p.LGBQ.out20 - p.LGBQ.out19)
  
  ##### Wellness Visit #####
  BRFSS_dum2$Wellness_No[BRFSS_dum2$LGBQ_cohort == 1]  <- BRFSS_dum$Wellness_No[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Wellness_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Wellness_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT21 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out21               <- svymean(~outP_STRAIGHT21, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Wellness              <- (p.LGBQ.out21 - p.LGBQ.out20)
  
  ##### Medical Difficulty #####
  BRFSS_dum2$Med_Yes[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Med_Yes[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$Med_MIS[BRFSS_dum2$LGBQ_cohort == 1] <- BRFSS_dum$Med_MIS[BRFSS_dum$cntrfctl == 1]
  BRFSS_dum2$outP_STRAIGHT22 <- predict(LGBQ_reg, newdata = BRFSS_dum2, type = "response")
  set.BRFSS_dum2             <- f.svy_set(BRFSS_dum2)
  p.LGBQ.out22               <- svymean(~outP_STRAIGHT22, design = subset(set.BRFSS_dum2, flag_complete_UMN == 1 & LGBQ_bin == 1), na.rm=TRUE)
  diff.Med                   <- (p.LGBQ.out22 - p.LGBQ.out21)
  
  
  #### Step 6: 汇总输出 --------------------------------------------------------
  
  diff_sum <- sum(diff.age, diff.EmoSupport, diff.EmployIns, diff.FoodIns,
                  diff.FoodStamp, diff.HouseIns, diff.TransIns, diff.UtilityIns,
                  diff.Race, diff.Region, diff.Urban, diff.Edu, diff.Employ,
                  diff.Marital, diff.Inc, diff.Health, diff.Chronic, diff.Ins,
                  diff.Reg, diff.Wellness, diff.Med)
  
  decomp_out <- c(diff_tot, diff.age, diff.EmoSupport, diff.EmployIns, 
                  diff.FoodIns, diff.FoodStamp, diff.HouseIns, diff.TransIns, 
                  diff.UtilityIns, diff.Race, diff.Region, diff.Urban, diff.Edu, 
                  diff.Employ, diff.Marital, diff.Inc, diff.Health, diff.Chronic, 
                  diff.Ins, diff.Reg, diff.Wellness, diff.Med, diff_sum)
  
  return(decomp_out)  
}
