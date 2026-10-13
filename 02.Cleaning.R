
#### Age -----------------------------------------------------------------------

BRFSS_all$Age_Cat <- ifelse((BRFSS_all$Age>=50 &  BRFSS_all$Age<60), "1",  
                     ifelse((BRFSS_all$Age>=60 &  BRFSS_all$Age<70), "2",  
                     ifelse((BRFSS_all$Age>=70 &  BRFSS_all$Age<75), "3", "0")))

BRFSS_all$Age_Cat2 <- ifelse((BRFSS_all$Age>=40 &  BRFSS_all$Age<50), "1",
                      ifelse((BRFSS_all$Age>=50 &  BRFSS_all$Age<60), "2",  
                      ifelse((BRFSS_all$Age>=60 &  BRFSS_all$Age<70), "3",  
                      ifelse((BRFSS_all$Age>=70 &  BRFSS_all$Age<75), "4", "0"))))

#### Race & Ethnicity ----------------------------------------------------------

BRFSS_all$Race[ BRFSS_all$Race==9 |  
                is.na(BRFSS_all$Race)] <-"Missing"


#### Region --------------------------------------------------------------------

BRFSS_all$Region <- ifelse(BRFSS_all$State==66 |BRFSS_all$State==72 | is.na(BRFSS_all$State), "Missing", 
                    ifelse((BRFSS_all$State==17 |BRFSS_all$State==18 |
                            BRFSS_all$State==19 |BRFSS_all$State==20 |
                            BRFSS_all$State==26 |BRFSS_all$State==27 |
                            BRFSS_all$State==29 |BRFSS_all$State==31 |
                            BRFSS_all$State==38 |BRFSS_all$State==39 |
                            BRFSS_all$State==46 |BRFSS_all$State==55), "c.Midwest",  
                    ifelse((BRFSS_all$State==9  |BRFSS_all$State==23 |
                            BRFSS_all$State==25 |BRFSS_all$State==33 |
                            BRFSS_all$State==34 |BRFSS_all$State==36 |
                            BRFSS_all$State==42 |BRFSS_all$State==44 |
                            BRFSS_all$State==50), "a.Northeast", 
                    ifelse((BRFSS_all$State==2  |BRFSS_all$State==4  |
                            BRFSS_all$State==6  |BRFSS_all$State==8  |
                            BRFSS_all$State==15 |BRFSS_all$State==16 |
                            BRFSS_all$State==30 |BRFSS_all$State==32 |
                            BRFSS_all$State==35 |BRFSS_all$State==41 |
                            BRFSS_all$State==49 |BRFSS_all$State==53 |
                            BRFSS_all$State==56), "d.West", "b.South"))))

#### Education -----------------------------------------------------------------

BRFSS_all$Edu <- ifelse((BRFSS_all$Educa <= 3), 1,
                 ifelse((BRFSS_all$Educa == 4), 2,
                 ifelse((BRFSS_all$Educa == 5), 3,
                 ifelse((BRFSS_all$Educa == 9), "Missing", 4)) ))
BRFSS_all$Edu[is.na(BRFSS_all$Edu)] <- "Missing"

#### Marital Status ------------------------------------------------------------

BRFSS_all$Marital_Cat <- ifelse((BRFSS_all$MaritalStat==1 | BRFSS_all$MaritalStat==6), 1,
                         ifelse((BRFSS_all$MaritalStat>=2 & BRFSS_all$MaritalStat<=4), 2, 
                         ifelse(BRFSS_all$MaritalStat==9, "Missing", 3)))
BRFSS_all$Marital_Cat[is.na(BRFSS_all$Marital_Cat)] <- "Missing"

#### Employment Status ---------------------------------------------------------

BRFSS_all$Employ[BRFSS_all$Employ == 9 | is.na(BRFSS_all$Employ)] <- "Missing"
BRFSS_all$Employ[BRFSS_all$Employ > 1 & BRFSS_all$Employ != "Missing"] <- 0

#### Income --------------------------------------------------------------------

# table(BRFSS_all$Income, useNA = "ifany")
BRFSS_all$Income_Cat <- NA
BRFSS_all$Income_Cat[BRFSS_all$Income <= 5] <- "0.Less than 35000"
BRFSS_all$Income_Cat[BRFSS_all$Income == 6 | BRFSS_all$Income == 7] <- "1.From 35000 to 75000"
BRFSS_all$Income_Cat[BRFSS_all$Income > 7 & BRFSS_all$Income < 12] <- "2.More than 75000"
BRFSS_all$Income_Cat[BRFSS_all$Income == 77 |
                   BRFSS_all$Income == 99 |
                   is.na(BRFSS_all$Income)] <- "Missing"

#### Home ownership ------------------------------------------------------------

BRFSS_all$Home[BRFSS_all$Home == 7 | 
               BRFSS_all$Home == 9 | 
               is.na(BRFSS_all$Home)] <- "Missing"

#### Urbancity -----------------------------------------------------------------

BRFSS_all$Urban[is.na(BRFSS_all$Urban)] <- "Missing"

#### Difficulty paying for care ------------------------------------------------

BRFSS_all$MED_Dff <- ifelse(is.na(BRFSS_all$MED_Dff) |
                            BRFSS_all$MED_Dff==7 |
                            BRFSS_all$MED_Dff==9, "Missing", 
                     ifelse(BRFSS_all$MED_Dff==1, "1.Yes", "0.No"))

#### Regular Source of Care ----------------------------------------------------

BRFSS_all$RegSource <- ifelse(BRFSS_all$RegSource==7 |
                              BRFSS_all$RegSource==9 |
                              is.na(BRFSS_all$RegSource), "Missing", 
                       ifelse(BRFSS_all$RegSource == 3, "1.No", "0.Yes"))

#### Wellness ------------------------------------------------------------------

BRFSS_all$Wellness <- ifelse(BRFSS_all$Wellness==7 |
                                BRFSS_all$Wellness==9 |
                                is.na(BRFSS_all$Wellness), "Missing", 
                              ifelse(BRFSS_all$Wellness == 1, "Yes", "No"))

#### Insurance -----------------------------------------------------------------

BRFSS_all$Hlthplan[is.na(BRFSS_all$Hlthplan) |
                   BRFSS_all$Hlthplan == 7 |
                   BRFSS_all$Hlthplan == 9] <- "Missing"
BRFSS_all$Hlthplan[BRFSS_all$Hlthplan == 2] <- "1.Uninsured"
BRFSS_all$Hlthplan[BRFSS_all$Hlthplan == 1] <- "0.Insured"

#### Binge drinking ------------------------------------------------------------

BRFSS_all$DrinkBinge[BRFSS_all$DrinkBinge == 9] <- "Missing"

#### Smoking -------------------------------------------------------------------

BRFSS_all$Smoke <- ifelse(is.na(BRFSS_all$Smoke100) | 
                            (BRFSS_all$Smoke100 == 1 & is.na(BRFSS_all$SmokeDay)), "Missing",
                          ifelse(BRFSS_all$Smoke100 == 2, "Never",
                                 ifelse(BRFSS_all$Smoke100 == 1 & 
                                          (BRFSS_all$SmokeDay == 1 | BRFSS_all$SmokeDay == 2), "Current",
                                        ifelse(BRFSS_all$Smoke100 == 1 & 
                                                 BRFSS_all$SmokeDay == 3, "Former", "Missing"))))

#### Physical activity ---------------------------------------------------------

BRFSS_all$PhyAct[BRFSS_all$PhyAct == 7 |
                 BRFSS_all$PhyAct == 9 |
                 is.na(BRFSS_all$PhyAct)] <- "Missing"

#### Self-rate health status ---------------------------------------------------

BRFSS_all$HLTHSTAT[BRFSS_all$HLTHSTAT == 9] <- "Missing"

#### Num of chronic conditions (sec. Chronic Health Conditions) ----------------

BRFSS_all$MI         <- ifelse(is.na(BRFSS_all$CVDINFR4) | BRFSS_all$CVDINFR4 == 7 | BRFSS_all$CVDINFR4 == 9, NA,
                        ifelse(BRFSS_all$CVDINFR4 == 1, 1, 0))
BRFSS_all$CHD        <- ifelse(is.na(BRFSS_all$CVDCRHD4) | BRFSS_all$CVDCRHD4 == 7 | BRFSS_all$CVDCRHD4 == 9, NA,
                        ifelse(BRFSS_all$CVDCRHD4 == 1, 1, 0))
BRFSS_all$STRK       <- ifelse(is.na(BRFSS_all$CVDSTRK3) | BRFSS_all$CVDSTRK3 == 7 | BRFSS_all$CVDSTRK3 == 9, NA,
                        ifelse(BRFSS_all$CVDSTRK3 == 1, 1, 0))
BRFSS_all$ASTH       <- ifelse(is.na(BRFSS_all$ASTHMA3) | BRFSS_all$ASTHMA3 == 7 | BRFSS_all$ASTHMA3 == 9, NA,
                        ifelse(BRFSS_all$ASTHMA3 == 1, 1, 0))
BRFSS_all$SKINCANCER <- ifelse(is.na(BRFSS_all$CHCSCNC1) | BRFSS_all$CHCSCNC1 == 7 | BRFSS_all$CHCSCNC1 == 9, NA,
                        ifelse(BRFSS_all$CHCSCNC1 == 1, 1, 0))
BRFSS_all$CANCER     <- ifelse(is.na(BRFSS_all$CHCOCNC1) | BRFSS_all$CHCOCNC1 == 7 | BRFSS_all$CHCOCNC1 == 9, NA,
                        ifelse(BRFSS_all$CHCOCNC1 == 1, 1, 0))
BRFSS_all$COPD       <- ifelse(is.na(BRFSS_all$CHCCOPD3) | BRFSS_all$CHCCOPD3 == 7 | BRFSS_all$CHCCOPD3 == 9, NA,
                        ifelse(BRFSS_all$CHCCOPD3 == 1, 1, 0))
BRFSS_all$DEPRESS    <- ifelse(is.na(BRFSS_all$ADDEPEV3) | BRFSS_all$ADDEPEV3 == 7 | BRFSS_all$ADDEPEV3 == 9, NA,
                        ifelse(BRFSS_all$ADDEPEV3 == 1, 1, 0))
BRFSS_all$KIDNEY     <- ifelse(is.na(BRFSS_all$CHCKDNY2) | BRFSS_all$CHCKDNY2 == 7 | BRFSS_all$CHCKDNY2 == 9, NA,
                        ifelse(BRFSS_all$CHCKDNY2 == 1, 1, 0))
BRFSS_all$ARTH       <- ifelse(is.na(BRFSS_all$HAVARTH4) | BRFSS_all$HAVARTH4 == 7 | BRFSS_all$HAVARTH4 == 9, NA,
                        ifelse(BRFSS_all$HAVARTH4 == 1, 1, 0))
BRFSS_all$DIAB       <- ifelse(is.na(BRFSS_all$DIABETE4) | BRFSS_all$DIABETE4 == 7 | BRFSS_all$DIABETE4 == 9, NA,
                        ifelse(BRFSS_all$DIABETE4 == 1, 1, 0))
BRFSS_all$Chronic    <- rowSums(
                        cbind(
                            BRFSS_all$MI,
                            BRFSS_all$CHD,
                            BRFSS_all$STRK,
                            BRFSS_all$ASTH,
                            BRFSS_all$SKINCANCER, 
                            BRFSS_all$CANCER,
                            BRFSS_all$COPD,
                            BRFSS_all$DEPRESS,
                            BRFSS_all$KIDNEY,
                            BRFSS_all$ARTH,
                            BRFSS_all$DIAB))
BRFSS_all$Chronic_Cat <- ifelse(is.na(BRFSS_all$Chronic), "Missing",
                         ifelse(BRFSS_all$Chronic == 0, "No", "Yes"))

#### Unmet needs ---------------------------------------------------------------

# For prevalence calculation

BRFSS_all$HouseIns_prev   <- 0
BRFSS_all$TransIns_prev   <- 0
BRFSS_all$UtilityIns_prev <- 0
BRFSS_all$FoodStamp_prev  <- 0
BRFSS_all$EmployIns_prev  <- 0
BRFSS_all$FoodIns_prev    <- 0
BRFSS_all$EmoSupport_prev <- 0

BRFSS_all$HouseIns_prev[BRFSS_all$HouseIns == 1]     <- 1
BRFSS_all$TransIns_prev[BRFSS_all$TransIns == 1]     <- 1
BRFSS_all$UtilityIns_prev[BRFSS_all$UtilityIns == 1] <- 1
BRFSS_all$FoodStamp_prev[BRFSS_all$FoodStamp == 1]   <- 1
BRFSS_all$EmployIns_prev[BRFSS_all$EmployIns == 1]   <- 1
BRFSS_all$FoodIns_prev[BRFSS_all$FoodIns < 4]        <- 1
BRFSS_all$EmoSupport_prev[BRFSS_all$EmoSupport == 4| 
                          BRFSS_all$EmoSupport == 5] <- 1

# For regression

BRFSS_all$HouseIns[BRFSS_all$HouseIns == 2]     <- 0
BRFSS_all$TransIns[BRFSS_all$TransIns == 2]     <- 0
BRFSS_all$UtilityIns[BRFSS_all$UtilityIns == 2] <- 0
BRFSS_all$FoodStamp[BRFSS_all$FoodStamp == 2]   <- 0
BRFSS_all$EmployIns[BRFSS_all$EmployIns == 2]   <- 0

BRFSS_all$FoodIns[BRFSS_all$FoodIns < 4]  <- 1
BRFSS_all$FoodIns[BRFSS_all$FoodIns == 4 | 
                  BRFSS_all$FoodIns == 5] <- 0

BRFSS_all$EmoSupport[BRFSS_all$EmoSupport < 4]  <- 0
BRFSS_all$EmoSupport[BRFSS_all$EmoSupport == 4 | 
                     BRFSS_all$EmoSupport == 5] <- 1

BRFSS_all$HouseIns[BRFSS_all$HouseIns > 2 | is.na(BRFSS_all$HouseIns)]     <- "Missing"
BRFSS_all$TransIns[BRFSS_all$TransIns > 2 | is.na(BRFSS_all$TransIns)]     <- "Missing"
BRFSS_all$UtilityIns[BRFSS_all$UtilityIns > 2 | is.na(BRFSS_all$UtilityIns)] <- "Missing"
BRFSS_all$FoodStamp[BRFSS_all$FoodStamp > 2 | is.na(BRFSS_all$FoodStamp)]   <- "Missing"
BRFSS_all$EmployIns[BRFSS_all$EmployIns > 2 | is.na(BRFSS_all$EmployIns)] <- "Missing"
BRFSS_all$FoodIns[BRFSS_all$FoodIns > 5 | is.na(BRFSS_all$FoodIns)]  <- "Missing"
BRFSS_all$EmoSupport[BRFSS_all$EmoSupport > 5 | is.na(BRFSS_all$EmoSupport)]  <- "Missing"

