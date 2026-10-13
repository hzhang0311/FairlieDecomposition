# Select Useful Vars

BRFSS_24 <- BRFSS_2024 %>%
  rename_with(toupper) %>%
  dplyr::select(
    "ID"     = SEQNO,
    "Strata" = `_STSTR`,
    "PSU"    = `_PSU`,
    "Weight" = `_LLCPWT`,
    
    "Ever_Mamm" = HADMAM,
    "Last_Mamm" = HOWLONG,
    
    "Age"   = `_AGE80`,
    "Race"  = `_RACEGR3`,
    "Sex" =`_SEX`,
    "Orient_Female" = SOFEMALE,
    "Income" = INCOME3,
    "MaritalStat" = MARITAL,
    "NCHILD" = CHILDREN,
    "Educa"  = EDUCA,
    "Employ" = EMPLOY1,
    "Home"   = RENTHOM1,
    "Urban"  = `_URBSTAT`,
    "State"  = `_STATE`,
    
    "BMI" = `_BMI5`,
    "BMI_Cat" =  `_BMI5CAT`,
    "HLTHSTAT" = `_RFHLTH`,
    #"Cancer"   = CHCOCNC1,
    #"NonSkinMel" = CHCSCNC1,
    "CancerType" = CNCRTYP2,
    "DrinkBinge" = `_RFBING6`,
    "Smoke100" = SMOKE100,
    "SmokeDay"  = SMOKDAY2,
    "PhyAct" = EXERANY2,
    "Hlthplan" = `_HLTHPL2`,
    "RegSource" = PERSDOC3,
    "Wellness" = CHECKUP1,
    
    CVDINFR4,
    CVDCRHD4,
    CVDSTRK3,
    ASTHMA3,
    CHCSCNC1,
    CHCOCNC1,
    CHCCOPD3,
    ADDEPEV3,
    CHCKDNY2,
    HAVARTH4,
    DIABETE4,
    
    "LifeDiss"   = LSATISFY,
    "EmoSupport" = EMTSUPRT,
    #"Loneliness" = SDLONELY, # changed "isolated" to "loneliness"
    #"Isolated"   = 99,
    "EmployIns" = SDHEMPLY,
    "FoodStamp"  = FOODSTMP,
    "FoodIns"    = SDHFOOD1,
    "TransIns"   = SDHTRNSP,
    "HouseIns"   = SDHBILLS,
    "UtilityIns" = SDHUTILS,
    #"MentalDis"  = 99,
    #"Safety"     = HOWSAFE1,
    "MED_Dff" = MEDCOST1
    
  )

BRFSS_22 <- BRFSS2022 %>% 
  dplyr::select(
    "ID"     = SEQNO,
    "Strata" = `_STSTR`,
    "PSU"    = `_PSU`,
    "Weight" = `_LLCPWT`,
    
    "Ever_Mamm" = HADMAM,
    "Last_Mamm" = HOWLONG,
    
    "Age"   = `_AGE80`,
    "Race"  = `_RACEGR4`,
    "Sex" =`_SEX`,
    "Orient_Female" = SOFEMALE,
    "Income" = INCOME3,
    "MaritalStat" = MARITAL,
    "NCHILD" = CHILDREN,
    "Educa"  = EDUCA,
    "Employ" = EMPLOY1,
    "Home"   = RENTHOM1,
    "Urban"  = `_URBSTAT`,
    "State"  = `_STATE`,
    
    "BMI" = `_BMI5`,
    "BMI_Cat" =  `_BMI5CAT`,
    "HLTHSTAT" = `_RFHLTH`,
    #"Cancer"   = CHCOCNC1,
    #"NonSkinMel" = CHCSCNC1,
    "CancerType" = CNCRTYP2,# Minor question changed in 2022 (1.6, 1.8, 2.1 across years). Most recent and the earliest ones are both cross-sectional
    "DrinkBinge" = `_RFBING6`,
    "Smoke100" = SMOKE100,
    "SmokeDay"  = SMOKDAY2,
    "PhyAct" = EXERANY2,
    "Hlthplan" = `_HLTHPLN`, # PRIMINSR?
    "RegSource" = PERSDOC3,
    "Wellness" = CHECKUP1,
    
    CVDINFR4,
    CVDCRHD4,
    CVDSTRK3,
    ASTHMA3,
    CHCSCNC1,
    CHCOCNC1,
    CHCCOPD3,
    ADDEPEV3,
    CHCKDNY2,
    HAVARTH4,
    DIABETE4,
    
    "LifeDiss"   = LSATISFY,
    "EmoSupport" = EMTSUPRT,
    #"Loneliness" = 99,
    #"Isolated"   = SDHISOLT,
    "EmployIns" = SDHEMPLY,
    "FoodStamp"  = FOODSTMP,
    "FoodIns"    = SDHFOOD1,
    "TransIns"   = SDHTRNSP,
    "HouseIns"   = SDHBILLS,
    "UtilityIns" = SDHUTILS,
    #"MentalDis"  = SDHSTRE1,
    #"Safety"     = 99,
    "MED_Dff"    = MEDCOST1
  )

BRFSS_24$Year <- rep(2024, nrow(BRFSS_24))
BRFSS_22$Year <- rep(2022, nrow(BRFSS_22))
BRFSS_all     <- rbind(BRFSS_22, BRFSS_24)

# Clean BC Screening Vars

# BRFSS_all$BC_Ever  <- ifelse((BRFSS_all$Ever_Mamm == 7 | 
#                               BRFSS_all$Ever_Mamm == 9 | 
#                               is.na(BRFSS_all$Ever_Mamm)), NA,
#                       ifelse(BRFSS_all$Ever_Mamm == 1, 1, 0))
# BRFSS_all$UTD_Mamm <- ifelse(BRFSS_all$Last_Mamm <= 2, 1, 0)
# 
# ### 250 ever screened individual missed the UTD question
# ###     - Only included people answered both questions
# 
# BRFSS_all$BC_UTD <- ifelse(is.na(BRFSS_all$BC_Ever), NA, 
#                     ifelse(BRFSS_all$BC_Ever == 0, 0,
#                     ifelse(BRFSS_all$BC_Ever == 1 & BRFSS_all$UTD_Mamm == 1, 1, 0)))


# Ever had mammogram (1 = yes, 2 = no, 7/9/NA = missing)
BRFSS_all$BC_Ever <- ifelse(BRFSS_all$Ever_Mamm %in% c(7, 9) | is.na(BRFSS_all$Ever_Mamm),
                            NA,
                            ifelse(BRFSS_all$Ever_Mamm == 1, 1, 0))

# Up-to-date mammogram (HOWLONG: 1=within 1 year, 2=within 2 years)
# DK/refused/missing should remain NA, not forced into 0
BRFSS_all$UTD_Mamm <- ifelse(BRFSS_all$Last_Mamm %in% c(7, 9) | is.na(BRFSS_all$Last_Mamm),
                             NA,
                             ifelse(BRFSS_all$Last_Mamm <= 2, 1, 0))

# Final combined outcome:
# - NA if BC_Ever is missing
# - 0 if never screened
# - If screened, BC_UTD depends on UTD_Mamm (can be NA)
BRFSS_all$BC_UTD <- ifelse(is.na(BRFSS_all$BC_Ever), NA,
                           ifelse(BRFSS_all$BC_Ever == 0, 0,
                                  BRFSS_all$UTD_Mamm))


BRFSS_all        <- dplyr::select(BRFSS_all, -Ever_Mamm, -Last_Mamm, -UTD_Mamm)