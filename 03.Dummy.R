
#### Change to dummy vars (covariates) -----------------------------------------

# Race
BRFSS_all$Race_BL <- ifelse(BRFSS_all$Race == "2", 1, 0)
BRFSS_all$Race_OT <- ifelse(BRFSS_all$Race == "3", 1, 0)
BRFSS_all$Race_ML <- ifelse(BRFSS_all$Race == "4", 1, 0)
BRFSS_all$Race_HI <- ifelse(BRFSS_all$Race == "5", 1, 0)
BRFSS_all$Race_MI <- ifelse(BRFSS_all$Race == "Missing", 1, 0)

# Region
BRFSS_all$Region_SO <- ifelse(BRFSS_all$Region == "b.South", 1, 0)
BRFSS_all$Region_MW <- ifelse(BRFSS_all$Region == "c.Midwest", 1, 0)
BRFSS_all$Region_WT <- ifelse(BRFSS_all$Region == "d.West", 1, 0)

# Urban
BRFSS_all$Urban_No <- ifelse(BRFSS_all$Urban == "2", 1, 0)
BRFSS_all$Urban_MI <- ifelse(BRFSS_all$Urban == "Missing", 1, 0)

# Education
BRFSS_all$Edu_LHS <- ifelse(BRFSS_all$Edu == "1", 1, 0)
BRFSS_all$Edu_HSG <- ifelse(BRFSS_all$Edu == "2", 1, 0)
BRFSS_all$Edu_ASS <- ifelse(BRFSS_all$Edu == "3", 1, 0)
BRFSS_all$Edu_MIS <- ifelse(BRFSS_all$Edu == "Missing", 1, 0)

# Employ
BRFSS_all$Employ_No  <- ifelse(BRFSS_all$Employ == "0", 1, 0)
BRFSS_all$Employ_MIS <- ifelse(BRFSS_all$Employ == "Missing", 1, 0)

# Marital status
BRFSS_all$Marital_Div <- ifelse(BRFSS_all$Marital_Cat == "2", 1, 0)
BRFSS_all$Marital_Nev <- ifelse(BRFSS_all$Marital_Cat == "3", 1, 0)
BRFSS_all$Marital_MIS <- ifelse(BRFSS_all$Marital_Cat == "Missing", 1, 0)

# Income
BRFSS_all$Inc_1   <- ifelse(BRFSS_all$Income_Cat == "0.Less than 35000", 1, 0)
BRFSS_all$Inc_2   <- ifelse(BRFSS_all$Income_Cat == "1.From 35000 to 75000", 1, 0)
BRFSS_all$Inc_MIS <- ifelse(BRFSS_all$Income_Cat == "Missing", 1, 0)

# Insurance
BRFSS_all$Ins_No   <- ifelse(BRFSS_all$Hlthplan == "1.Uninsured", 1, 0)
BRFSS_all$Ins_MIS  <- ifelse(BRFSS_all$Hlthplan == "Missing", 1, 0)

# Regular source of care
BRFSS_all$Reg_No   <- ifelse(BRFSS_all$RegSource == "1.No", 1, 0)
BRFSS_all$Reg_MIS  <- ifelse(BRFSS_all$RegSource == "Missing", 1, 0)

# Difficulty paying for care
BRFSS_all$Med_Yes  <- ifelse(BRFSS_all$MED_Dff == "1.Yes", 1, 0)
BRFSS_all$Med_MIS  <- ifelse(BRFSS_all$MED_Dff == "Missing", 1, 0)

# Wellness
BRFSS_all$Wellness_No  <- ifelse(BRFSS_all$Wellness == "Yes", 0, 1)
BRFSS_all$Wellness_MIS  <- ifelse(BRFSS_all$Wellness == "Missing", 1, 0)

# Chronic
BRFSS_all$Chronic_Yes  <- ifelse(BRFSS_all$Chronic_Cat == "Yes", 1, 0)
BRFSS_all$Chronic_MIS  <- ifelse(BRFSS_all$Chronic_Cat == "Missing", 1, 0)

# Health Status
BRFSS_all$HLTHSTAT_POOR  <- ifelse(BRFSS_all$HLTHSTAT == "2", 1, 0)
BRFSS_all$HLTHSTAT_MIS   <- ifelse(BRFSS_all$HLTHSTAT == "Missing", 1, 0)

# Binge drinking
BRFSS_all$DrinkBinge_Yes  <- ifelse(BRFSS_all$DrinkBinge == "2", 1, 0)
BRFSS_all$DrinkBinge_MIS   <- ifelse(BRFSS_all$DrinkBinge == "Missing", 1, 0)

# Physical activity
BRFSS_all$PhyAct_No    <- ifelse(BRFSS_all$PhyAct == "2", 1, 0)
BRFSS_all$PhyAct_MIS   <- ifelse(BRFSS_all$PhyAct == "Missing", 1, 0)

# Smoking
BRFSS_all$Smoke_Current <- ifelse(BRFSS_all$Smoke == "Current", 1, 0)
BRFSS_all$Smoke_Former  <- ifelse(BRFSS_all$Smoke == "Former", 1, 0)
BRFSS_all$Smoke_MIS     <- ifelse(BRFSS_all$Smoke == "Missing", 1, 0)


#### Change to dummy vars (Unmet needs) ----------------------------------------

BRFSS_all$EmoSupport_Yes <- ifelse(BRFSS_all$EmoSupport == "1", 1, 0)
BRFSS_all$EmoSupport_MIS <- ifelse(BRFSS_all$EmoSupport == "Missing", 1, 0)

BRFSS_all$EmployIns_Yes <- ifelse(BRFSS_all$EmployIns == "1", 1, 0)
BRFSS_all$EmployIns_MIS <- ifelse(BRFSS_all$EmployIns == "Missing", 1, 0)

BRFSS_all$FoodIns_Yes  <- ifelse(BRFSS_all$FoodIns == "1", 1, 0)
BRFSS_all$FoodIns_MIS  <- ifelse(BRFSS_all$FoodIns == "Missing", 1, 0)

BRFSS_all$FoodStamp_Yes <- ifelse(BRFSS_all$FoodStamp == "1", 1, 0)
BRFSS_all$FoodStamp_MIS <- ifelse(BRFSS_all$FoodStamp == "Missing", 1, 0)

BRFSS_all$HouseIns_Yes <- ifelse(BRFSS_all$HouseIns == "1", 1, 0)
BRFSS_all$HouseIns_MIS <- ifelse(BRFSS_all$HouseIns == "Missing", 1, 0)

BRFSS_all$TransIns_Yes <- ifelse(BRFSS_all$TransIns == "1", 1, 0)
BRFSS_all$TransIns_MIS <- ifelse(BRFSS_all$TransIns == "Missing", 1, 0)

BRFSS_all$UtilityIns_Yes <- ifelse(BRFSS_all$UtilityIns == "1", 1, 0)
BRFSS_all$UtilityIns_MIS <- ifelse(BRFSS_all$UtilityIns == "Missing", 1, 0)
