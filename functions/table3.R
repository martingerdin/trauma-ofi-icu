#TABLE 2: Adjusted and unadjusted logistic regression
# Data Preparation
ofi_alive <- subset(tablereg, subset = (Survival == "Alive"))

tablereg1 <- ofi_alive %>% 
  select(Sex, Age, Intubation, RTS, ISS,TimeFCT, OnDuty, daysinICU, TimeFCT, 
         ASApreinjury, OpportunityForImprovement1)

tablereg1$daysinICU <- fct_relevel(tablereg1$daysinICU, "≤ 7 days", "> 7 days")

# Unadjusted Table
table3aalive <- tbl_uvregression(data = tablereg1,
                            method = glm,
                            y = OpportunityForImprovement1,
                            method.args = list(family = binomial),
                            exponentiate = TRUE, 
                            label = list(
                              RTS = "Revised Trauma Score",
                              daysinICU = "Days in the ICU",
                              TimeFInt = "Time to first intervention",
                              ASApreinjury = "ASA preinjury",
                              OnDuty = "On call hours",
                              Intubation = "Mechanical ventilation",
                              TimeFCT = "Time to first CT"
                            )) %>%
  bold_labels() %>%
  bold_p(t = 0.05) 

# Adjusted Table
#Creating linear regression 
adjusted_table1 <- glm(OpportunityForImprovement1 ~ Sex + Age + Intubation + RTS +  ISS + OnDuty + daysinICU + TimeFCT + ASApreinjury, family = binomial, data = tablereg1) 

table3balive <- tbl_regression(adjusted_table1,
                          exponentiate = TRUE, 
                          label = list(RTS = "Revised Trauma Score",
                                       daysinICU = "Days in the ICU",
                                       OnDuty = "On call hours",
                                       Intubation = "Mechanical ventilation",
                                       ASApreinjury = "ASA preinjury",
                                       TimeFCT = "Time to first CT")) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

# Merging Tables
table3alive_merge <- tbl_merge(tbls = list(table3aalive, table3balive),
                          tab_spanner = c("**Unadjusted**", "**Adjusted**")) %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 3. Unadjusted and adjusted logistic regression analyses of associations between patient level factors and opportunities for improvement in patients alive 30 days after hospitalization</div>")

print(table3alive_merge)

