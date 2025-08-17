#==============================================================================#
#                         Replication of Main Results                          #
#==============================================================================#

# Load required libraries
library(tidyverse)    # For data handling
library(haven)        # For reading .dta files
library(fixest)       # For fast fixed effects estimation with clustering
library(modelsummary) # For making tables

rm(list=ls())

##########################
# Replication of Table 1 #
##########################

# Load data
table1 <- read_dta("./Data/Analysis/Analysis_Data_Districtlevel.dta")

# Create list to store regression results
models <- list()

# Column 1
models[["(1)"]] <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + EX_Raw_Food_Shock_17_13,
                         data = table1, 
                         cluster = ~Census_Division)

# Column 2
models[["(2)"]] <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + EX_Raw_Food_Shock_17_13 +
                           Manufacturing_1911 + Military + Urban_1911 + Coastal +
                           Literacy_Rate + Literacy_Eng + Age_above_20 |
                           Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                           Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                           Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                           Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                           Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                           Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                           Census_Dummy_19,
                         data = table1,
                         cluster = ~Census_Division)

# Column 3
models[["(3)"]] <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                           Military + Urban_1911 + Coastal + Literacy_Rate +
                           Literacy_Eng + Age_above_20 |
                           Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                           Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                           Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                           Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                           Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                           Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                           Census_Dummy_19,
                         data = table1,
                         cluster = ~Census_Division)

# Column 4
models[["(4)"]] <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                           Military + Urban_1911 + Coastal + Literacy_Rate +
                           Literacy_Eng + Age_above_20 + USA_Trade_Shock_21_13 |
                           Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                           Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                           Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                           Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                           Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                           Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                           Census_Dummy_19,
                         data = table1,
                         cluster = ~Census_Division)

# Column 5 (with subset condition)
table1_subset <- table1 %>%
  filter(Native_State != "Native" | is.na(Native_State))

models[["(5)"]] <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                           Military + Urban_1911 + Coastal + Literacy_Rate +
                           Literacy_Eng + Age_above_20 |
                           Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                           Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                           Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                           Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                           Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                           Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                           Census_Dummy_19,
                         data = table1_subset,
                         cluster = ~Census_Division)

# Create custom gof_map for better formatting
gof_custom <- tibble(
  raw = c("adj.r.squared", "nobs"),
  clean = c("Adjusted R-sq", "N (districts)"),
  fmt = c(3, 0)
)

# Order of the variables
coef_map <- c("IM_Manu_Shock_17_13" = "IM Manufactures Shock",
              "EX_Raw_Food_Shock_17_13" = "EX Raw and Food Shock",
              "USA_Trade_Shock_21_13" = "IM Shock (Britain to USA)",
              "Manufacturing_1911" = "Industrial employment share 1911",
              "Military" = "Military share 1911",
              "Urban_1911" = "Urban share 1911",
              "Coastal" = "Coastal",
              "Literacy_Rate" = "Literate share 1911",
              "Literacy_Eng" = "Literate English share 1911",
              "Age_above_20" = "Age 20+ share 1911")

# Display results in console
modelsummary(models,
             coef_map = coef_map,
             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
             gof_map = gof_custom,
             notes = "Robust standard errors in parentheses clustered on province sub-divisions.",
             title = "Dependent variable: Change industry employment share 1911–1921")

# Export to LaTeX
modelsummary(models,
             output = "./Output/Table1.tex",
             coef_map = coef_map,
             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
             gof_map = gof_custom,
             notes = "Robust standard errors in parentheses clustered on province sub-divisions.",
             title = "Dependent variable: Change industry employment share 1911–1921",
             fmt = 3,
             escape = FALSE,
             threeparttable = TRUE,
             booktabs = TRUE)

##########################
# Replication of Table 2 #
##########################

# Columns 1-5: Industry-level

# Load industry-level data
industry_data <- read_dta("./Data/Analysis/Analysis_Data_Industrylevel.dta")

# Column 1: Basic regression
model1 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_EX_Raw_Food_Trade_17_13, 
                data = industry_data,
                vcov=vcov_cluster(~Sector2))

# Column 2: Add Log_Manufacturing_1911
model2 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_EX_Raw_Food_Trade_17_13 + 
                  Log_Manufacturing_1911, 
                data = industry_data, 
                vcov = vcov_cluster(~Sector2))

# Column 3: Add sector fixed effects
model3 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_EX_Raw_Food_Trade_17_13 + 
                  Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                vcov = vcov_cluster(~Sector2))

# Column 4: Subset to Manufactures == 1, exclude D_EX_Raw_Food_Trade_17_13
model4 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                subset = ~Manufactures == 1,
                vcov = vcov_cluster(~Sector2))

# Column 5: Add D_USA_Trade_21_13
model5 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_USA_Trade_21_13 + 
                  Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                subset = ~Manufactures == 1,
                vcov = vcov_cluster(~Sector2))

# Columns 6-7: Port-industry

# Load port-level data
port_data <- read_dta("Data/Analysis/Analysis_Data_Portlevel.dta") %>%
  # Rename variable to match industry data
  rename(D_IM_Manu_Trade_17_13 = IM_Manu_Shock_17_13)

# Model 6: Port-level with three-digit industry fixed effects
model6 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 | ThreeDigit, 
                data = port_data, 
                subset = ~OneDigit == "Manufactures",
                vcov = vcov_cluster(~ThreeDigit))

# Model 7: Port-level with four-digit industry and port fixed effects
model7 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 | Subdivision + Matchedcategory,
                data = port_data,
                subset = ~OneDigit == "Manufactures",
                vcov = vcov_cluster(~ThreeDigit))

# Create list of models for table
models <- list(
  "(1)" = model1,
  "(2)" = model2, 
  "(3)" = model3,
  "(4)" = model4,
  "(5)" = model5,
  "(6)" = model6,
  "(7)" = model7
)

# Order of the variables
coef_map <- c("D_IM_Manu_Trade_17_13" = "IM Manufactures Shock",
              "D_EX_Raw_Food_Trade_17_13" = "EX Raw & Food Shock",
              "D_USA_Trade_21_13" = "IM Shock (UK-USA)",
              "Log_Manufacturing_1911" = "Log Employment 1911")

# Display table in console
modelsummary(models,
             coef_map = coef_map,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_map = c("nobs"))

# Export to LaTeX
modelsummary(models,
             output = "./Output/Table2.tex",
             coef_map = coef_map,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             fmt = 3,
             gof_map = NA,
             statistic = "std.error",
             title = "Dependent variable: Industrial employment growth rate 1911–1921 (multiplied by 100)")

##########################
# Replication of Table 5 #
##########################

# Load data
table5 <- read_dta("./Data/Analysis/Analysis_Data_CivilDisDistrictlevel.dta")

# Drop observations where Native == 1
table5 <- table5 %>% filter(Native != 1)

# First stage of IV for Any_Response
f1 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
              Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
              Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
              Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
              Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
              Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19,
            data = table5,
            vcov = "hetero")

# Second stage of IV for Any_Response
iv1 <- feols(Any_Response ~ Indian_Mutiny + Manufacturing_1911 + Military +
              Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
              Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
              Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
              Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
              Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 |
              D_Manufacturing ~ IM_Manu_Shock_17_13,
            data = table5,
            vcov = "hetero")

# First stage of IV for Dummy_KC
f2 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
              Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
              Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
              Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
              Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
              Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19,
            data = table5,
            vcov = "hetero")

# Second stage of IV for Dummy_KC
iv2 <- feols(Dummy_KC ~ Indian_Mutiny + Manufacturing_1911 + Military +
               Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
               Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
               Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
               Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
               Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 |
               D_Manufacturing ~ IM_Manu_Shock_17_13,
             data = table5,
             vcov = "hetero")

# First stage of IV for Any_Civil_Disobedience
f3 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
              Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
              Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
              Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
              Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
              Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
              Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
              Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
              Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
              Province_16 + Province_17 + Province_18 + Province_19 + Province_20,
            data = table5,
            vcov = "hetero")

# Second stage of IV for Any_Civil_Disobedience
iv3 <- feols(Any_Civil_Disobedience ~ Indian_Mutiny + Manufacturing_1911 + Military +
                    Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                    KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
                    Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
                    Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
                    Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                    Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
                    Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
                    Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
                    Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
                    Province_16 + Province_17 + Province_18 + Province_19 + Province_20 |
                    D_Manufacturing ~ IM_Manu_Shock_17_13,
                  data = table5,
                  vcov = "hetero")

# First stage of IV for Boycott_British_Goods
f4 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
              Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
              Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
              Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
              Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
              Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
              Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
              Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
              Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
              Province_16 + Province_17 + Province_18 + Province_19 + Province_20,
            data = table5,
            vcov = "hetero")

# Second stage of IV Boycott_British_Goods
iv4 <- feols(Boycott_British_Goods ~ Indian_Mutiny + Manufacturing_1911 + Military +
                    Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                    KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
                    Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
                    Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
                    Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                    Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
                    Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
                    Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
                    Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
                    Province_16 + Province_17 + Province_18 + Province_19 + Province_20 |
                    D_Manufacturing ~ IM_Manu_Shock_17_13,
                  data = table5,
                  vcov = "hetero")

# First stage of IV for Boycott_Councils
f5 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
              Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
              Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
              Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
              Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
              Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
              Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
              Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
              Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
              Province_16 + Province_17 + Province_18 + Province_19 + Province_20,
            data = table5,
            vcov = "hetero")

# Second stage of IV for Boycott_Councils
iv5 <- feols(Boycott_Councils ~ Indian_Mutiny + Manufacturing_1911 + Military +
              Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
              KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
              Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
              Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
              Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
              Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
              Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
              Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
              Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
              Province_16 + Province_17 + Province_18 + Province_19 + Province_20 |
              D_Manufacturing ~ IM_Manu_Shock_17_13,
            data = table5,
            vcov = "hetero")

# First stage of IV for Boycott_Educ
f6 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
               Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
               Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
               Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
               Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
               Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
               Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
               Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
               Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
               Province_16 + Province_17 + Province_18 + Province_19 + Province_20,
             data = table5,
             vcov = "hetero")

# Second stage of IV for Boycott_Educ
iv6 <- feols(Boycott_Educ ~ Indian_Mutiny + Manufacturing_1911 + Military +
               Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
               Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
               Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
               Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
               Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
               Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
               Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
               Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
               Province_16 + Province_17 + Province_18 + Province_19 + Province_20 |
               D_Manufacturing ~ IM_Manu_Shock_17_13,
             data = table5,
             vcov = "hetero")

# First stage of IV for Boycott_Courts_Priv_Cases
f7 <- feols(D_Manufacturing ~ IM_Manu_Shock_17_13 + Indian_Mutiny + Manufacturing_1911 +
               Military + Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
               Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
               Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
               Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
               Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
               Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
               Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
               Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
               Province_16 + Province_17 + Province_18 + Province_19 + Province_20,
             data = table5,
             vcov = "hetero")

# Second stage of IV for Boycott_Courts_Priv_Cases
iv7 <- feols(Boycott_Courts_Priv_Cases ~ Indian_Mutiny + Manufacturing_1911 + Military +
               Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               KC + AICC_PCC + Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + 
               Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 +
               Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 +
               Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
               Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 +
               Province_1 + Province_2 + Province_3 + Province_4 + Province_5 +
               Province_6 + Province_7 + Province_8 + Province_9 + Province_10 + 
               Province_11 + Province_12 + Province_13 + Province_14 + Province_15 +
               Province_16 + Province_17 + Province_18 + Province_19 + Province_20 |
               D_Manufacturing ~ IM_Manu_Shock_17_13,
             data = table5,
             vcov = "hetero")

# Calculate F-statistics for first stage
f_stat1 <- round(fitstat(iv1, "ivwald1", simplify = TRUE)$stat, 2)
f_stat2 <- round(fitstat(iv2, "ivwald1", simplify = TRUE)$stat, 2)
f_stat3 <- round(fitstat(iv3, "ivwald1", simplify = TRUE)$stat, 2)
f_stat4 <- round(fitstat(iv4, "ivwald1", simplify = TRUE)$stat, 2)
f_stat5 <- round(fitstat(iv5, "ivwald1", simplify = TRUE)$stat, 2)
f_stat6 <- round(fitstat(iv6, "ivwald1", simplify = TRUE)$stat, 2)
f_stat7 <- round(fitstat(iv7, "ivwald1", simplify = TRUE)$stat, 2)

# Combine F-statistics into vector
f_stats <- c(f_stat1, f_stat2, f_stat3, f_stat4, f_stat5, f_stat6, f_stat7)

# Get first stage coefficients and standard errors
first_stage_coef1 <- round(coef(f1)["IM_Manu_Shock_17_13"], 3)
first_stage_coef2 <- round(coef(f2)["IM_Manu_Shock_17_13"], 3)
first_stage_coef3 <- round(coef(f3)["IM_Manu_Shock_17_13"], 3)
first_stage_coef4 <- round(coef(f4)["IM_Manu_Shock_17_13"], 3)
first_stage_coef5 <- round(coef(f5)["IM_Manu_Shock_17_13"], 3)
first_stage_coef6 <- round(coef(f6)["IM_Manu_Shock_17_13"], 3)
first_stage_coef7 <- round(coef(f7)["IM_Manu_Shock_17_13"], 3)

first_stage_coefs <- c(first_stage_coef1, first_stage_coef2, first_stage_coef3, 
                       first_stage_coef4, first_stage_coef5, first_stage_coef6, first_stage_coef7)

# Get first stage standard errors
first_stage_se1 <- round(sqrt(vcov(f1)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se2 <- round(sqrt(vcov(f2)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se3 <- round(sqrt(vcov(f3)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se4 <- round(sqrt(vcov(f4)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se5 <- round(sqrt(vcov(f5)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se6 <- round(sqrt(vcov(f6)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)
first_stage_se7 <- round(sqrt(vcov(f7)["IM_Manu_Shock_17_13", "IM_Manu_Shock_17_13"]), 3)

first_stage_ses <- c(first_stage_se1, first_stage_se2, first_stage_se3, 
                     first_stage_se4, first_stage_se5, first_stage_se6, first_stage_se7)

# Create complete etable
etable(iv1, iv2, iv3, iv4, iv5, iv6, iv7,
       
       # Variable labels dictionary
       dict = c("fit_D_Manufacturing" = "Diff. Industrial share 1911-1921",
                "Indian_Mutiny" = "Mutiny 1857",
                "KC" = "Khilafat member",
                "AICC_PCC" = "PCC or AICC member",
                "Manufacturing_1911" = "Industrial employment share 1911",
                "Military" = "Military share 1911", 
                "Urban_1911" = "Urban share 1911",
                "Coastal" = "Coastal",
                "Literacy_Rate" = "Literate share 1911",
                "Literacy_Eng" = "Literate English share 1911",
                "Age_above_20" = "Age 20+ share 1911"),
       
       # Column headers
       headers = c("Enquiry response", 
                   "KC member response",
                   "For immediate civil disobedience",
                   "British products",
                   "For boycott of Legislative councils",
                   "British education",
                   "British courts"),
       
       # Table title
       title = "Dependent variable: Share of interviewees in favour of reported action",
       
       # Keep only relevant variables (hide census and province dummies)
       keep = c("fit_D_Manufacturing", "Indian_Mutiny", "Manufacturing_1911", 
                "Military", "Urban_1911", "Coastal", "Literacy_Rate", 
                "Literacy_Eng", "Age_above_20", "KC", "AICC_PCC"),
       
       fixef_sizes = TRUE,
       fixef_sizes.simplify = TRUE,
       
       extralines = list(
         "Province FE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
         "INC FE" = c("No", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
         "F-stat (1st stage)" = f_stats,
         "First Stage" = paste0(first_stage_coefs, " (", first_stage_ses, ")")
       ),
       
       # Output to LaTeX file
       tex = TRUE,
       file = "./Output/Table5.tex",
       replace = TRUE,

       # Formatting options
       se.below = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       digits = 3)

##########################
# Replication of Table 6 #
##########################

# Load data
table6 <- read_dta("./Data/Analysis/Analysis_Data_ElectionConstituencylevel.dta")

# First stage of IV
f1 <- feols(D_Manufacturing_FR_1913_36 ~ IM_Manu_Shock_17_13 + Indian_Mutiny +
              Manufacturing_1911 + Military + Urban_1911 + Coastal + Literacy_Rate +
              Literacy_Eng + Age_above_20 + Census_Dummy_1 + Census_Dummy_2 + 
              Census_Dummy_3 + Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 + 
              Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 + Census_Dummy_10 + 
              Census_Dummy_11 + Census_Dummy_12 + Census_Dummy_13 + Census_Dummy_14 + 
              Census_Dummy_15 + Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 + 
              Census_Dummy_19 + General_Urban,
            data = table6,
            subset = table6$Muhammadan != 1 & table6$NoofSeats == 1,
            vcov = "hetero")

# Second stage of IV for Congress_Winner
iv1 <- feols(Congress_Winner ~ Indian_Mutiny + Manufacturing_1911 + Military +
               Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
               Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
               Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
               Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
               Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 + General_Urban |
               D_Manufacturing_FR_1913_36 ~ IM_Manu_Shock_17_13,
             data = table6,
             subset = table6$Muhammadan != 1 & table6$NoofSeats == 1,
             vcov = "hetero")

# Second stage of IV for Unionist_Winner
iv2 <- feols(Unionist_Winner ~ Indian_Mutiny + Manufacturing_1911 + Military +
               Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
               Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
               Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
               Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
               Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
               Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 + General_Urban |
               D_Manufacturing_FR_1913_36 ~ IM_Manu_Shock_17_13,
             data = table6,
             subset = table6$Muhammadan != 1 & table6$NoofSeats == 1,
             vcov = "hetero")

# Second stage of IV for Independent_Winner
iv3 <- feols(Independent_Winner ~ Indian_Mutiny + Manufacturing_1911 + Military +
                    Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                    Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
                    Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
                    Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
                    Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
                    Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 + General_Urban |
                    D_Manufacturing_FR_1913_36 ~ IM_Manu_Shock_17_13,
                  data = table6,
                  subset = table6$Muhammadan != 1 & table6$NoofSeats == 1,
                  vcov = "hetero")

# Second stage of IV for Other_Winner
iv4 <- feols(Other_Winner ~ Indian_Mutiny + Manufacturing_1911 + Military +
                    Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                    Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
                    Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
                    Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
                    Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
                    Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 + General_Urban |
                    D_Manufacturing_FR_1913_36 ~ IM_Manu_Shock_17_13,
                  data = table6,
                  subset = table6$Muhammadan != 1 & table6$NoofSeats == 1,
                  vcov = "hetero")

# Calculate F-statistics for first stage
f_stat <- round(fitstat(iv1, "ivwald1", simplify = TRUE)$stat, 2)

# Coefficient and SE on the instrument from first stage
first_stage_coef <- round(coef(f1)["IM_Manu_Shock_17_13"], 3)
first_stage_se <- round(se(f1)["IM_Manu_Shock_17_13"], 3)

# Create first stage display with coefficient and SE in parentheses
first_stage_display <- paste0(first_stage_coef, "***", " (", first_stage_se, ")")

modelsummary(list(iv1, iv2, iv3, iv4))

# Create LaTeX table
etable(iv1, iv2, iv3, iv4,
       keep = c("D_Manufacturing_FR_1913_36",
                "Indian_Mutiny",
                "Manufacturing_1911", 
                "Military",
                "Urban_1911",
                "Coastal",
                "Literacy_Rate", 
                "Literacy_Eng",
                "Age_above_20",
                "General_Urban"),
       dict = c("D_Manufacturing_FR_1913_36" = "Diff. Industry share 1913–1936",
                "Indian_Mutiny" = "Mutiny 1857",
                "Manufacturing_1911" = "Industrial employment share 1911",
                "Military" = "Military share 1911",
                "Urban_1911" = "Urban share 1911",
                "Coastal" = "Coastal",
                "Literacy_Rate" = "Literate share 1911",
                "Literacy_Eng" = "Literate English share 1911",
                "Age_above_20" = "Age 20+ share 1911",
                "General_Urban" = "Urban constituency"),
       headers = list("(1)" = "Congress",
                      "(2)" = "Unionist", 
                      "(3)" = "Independents",
                      "(4)" = "Other"),
       title = "Dependent variable: Seats won by reported party",
       extralines = list("Province FE" = rep("Yes", 4),
                         "F-stat (1st stage)" = rep(f_stat, 4),
                         "First Stage" = rep(first_stage_display, 4),
                         "N (constituencies)" = rep("335", 4)),
       file = "./Output/Table6.tex",
       replace = TRUE,
       se.below = TRUE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       digits = 3)
