#==============================================================================#
#                         Replication of Main Results                          #
#==============================================================================#

# Load required libraries
library(tidyverse)    # For data handling
library(haven)        # For reading .dta files
library(fixest)       # For fast fixed effects estimation with clustering
library(modelsummary) # For making tables

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
                cluster = ~Sector2)

# Column 2: Add Log_Manufacturing_1911
model2 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_EX_Raw_Food_Trade_17_13 + 
                  Log_Manufacturing_1911, 
                data = industry_data, 
                cluster = ~Sector2)

# Column 3: Add sector fixed effects
model3 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_EX_Raw_Food_Trade_17_13 + 
                  Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                cluster = ~Sector2)

# Column 4: Subset to Manufactures == 1, exclude D_EX_Raw_Food_Trade_17_13
model4 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                subset = ~Manufactures == 1,
                cluster = ~Sector2)

# Column 5: Add D_USA_Trade_21_13
model5 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 + D_USA_Trade_21_13 + 
                  Log_Manufacturing_1911 | Sector2, 
                data = industry_data, 
                subset = ~Manufactures == 1,
                cluster = ~Sector2)

# Columns 6-7: Port-industry

# Load port-level data
port_data <- read_dta("Data/Analysis/Analysis_Data_Portlevel.dta") %>%
  # Rename variable to match industry data
  rename(D_IM_Manu_Trade_17_13 = IM_Manu_Shock_17_13)

# Model 6: Port-level with three-digit industry fixed effects
model6 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 | ThreeDigit, 
                data = port_data, 
                subset = ~OneDigit == "Manufactures",
                cluster = ~ThreeDigit)

# Model 7: Port-level with four-digit industry and port fixed effects
model7 <- feols(Perc_Manufacturing ~ D_IM_Manu_Trade_17_13 | Subdivision + Matchedcategory, 
                data = port_data, 
                subset = ~OneDigit == "Manufactures",
                cluster = ~ThreeDigit)

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
             gof_map = c("nobs", "r.squared"))

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

# Create list to store models
ivr <- list()

# IV regreesion: Any_Response
ivr[[1]] <- feols(Any_Response ~ Indian_Mutiny + Manufacturing_1911 + Military +
                       Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                       Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
                       Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
                       Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
                       Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
                       Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 |
                       D_Manufacturing ~ IM_Manu_Shock_17_13,
                     data = table5, vcov = "hetero")

# IV regreesion: Dummy_KC
ivr[[1]] <- feols(Any_Response ~ Indian_Mutiny + Manufacturing_1911 + Military +
                    Urban_1911 + Coastal + Literacy_Rate + Literacy_Eng + Age_above_20 + 
                    Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 + Census_Dummy_4 +
                    Census_Dummy_5 + Census_Dummy_6 + Census_Dummy_7 + Census_Dummy_8 +
                    Census_Dummy_9 + Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 + 
                    Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 + Census_Dummy_16 + 
                    Census_Dummy_17 + Census_Dummy_18 + Census_Dummy_19 |
                    D_Manufacturing ~ IM_Manu_Shock_17_13,
                  data = table5, vcov = "hetero")



##########################
# Replication of Table 6 #
##########################

