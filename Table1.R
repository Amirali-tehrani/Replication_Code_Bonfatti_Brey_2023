#==============================================================================#
#                            Replication of Table 1                            #
#==============================================================================#

# Load required libraries
library(tidyverse)    # For data handling
library(haven)        # for reading .dta files
library(fixest)       # for fast fixed effects estimation with clustering
library(modelsummary) # For making tables

# Read the data
df <- read_dta("./Data/Analysis/Analysis_Data_Districtlevel.dta")

# Create list to store regression results
models <- list()

# Column 1
models[["(1)"]] <- lm(D_Manufacturing ~ IM_Manu_Shock_17_13 + EX_Raw_Food_Shock_17_13,
                      data = df)

# Column 2
models[["(2)"]] <- lm(D_Manufacturing ~ IM_Manu_Shock_17_13 + EX_Raw_Food_Shock_17_13 +
                        Manufacturing_1911 + Military + Urban_1911 + Coastal +
                        Literacy_Rate + Literacy_Eng + Age_above_20 +
                        Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                        Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                        Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                        Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                        Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                        Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                        Census_Dummy_19,
                      data = df)

# Column 3
models[["(3)"]] <- lm(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                        Military + Urban_1911 + Coastal + Literacy_Rate +
                        Literacy_Eng + Age_above_20 +
                        Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                        Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                        Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                        Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                        Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                        Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                        Census_Dummy_19,
                      data = df)

# Column 4
models[["(4)"]] <- lm(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                        Military + Urban_1911 + Coastal + Literacy_Rate +
                        Literacy_Eng + Age_above_20 + USA_Trade_Shock_21_13 +
                        Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                        Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                        Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                        Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                        Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                        Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                        Census_Dummy_19,
                      data = df)

# Column 5 (with subset condition)
df_subset <- df %>%
  filter(Native_State != "Native" | is.na(Native_State))

models[["(5)"]] <- lm(D_Manufacturing ~ IM_Manu_Shock_17_13 + Manufacturing_1911 +
                        Military + Urban_1911 + Coastal + Literacy_Rate +
                        Literacy_Eng + Age_above_20 +
                        Census_Dummy_1 + Census_Dummy_2 + Census_Dummy_3 +
                        Census_Dummy_4 + Census_Dummy_5 + Census_Dummy_6 +
                        Census_Dummy_7 + Census_Dummy_8 + Census_Dummy_9 +
                        Census_Dummy_10 + Census_Dummy_11 + Census_Dummy_12 +
                        Census_Dummy_13 + Census_Dummy_14 + Census_Dummy_15 +
                        Census_Dummy_16 + Census_Dummy_17 + Census_Dummy_18 +
                        Census_Dummy_19,
                      data = df_subset)

# Function to add clustered standard errors to models
add_clustered_se <- function(model, cluster_var, data_used) {
  cluster_robust_vcov <- vcovCL(model, cluster = data_used[[cluster_var]])
  model$vcov <- cluster_robust_vcov
  return(model)
}

# Add clustered standard errors to all models
models[["(1)"]] <- add_clustered_se(models[["(1)"]], "Census_Division", df)
models[["(2)"]] <- add_clustered_se(models[["(2)"]], "Census_Division", df)
models[["(3)"]] <- add_clustered_se(models[["(3)"]], "Census_Division", df)
models[["(4)"]] <- add_clustered_se(models[["(4)"]], "Census_Division", df)
models[["(5)"]] <- add_clustered_se(models[["(5)"]], "Census_Division", df_subset)

# Create custom gof_map for better formatting
gof_custom <- tibble(
  raw = c("adj.r.squared", "nobs"),
  clean = c("Adjusted R-sq", "N (districts)"),
  fmt = c(3, 0)
)

# Display results in console
modelsummary(models,
             coef_omit = "Census_Dummy|(Intercept)",
             coef_rename = c("IM_Manu_Shock_17_13" = "IM Manufactures Shock",
                             "EX_Raw_Food_Shock_17_13" = "EX Raw and Food Shock",
                             "USA_Trade_Shock_21_13" = "IM Shock (Britain to USA)",
                             "Manufacturing_1911" = "Industrial employment share 1911",
                             "Military" = "Military share 1911",
                             "Urban_1911" = "Urban share 1911",
                             "Coastal" = "Coastal",
                             "Literacy_Rate" = "Literate share 1911",
                             "Literacy_Eng" = "Literate English share 1911",
                             "Age_above_20" = "Age 20+ share 1911"),
             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
             gof_map = gof_custom,
             notes = "Robust standard errors in parentheses clustered on province sub-divisions.",
             title = "Dependent variable: Change industry employment share 1911–1921")

# Export to LaTeX
modelsummary(models,
             output = "./Output/Table1.tex",
             coef_omit = "Census_Dummy|(Intercept)",
             coef_rename = c("IM_Manu_Shock_17_13" = "IM Manufactures Shock",
                             "EX_Raw_Food_Shock_17_13" = "EX Raw and Food Shock", 
                             "USA_Trade_Shock_21_13" = "IM Shock (Britain to USA)",
                             "Manufacturing_1911" = "Industrial employment share 1911",
                             "Military" = "Military share 1911",
                             "Urban_1911" = "Urban share 1911",
                             "Coastal" = "Coastal",
                             "Literacy_Rate" = "Literate share 1911",
                             "Literacy_Eng" = "Literate English share 1911",
                             "Age_above_20" = "Age 20+ share 1911"),
             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
             gof_map = gof_custom,
             notes = "Robust standard errors in parentheses clustered on province sub-divisions.",
             title = "Dependent variable: Change industry employment share 1911–1921",
             fmt = 3,
             escape = FALSE,
             threeparttable = TRUE,
             booktabs = TRUE)


