# Install packages
install.packages("tidyverse")
install.packages("haven")
install.packages("janitor")
install.packages("patchwork")
install.packages("e1071")
install.packages("readr")

# Load libraries
library(tidyverse)
library(haven)
library(janitor)
library(patchwork)
library(e1071)
library(readr)

# Set data directory
data_dir <- "C:\Users\atehr\Dropbox\Uni\His&Dev/Replication_BonfattiBrey2023/Data/Datasets"

# Helper: parse to numeric
to_num <- function(x) as.numeric(readr::parse_number(as.character(x)))

# Helper: pick first matching column name
pick_first <- function(df, candidates, label) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) stop("None of the candidate columns for ", label, " found. Looked for: ",
                         paste(candidates, collapse = ", "), "\nAvailable: ",
                         paste(names(df), collapse = ", "))
  hit[1]
}

# Helper: percentile range
rng <- function(x, p = c(.01, .99)) stats::quantile(x, p, na.rm = TRUE)

# Helper: annotation text for plots
annot_text <- function(x) paste0(
  "N=", length(x),
  "   mean=", round(mean(x, na.rm=TRUE), 3),
  "   sd=", round(sd(x, na.rm=TRUE), 3),
  "   skew=", round(e1071::skewness(x, na.rm=TRUE), 3)
)

# Helper: plotting theme
theme_pub <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Load raw datasets
lshares <- read_dta(file.path(data_dir, "Misc_Data/Lshares.dta")) %>% clean_names()
shocks  <- read_dta(file.path(data_dir, "Misc_Data/shocks.dta"))  %>% clean_names()
ind1911 <- read_dta(file.path(data_dir, "Census_Data/India_Industry_1911.dta")) %>% clean_names()
ind1921 <- read_dta(file.path(data_dir, "Census_Data/India_Industry_1921.dta")) %>% clean_names()
pop1911 <- read_dta(file.path(data_dir, "Census_Data/India_Pop_1911.dta")) %>% clean_names()
pop1921 <- read_dta(file.path(data_dir, "Census_Data/India_Pop_1921.dta")) %>% clean_names()
xwalk   <- read_dta(file.path(data_dir, "Xwalk/xwalk_district.dta")) %>% clean_names()

# Auto-detect key columns and weights
x_d_can  <- pick_first(xwalk,  c("district"),       "xwalk canonical district")
x_d_1911 <- pick_first(xwalk,  c("district_1911"),  "xwalk district_1911")
x_d_1921 <- pick_first(xwalk,  c("district_1921"),  "xwalk district_1921")
w1911    <- pick_first(xwalk,  c("weight_1911"),    "xwalk weight_1911")
w1921    <- pick_first(xwalk,  c("weight_1921"),    "xwalk weight_1921")

# Auto-detect population keys/values
p1911_key <- pick_first(pop1911, c("district_1911","district"), "pop1911 district key")
p1921_key <- pick_first(pop1921, c("district_1921","district"), "pop1921 district key")
pop1911_val <- pick_first(pop1911, c("person","person_1911","persons_1911","population"), "pop1911 population")
pop1921_val <- pick_first(pop1921, c("person","person_1921","person_1911_boundary1921"),   "pop1921 population")

# Auto-detect industry keys
i1911_key <- pick_first(ind1911, c("district_1911","district"), "ind1911 district key")
i1921_key <- pick_first(ind1921, c("district_1921","district"), "ind1921 district key")

# Build crosswalk with numeric weights
xwalk_w <- xwalk %>%
  transmute(
    district        = .data[[x_d_can]],
    district_1911   = .data[[x_d_1911]],
    district_1921   = .data[[x_d_1921]],
    weight_1911     = to_num(.data[[w1911]]),
    weight_1921     = to_num(.data[[w1921]])
  )

# Aggregate population 1911 to canonical districts
pop1911_h <- pop1911 %>%
  transmute(yr_key = .data[[p1911_key]], person = to_num(.data[[pop1911_val]])) %>%
  group_by(yr_key) %>% summarise(person = sum(person, na.rm = TRUE), .groups = "drop") %>%
  inner_join(xwalk_w %>% select(district, district_1911, weight_1911),
             by = c("yr_key" = "district_1911")) %>%
  filter(!is.na(district), district != ".") %>%
  group_by(district) %>% summarise(pop1911 = sum(weight_1911 * person, na.rm = TRUE), .groups = "drop")

# Aggregate population 1921 to canonical districts
pop1921_h <- pop1921 %>%
  transmute(yr_key = .data[[p1921_key]], person_1921 = to_num(.data[[pop1921_val]])) %>%
  group_by(yr_key) %>% summarise(person_1921 = sum(person_1921, na.rm = TRUE), .groups = "drop") %>%
  inner_join(xwalk_w %>% select(district, district_1921, weight_1921),
             by = c("yr_key" = "district_1921")) %>%
  filter(!is.na(district), district != ".") %>%
  group_by(district) %>% summarise(pop1921 = sum(weight_1921 * person_1921, na.rm = TRUE), .groups = "drop")

# Aggregate industrial employment 1911
emp1911 <- ind1911 %>%
  transmute(yr_key = .data[[i1911_key]],
            male_emp_1911   = to_num(male_emp_1911),
            female_emp_1911 = to_num(female_emp_1911),
            emp_1911 = coalesce(male_emp_1911,0) + coalesce(female_emp_1911,0)) %>%
  group_by(yr_key) %>% summarise(emp_1911 = sum(emp_1911, na.rm = TRUE), .groups = "drop") %>%
  inner_join(xwalk_w %>% select(district, district_1911, weight_1911),
             by = c("yr_key" = "district_1911")) %>%
  filter(!is.na(district), district != ".") %>%
  group_by(district) %>% summarise(industryemp1911 = sum(weight_1911 * emp_1911, na.rm = TRUE), .groups = "drop")

# Aggregate industrial employment 1921
emp1921 <- ind1921 %>%
  transmute(yr_key = .data[[i1921_key]],
            male_emp_1921   = to_num(male_emp_1921),
            female_emp_1921 = to_num(female_emp_1921),
            emp_1921 = coalesce(male_emp_1921,0) + coalesce(female_emp_1921,0)) %>%
  group_by(yr_key) %>% summarise(emp_1921 = sum(emp_1921, na.rm = TRUE), .groups = "drop") %>%
  inner_join(xwalk_w %>% select(district, district_1921, weight_1921),
             by = c("yr_key" = "district_1921")) %>%
  filter(!is.na(district), district != ".") %>%
  group_by(district) %>% summarise(industryemp1921 = sum(weight_1921 * emp_1921, na.rm = TRUE), .groups = "drop")

# Build shift–share import shock per person (1911, £1911)
lshares_1911 <- lshares %>%
  mutate(year = to_num(year),
         ind_share = to_num(ind_share),
         districtman = to_num(district_man)) %>%
  filter(year == 1911)

# Filter shocks for manufactures in 1911
shocks_m <- shocks %>%
  mutate(manufactures = to_num(manufactures),
         year = to_num(year),
         d_im_manu_trade_17_13 = to_num(d_im_manu_trade_17_13)) %>%
  filter(manufactures == 1, year == 1911)

# Merge lshares with shocks
merge_shock <- lshares_1911 %>%
  inner_join(shocks_m, by = "matchedcategory") %>%
  filter(!is.na(district), district != ".", !is.na(ind_share), !is.na(d_im_manu_trade_17_13))

# Aggregate to district and scale per person
shock_d <- merge_shock %>%
  group_by(district) %>%
  summarise(
    shock_per_worker = sum(ind_share * d_im_manu_trade_17_13, na.rm = TRUE),
    districtman      = max(districtman, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(pop1911_h, by = "district") %>%
  mutate(im_shock_pp_1911 = shock_per_worker * (districtman / pop1911)) %>%
  select(district, im_shock_pp_1911)

# Compute change in industrial employment share (1911→1921)
ind_change <- emp1911 %>%
  inner_join(emp1921,   by = "district") %>%
  inner_join(pop1911_h, by = "district") %>%
  inner_join(pop1921_h, by = "district") %>%
  mutate(
    ind_share_1911     = industryemp1911 / pop1911,
    ind_share_1921     = industryemp1921 / pop1921,
    d_ind_share_111921 = ind_share_1921 - ind_share_1911
  ) %>%
  select(district, d_ind_share_111921)

# Build final analysis dataset
final_df <- shock_d %>% inner_join(ind_change, by = "district")

# Plot: import shock distributions (full, zoom, asinh)
sh <- final_df$im_shock_pp_1911
lims_sh <- rng(sh, c(.01,.99))

# Plot: import shock (full range)
p_sh_full <- ggplot(final_df, aes(im_shock_pp_1911)) +
  geom_histogram(aes(y = after_stat(density)), bins = nclass.FD(sh), linewidth=.2) +
  geom_density(adjust = 1.2, linewidth=.6) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = "Import–Manufactures Shock (Full range)",
       x = "£1911 per person", y = "Density") +
  theme_pub

# Plot: import shock (zoomed 1–99%)
p_sh_zoom <- ggplot(final_df, aes(im_shock_pp_1911)) +
  geom_histogram(aes(y = after_stat(density)), bins = nclass.FD(sh), linewidth=.2) +
  geom_density(adjust = 1.2, linewidth=.6) +
  geom_vline(xintercept = 0, linetype = 2) +
  coord_cartesian(xlim = lims_sh) +
  labs(title = "Import–Manufactures Shock (Zoomed 1–99%)",
       x = "£1911 per person", y = "Density",
       caption = "Display zoom only. Data are raw (no winsorization).") +
  theme_pub +
  annotate("label", x = lims_sh[2], y = Inf, hjust = 1, vjust = 1.2,
           label = annot_text(sh), size = 3)

# Plot: import shock (asinh transform)
p_sh_asinh <- ggplot(final_df, aes(x = asinh(im_shock_pp_1911))) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, linewidth=.2) +
  geom_density(adjust = 1.2, linewidth=.6) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(labels = function(z) scales::number(sinh(z), accuracy = 0.01)) +
  labs(title = "Import–Manufactures Shock",
       x = "£1911 per person", y = "Density") +
  theme_pub

# Print import shock plots
(p_sh_full | p_sh_zoom); p_sh_asinh

# Plot: change in industrial employment share
ds <- final_df$d_ind_share_111921
lims_ds <- rng(ds, c(.01,.99))

# Plot: change in industrial share (zoomed)
p_ds_zoom <- ggplot(final_df, aes(d_ind_share_111921)) +
  geom_histogram(aes(y = after_stat(density)), bins = nclass.FD(ds), linewidth=.2) +
  geom_density(adjust = 1.2, linewidth=.6) +
  coord_cartesian(xlim = lims_ds) +
  labs(title = "Change in Industrial Employment Share (1911–1921)",
       subtitle = "Zoomed (1–99%) for readability; raw data",
       x = "share (pp of population)", y = "Density",
       caption = "Note: 1921 threshold (>10 vs >20 workers) may inflate right tail.") +
  theme_pub +
  annotate("label", x = lims_ds[2], y = Inf, hjust = 1, vjust = 1.2,
           label = annot_text(ds), size = 3)

# Plot: violin/box for change in industrial share (3:1 stack)
p_ds_strip <- ggplot(final_df, aes(y = d_ind_share_111921, x = 1)) +
  geom_violin(width = .35, fill = "grey92") +
  geom_boxplot(width = .12, outlier.alpha = .3) +
  coord_flip(ylim = lims_ds) +
  labs(x = NULL, y = NULL) +
  theme_pub + theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    panel.grid = element_blank())

# Print change-in-share plots with 3:1 height ratio
p_ds_zoom / p_ds_strip + plot_layout(heights = c(3,1))
