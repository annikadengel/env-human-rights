library(tidyverse)
library(ggplot2)
library(haven)
library(skimr)
library(WDI)
library(lfe)
library(mice)
library(countrycode)
library(DataExplorer)
library(corrr)
library(caret)
library(marginaleffects)
library(margins) 
library(fixest)
library(dplyr)
library(e1071)
library(ggeffects)
library(extrafont)
library(car)
library(did)
library(sandwich)
library(lmtest)
library(did2s)
library(broom)
library(gsynth)
library(rlang)
library(randomForest) 
library(readxl)
library(sf)
library(rworldmap)
library(gridExtra)

#install.packages("miceadds")
#font_import(prompt = FALSE)

# package citations
all_packages <- c(
  "tidyverse", "ggplot2", "haven", "skimr", "WDI", "lfe", "mice", "miceadds",
  "countrycode", "DataExplorer", "corrr", "caret", "marginaleffects", "margins",
  "fixest", "dplyr", "e1071", "ggeffects", "extrafont", "car", "did", "sandwich",
  "lmtest", "did2s", "broom", "gsynth", "rlang", "randomForest", "readxl", "sf",
  "rworldmap", "gridExtra"
)
for (pkg in all_packages) {
  citation_info <- citation(pkg)
  print(citation_info)
}

# -------------------------------------------------------------
# data from quality of governance environmental indicators
# -------------------------------------------------------------
original_qog_data <- read_dta("/Users/annikadengel/Downloads/qog_ei_ts_sept21.dta")



# -------------------------------------------------------------
# create a main panel dataset
# -------------------------------------------------------------
years <- 1970:2015 # earliest recognizer in 1970s

state_cap_index <- read_dta("/Users/annikadengel/Desktop/StateCapacityDataset_v1.dta")

head(state_cap_index)
table(state_cap_index$ccode, useNA = "ifany")
unique(state_cap_index$country[is.na(state_cap_index$ccode)]) # countries missing codes are bahamas, barbados, belize, brunei, grenada, iceland, luxembourg, maldives, malta, samoa, seychelles, st. lucia, and vanuatu

state_cap_index <- state_cap_index |>
  mutate(ccode = case_when( # from the correlates of war package: https://www.jkarreth.net/countrycodes.html
    country == "Bahamas"     ~ 31,
    country == "Barbados"    ~ 53,
    country == "Belize"      ~ 80,
    country == "Brunei"      ~ 835,
    country == "Grenada"     ~ 55,
    country == "Iceland"     ~ 395,
    country == "Luxembourg"  ~ 212,
    country == "Maldives"    ~ 781,
    country == "Malta"       ~ 338,
    country == "Samoa"       ~ 990,  
    country == "Seychelles"  ~ 591,
    country == "St. Lucia"   ~ 56,
    country == "Vanuatu"     ~ 935,
    TRUE ~ ccode  # retain existing codes
  ))

checking_codes <- state_cap_index |> # after printing they correspond w/ the right codes
  group_by(country, ccode) |>
  summarize(m_c = mean(Capacity))

countries_cowcodes <- unique(state_cap_index$ccode)

panel_data <- expand.grid(ccodecow = countries_cowcodes, year = years)

# caused by warning:
#! some values were not matched unambiguously: 260, 342, 348, 364, 525, 529, 769, 818 

# > unique(state_cap_index$country[state_cap_index$ccode %in% c(260, 342, 348, 525, 529, 769, 818)])
#[1] "Ethiopia"      "Germany, West" "Pakistan"      "Vietnam"       "Serbia"       
#[6] "Montenegro"    "South Sudan"

# there are some wrong codes in the state_cap_index dataset -> this fixes them
panel_data <- panel_data |>
  mutate(ccodecow = case_when(
    ccodecow == 529 ~ 530,
    ccodecow == 348 ~ 341,
    ccodecow == 525 ~ 626,
    ccodecow == 769 ~ 770,
    ccodecow == 818 ~ 816,
    ccodecow == 342 ~ 345,
    TRUE ~ ccodecow
  ))

# fixing duplicates that arise from recoding
panel_data <- panel_data |>
  distinct(ccodecow, year, .keep_all = TRUE)

#caused by warning:
#! some values were not matched unambiguously: 260, 364 -> these are west germany and the soviet union

panel_data <- panel_data |>
  mutate(country = countrycode(ccodecow, origin = "cown", destination = "country.name")) |>
  filter(!(ccodecow %in% c(260, 364, 265)))

table(panel_data$country, useNA = "ifany")

#view(panel_data)

# add constitutional recognition of ehrs variable
ehr_years <- read_csv("/Users/annikadengel/Desktop/ehr recognition years.csv")
head(ehr_years)

ehr_years <- ehr_years |>
  mutate(ccodecow = countrycode(country, origin = "country.name", destination = "cown"))

ehr_years <- ehr_years |> 
  mutate(ccodecow = case_when(
    country == "Serbia" ~ 345,
    TRUE ~ ccodecow
  ))

unique(ehr_years$ccodecow)

ehr_years <- ehr_years |>
  dplyr::select(-country)

panel_data <- panel_data |>
  left_join(ehr_years, by = c("ccodecow")) 

panel_data <- panel_data |>
  mutate(
    ehr_recognized = if_else(ehr_year != 0 & year >= ehr_year, 1, 0)
  )

#view(panel_data)

unique(panel_data$country)

skim(panel_data)



# -------------------------------------------------------------
# data from state capacity index (hsi) by hanson and sigman
# here's their article about it -> https://websites.umich.edu/~jkhanson/resources/hanson_sigman21.pdf
# -------------------------------------------------------------
#view(state_cap_index) # data stretches from 1960-2015

state_cap_index <- state_cap_index |>
  dplyr::select(-country)

panel_data <- panel_data |>
  left_join(state_cap_index, by = c("ccodecow" = "ccode", "year"))

skim(panel_data) # state capacity variable @ a complete rate of 77%

panel_data_check1 <- panel_data |>
  group_by(country) |>
  summarize(mean_cap = mean(Capacity, na.rm = TRUE))

#view(panel_data_check1) # some countries are missing the variable entirely...
panel_data_check1$country[is.na(panel_data_check1$mean_cap)]

# countries missing all values for capacity in the dataset -> will drop them
# [1] "Bahamas"    "Barbados"   "Belize"     "Brunei"     "Grenada"    "Iceland"    "Luxembourg" "Maldives"  
# [9] "Malta"      "Samoa"      "Seychelles" "St. Lucia"  "Vanuatu"

missing_all_values <- c("Bahamas", "Barbados", "Belize", "Brunei", "Grenada", "Iceland", "Luxembourg",
                        "Maldives", "Malta", "Samoa", "Seychelles", "St. Lucia", "Vanuatu")

colnames(panel_data)

panel_data <- panel_data |>
  rename("capacity" = "Capacity", "capacity_sd" = "Capacity_sd", "vdem" = "VDem", "wbregion" = "WBregion", "adm_effic" = "AdmEffic", "state_hist_50s" = "StateHist50s") |> 
  filter(!(country %in% missing_all_values)) |>
  dplyr::select(-iso3, -iso2, -scode)
  
skim(panel_data) # now capacity's completion rate is 83% -> not bad!



# -------------------------------------------------------------
# data from world bank development indicators
# -------------------------------------------------------------
indicators <- c(
  "AG.LND.AGRI.ZS",    # agricultural land (% of land area)
  "AG.LND.CREL.HA",    # land under cereal production (hectacres)
  "AG.YLD.CREL.KG",    # cereal yield (kg per hectare)
  "NV.AGR.EMPLY.KD",   # agriculture, value added per worker (constant 2010 us$)
  "AG.LND.PRCP.MM",    # avg percipitation in depth (mm per year)
  "EG.EGY.PRIM.PP.KD", # energy intensity level of primary energy (mj/$2017 ppp gdp)
  "EN.ATM.CO2E.PC",    # co2 emissions (metric tons per capita) -> said it could not be downloaded
  "EN.ATM.PM25.MC.M3", # pm2.5 air pollution, mean annual exposure (micrograms per cubic meter)
  "EN.ATM.PM25.MC.ZS", # pm2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)
  "EG.FEC.RNEW.ZS",    # renewable energy consumption (% of total final energy consumption)*
  "EG.ELC.RNEW.ZS",    # renewable electricity output (% of total electricity output)*
  "EG.ELC.ACCS.ZS",    # access to electricity (% of population)
  "AG.LND.FRST.ZS",    # forest area (% of land area)*
  "NY.GDP.TOTL.RT.ZS", # total natural resources rents (% of gdp)
  "ER.LND.PTLD.ZS",    # terrestrial protected areas (% of total land area)
  "ER.PTD.TOTL.ZS",    # terrestrial and marine protected areas (% of total territorial area)
  "ER.MRN.PTMR.ZS",    # marine protected areas (% of territorial waters)
  "NY.GDP.MKTP.CD",    # gdp (current us$)*
  "NY.GDP.MKTP.KD.ZG", # gdp growth (annual %)*
  "NY.GNP.PCAP.CD",    # gni per capita, atlas method (current us$)*
  "NY.GNP.PCAP.PP.CD", # gni per capita, ppp (current international $)
  "NV.AGR.TOTL.KD.ZG", # agricultural value added (annual % growth)
  "NV.IND.TOTL.KD.ZG", # industry, value added (annual % growth)
  "NV.IND.TOTL.ZS",    # industry, value added (% of gdp)
  "NE.EXP.GNFS.ZS",    # exports of goods and services (% of gdp)
  "NE.IMP.GNFS.ZS",    # imports of goods and services (% of gdp)
  "SP.POP.TOTL",       # population, total
  "SP.POP.GROW"        # population growth (annual %)
)

# *indicators w/ better completion rates

wdi_data = WDI(country = "all", indicator = indicators, start = 1960, end = 2015, extra = FALSE)
skim(wdi_data)
colnames(wdi_data)

wdi_data <- wdi_data |>
  mutate(ccodecow = countrycode(country, origin = "country.name", destination = "cown"))

unique(wdi_data$country[is.na(wdi_data$ccodecow)]) 

wdi_data <- wdi_data |>
  filter(!(is.na(wdi_data$ccodecow))) |>
  dplyr::select(-country)

panel_data <- panel_data |>
  left_join(wdi_data, by = c("ccodecow", "year"))

panel_data <- panel_data |>
  dplyr::select(-vdem, -iso2c, -iso3c, -cntrynum)

colnames(panel_data)
skim(panel_data)



# -------------------------------------------------------------
# creating the environmental index -> imputation and principal component analysis 
# -------------------------------------------------------------
# these are the indicators in the index: 
# wdi_co2 - co2 emissions (metric tons per capita) 
# wdi_fossil - fossil fuel energy consumption (% of total) 
# edgar_pm25 - pm 2.5 emissions
# EG.ELC.RNEW.ZS - renewable electricity output (% of total electricity output)


# part I: creating a dataset for imputation
# -------------------------------------------------------------
# include: variables with high correlation with at least one of the variables
# don't include: identifiers, high collinearity, identifiers, high missingness

target_vars <- c("wdi_fossil", "edgar_pm25", "EG.ELC.RNEW.ZS", "wdi_co2")

#head(original_qog_data)
#head(wdi_data)
#head(vdem_data)

#countries_list <- unique(panel_data$ccodecow)
#length(countries_list)

# function to filter data for correlated variables/collinear variables/id
filter_dataset_by_correlation <- function(data, 
                                          na_threshold = 0.65, 
                                          corr_threshold = 0.3,
                                          collinear_threshold = 0.95,
                                          output_name = "filtered_data") {
  require(dplyr)
  
  # always retain these columns if present
  id_vars <- intersect(c("ccodecow", "year"), names(data))
  
  # drop columns with too much missingness
  na_prop <- colMeans(is.na(data))
  data <- data[, na_prop <= na_threshold]
  
  # subset to numeric variables only (excluding ID vars first)
  num_data <- data[, sapply(data, is.numeric) & !(names(data) %in% id_vars)]
  
  # remove columns with no variance or insufficient data
  constant_or_insufficient <- sapply(num_data, function(x) {
    valid_x <- x[!is.na(x)]
    length(valid_x) < 2 || sd(valid_x) == 0
  })
  num_data <- num_data[, !constant_or_insufficient]
  
  # remove duplicate columns
  num_data <- num_data[, !duplicated(as.data.frame(t(num_data)))]
  
  # compute correlation matrix
  cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
  
  # keep variables moderately correlated with target variables
  keep_vars <- unique(unlist(lapply(target_vars, function(var) {
    if (var %in% rownames(cor_matrix)) {
      names(which(abs(cor_matrix[var, ]) > corr_threshold))
    } else {
      NULL
    }
  })))
  
  # identify variables that are collinear with target variables
  collinear_vars <- unique(unlist(lapply(target_vars, function(var) {
    if (var %in% rownames(cor_matrix)) {
      names(which(abs(cor_matrix[var, ]) > collinear_threshold))
    } else {
      NULL
    }
  })))
  
  # final variable set: keep_vars minus collinear vars and target vars
  final_vars <- setdiff(keep_vars, union(collinear_vars, target_vars))
  
  # recombine with id variables
  final_cols <- c(id_vars, final_vars)
  final_cols <- intersect(final_cols, names(data))  # Ensure all exist
  filtered_data <- data[, final_cols, drop = FALSE]
  
  # assign to global environment
  assign(output_name, filtered_data, envir = .GlobalEnv)
  
  cat("saved", ncol(filtered_data), "variables to global environment as", output_name, "\n")
  return(invisible(filtered_data))
}

# applying function to original_qog_data
filter_dataset_by_correlation(
  original_qog_data,
  output_name = "filtered_qog_data"
) # retained 38 variables 

# applying function to wdi_data -> could always download more indicators later
filter_dataset_by_correlation(
  wdi_data,
  output_name = "filtered_wdi_data"
) # retained 2 variables

# checking to see if there's duplicate columns before merging -> function was run in console
print_shared_columns <- function(df1, df2) {
  shared_cols <- intersect(names(df1), names(df2))
  if (length(shared_cols) == 0) {
    cat("no shared columns found.\n")
  } else {
    cat("shared columns:\n")
    print(shared_cols)
  }
}

# joining the datasets
env_data <- filtered_wdi_data |>
  left_join(filtered_qog_data, by = c("ccodecow", "year"))

#skim(env_data)

# need to re-add in target variables
target_var_data <- original_qog_data |>
  dplyr::select(ccodecow, year, wdi_co2, edgar_pm25, wdi_co2, wdi_fossil) #

env_data <- env_data |>
  left_join(target_var_data, by = c("ccodecow", "year"))

wdi_renew <- wdi_data |>
  dplyr::select(ccodecow, year, EG.ELC.RNEW.ZS)

env_data <- env_data |>
  left_join(wdi_renew, by = c("ccodecow", "year"))

head(env_data)
colnames(env_data)
#skim(env_data)

# attempting imputation w/ varying years of recognition -> earliest year is 1975
env_data <- env_data |>
  filter(year >= 1970 & year <= 2015)


# part II: preparing the dataset
# -------------------------------------------------------------
# one more debugging session before imputation
impute_data <- env_data

# checking correlations
impute_data |>
  dplyr::select(where(is.numeric)) |>
  correlate() |>
  focus(wdi_fossil) |>
  arrange(desc(abs(wdi_fossil))) 

impute_data |>
  dplyr::select(where(is.numeric)) |>
  correlate() |>
  focus(edgar_pm25) |>
  arrange(desc(abs(edgar_pm25))) 

# checking constants / variance
nearZeroVar(impute_data, saveMetrics = TRUE) # "zeroVar" and "nzv" are false and false for all

# checking the skews
# define skew threshold (> 1 or < -1 is usually considered highly skewed)
skew_threshold <- 1
numeric_predictors <- sapply(impute_data, is.numeric)

for (var in names(numeric_predictors[numeric_predictors])) {
  x <- impute_data[[var]]
  
  # only calculate skew on non-missing values
  skew_val <- skewness(x, na.rm = TRUE)
  
  if (skew_val > skew_threshold) {
    if (all(x >= 0, na.rm = TRUE)) {
      message(paste("right-skewed:", var, "- applying log1p()"))
      impute_data[[var]] <- log1p(x)
    } else {
      warning(paste("skipped", var, "- contains negative values"))
    }
    
  } else if (skew_val < -skew_threshold) {
    max_val <- max(x, na.rm = TRUE)
    if (all(max_val + 1 - x > 0, na.rm = TRUE)) {
      message(paste("left-skewed:", var, "- reflecting and applying log1p()"))
      impute_data[[var]] <- log1p(max_val + 1 - x)
    } else {
      warning(paste("skipped", var, "- reflection would produce invalid values"))
    }
    
  } else {
    message(paste("not highly skewed:", var, "- no transformation"))
  }
}


# part III: imputation using 2l.pan
# -------------------------------------------------------------
# mice w/ 2l.pan clustered at country-level 
cluster_var <- "ccodecow"
target_vars <- c("wdi_fossil", "edgar_pm25", "EG.ELC.RNEW.ZS", "wdi_co2")

meth <- make.method(impute_data)
meth[target_vars] <- "2l.pan"
meth[cluster_var] <- ""

pred <- make.predictorMatrix(impute_data)

# let all vars predict all targets except for self-prediction
for (v in target_vars) {
  pred[v, ] <- 1
  pred[v, v] <- 0
  pred[v, cluster_var] <- -2  # -2 indicates the cluster/grouping variable
}

pred["fao_luforplant", "fao_luforreg"] <- 0
pred["fao_luforreg", "fao_luforplant"] <- 0

# could increase "m" later -> could increase for final estimates
imp_panel <- mice(impute_data, method = meth, predictorMatrix = pred, m = 40, maxit = 10)
#skim(completed_data_panel)

# diagnostics
stripplot(imp_panel, wdi_fossil,
          main = "Convergence Diagnostics: Fossil Fuel Consumption",
          xlab = "Imputation Number", 
          ylab = "Fossil Fuel Consumption (% of Total)")

stripplot(imp_panel, edgar_pm25,
          main = "Convergence Diagnostics: PM2.5 Emissions",
          xlab = "Imputation Number",
          ylab = "PM2.5 Emissions")

stripplot(imp_panel, EG.ELC.RNEW.ZS,
          main = "Convergence Diagnostics: Renewable Energy Output",
          xlab = "Imputation Number",
          ylab = "Renewable Energy Output (% of Total Electricity Output)")

stripplot(imp_panel, wdi_co2,
          main = "Convergence Diagnostics: Carbon Dioxide Emissions",
          xlab = "Imputation Number",
          ylab = "Carbon Dioxide (Metric Tons per Capita)")

bwplot(imp_panel, wdi_fossil)
bwplot(imp_panel, edgar_pm25)
bwplot(imp_panel, EG.ELC.RNEW.ZS)
bwplot(imp_panel, wdi_co2)


# part IV: principal component analysis for the index
# -------------------------------------------------------------
# step 1: create list of all datasets from mice
imputed_data_list <- complete(imp_panel, action = "all")

# step 2: make list to store composite index from each imputation
env_index_list <- vector("list", length = length(imputed_data_list))

# step 3: go through each imputed dataset
for (i in seq_along(imputed_data_list)) {
  imputed_data <- imputed_data_list[[i]]
  
  # subset to relevant columns
  subset_data <- imputed_data[, c("ccodecow", "year", target_vars)]
  
  # keep only complete cases for pca
  subset_data <- subset_data[complete.cases(subset_data[, target_vars]), ]
  
  # adjust directionality: reverse variables where higher = better so that all higher = worse
  subset_data_adj <- subset_data
  subset_data_adj$EG.ELC.RNEW.ZS <- -subset_data_adj$EG.ELC.RNEW.ZS  # if higher renewable = better
  
  # scale the variables
  scaled_data <- scale(subset_data_adj[, target_vars])
  
  # run pca
  pca <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
  
  # calculate variance explained
  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  pc1_weight <- var_explained[1]
  pc2_weight <- var_explained[2]
  
  # extract pc1 and pc2 scores
  pc1_scores <- pca$x[, 1]
  pc2_scores <- pca$x[, 2]
  
  # calculate weighted composite index
  composite_score <- pc1_scores * pc1_weight + pc2_scores * pc2_weight
  
  # store results
  env_index_list[[i]] <- data.frame(
    ccodecow = subset_data$ccodecow,
    year = subset_data$year,
    composite_index = composite_score
  )
}

# step 4: combine all composite index scores
pc_scores_long <- do.call(rbind, env_index_list)

# step 5: average composite index across imputations
env_index_avg <- pc_scores_long |>
  group_by(ccodecow, year) |>
  summarise(
    env_index_pca_raw = mean(composite_index, na.rm = TRUE),
    .groups = "drop"
  )

# step 6: flip index so that higher = better environmental performance
env_index_avg <- env_index_avg |>
  mutate(env_index_pca_raw = -env_index_pca_raw)

# step 7: rescale to 0–100
range01 <- function(x) (x - min(x)) / (max(x) - min(x))
env_index_avg <- env_index_avg |>
  mutate(env_index_pca_scaled = range01(env_index_pca_raw) * 100)

# step 8: merge into panel
final_panel_data <- panel_data |>
  left_join(env_index_avg, by = c("ccodecow", "year"))

# check completion
skim(final_panel_data) # 96.5% complete

# summary of the pca information
#  | Variable                   | PC1   | PC2       | PC3        |
#  | -------------------------- | ----- | --------- | ---------- |
#  | `wdi_fossil`               | 0.612 | 0.026     | 0.314      |
#  | `edgar_pm25`               | 0.122 | **0.978** | -0.157     |
#  | `EG.ELC.RNEW.ZS` (flipped) | 0.501 | -0.201    | **-0.841** |
#  | `wdi_co2`                  | 0.600 | -0.058    | 0.412      |
# according to the loadings, pc1 is fossil fuel consumption and pc2 is air pollution 
# for the pca index pc1 and p2 combined and weighted based on their variance explained

# checking 
plot_env_index <- function(country_name, index) {
  # calculate global min and max of the index variable across all countries
  index_sym <- rlang::ensym(index)  # handle bare variable name
  
  global_min <- min(final_panel_data[[rlang::as_string(index_sym)]], na.rm = TRUE)
  global_max <- max(final_panel_data[[rlang::as_string(index_sym)]], na.rm = TRUE)
  
  # filter data for the chosen country
  country_data <- final_panel_data |> filter(country == country_name)
  
  ggplot(country_data, aes(x = year, y = !!index_sym)) +
    geom_line(color = "lightblue", size = 1) +
    geom_point(color = "lightblue", size = 2) +
    labs(
      title = paste("Environmental Index Over Time in", country_name),
      x = "Year",
      y = "Environmental Index (Higher = Better)"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(global_min, global_max))
}

#plot_env_index("Brazil", env_index_scaled)
#plot_env_index("China", env_index_scaled)
#plot_env_index("United States", env_index_scaled)
#plot_env_index("Switzerland", env_index_scaled)

plot_target_vars <- function(country_name) {
  country_data <- imputed_data |> filter(ccodecow == country_name)
  
  # pivot data to long format for plotting multiple variables
  long_data <- country_data |>
    dplyr::select(year, all_of(target_vars)) |>
    pivot_longer(cols = all_of(target_vars), names_to = "indicator", values_to = "value")
  
  ggplot(long_data, aes(x = year, y = value, color = indicator)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Environmental Indicators Over Time in", country_name),
      x = "Year",
      y = "Value",
      color = "Indicator"
    ) +
    theme_minimal()
}


# part V: index using the standard inverse-covariance weighted average of indicators 
# -------------------------------------------------------------
# package for this is only available on stata -> this is done using a file called "creating_env_index"
# using this paper as a guide https://journals.sagepub.com/doi/pdf/10.1177/1536867X20976325 

# step 1: get the mice imputations 
imputed_data_long <- complete(imp_panel, action = "long", include = TRUE)

imputed_data_filtered <- imputed_data_long |>
  dplyr::select(ccodecow, year, wdi_fossil, edgar_pm25, EG.ELC.RNEW.ZS, wdi_co2) |>
  rename(renew_output = "EG.ELC.RNEW.ZS") |>
  group_by(ccodecow, year) |>  # group by id & year
  summarise(
    across(c(wdi_fossil, edgar_pm25, renew_output, wdi_co2), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # truncate negatives to 0 for non-negative variables
  mutate(across(c(wdi_fossil, edgar_pm25, renew_output, wdi_co2),
                ~ ifelse(. < 0, 0, .))) |>
  # z-score all variables (scale to mean 0, sd 1)
  mutate(across(c(wdi_fossil, edgar_pm25, renew_output, wdi_co2), ~ scale(.)[, 1]))

summary(imputed_data_filtered$edgar_pm25)

# step 2: make it a file for stata
write_dta(imputed_data_filtered, "/Users/annikadengel/Desktop/imputed_data_filtered.dta")

# step 3: read back in the file w/ the created index
env_index_data <- read_csv("/Users/annikadengel/Desktop/env_index_gls.csv")
colnames(env_index_data)

env_index_data_prepped <- env_index_data |>
  dplyr::select(-wdi_fossil, -edgar_pm25, -renew_output, -wdi_co2)

env_index_data_prepped <- env_index_data_prepped |> mutate(ccodecow = as.numeric(ccodecow))
final_panel_data <- final_panel_data |> mutate(ccodecow = as.numeric(ccodecow))

final_panel_data <- final_panel_data |>
  left_join(env_index_data_prepped, join_by("ccodecow", "year"))

skim(final_panel_data) #-> env_index_gls has complete rate of 99% w/ normal distribution

# here are the weights for this index...
#   wdi_fossil       .34434826
#   edgar_pm25       .23935157
# renew_output       .42924378
#.    wdi_co2       -.01294362
# less correlation w/ the other variables = greater weight 

#plot_env_index("Brazil", env_index_gls)
#plot_env_index("United States", env_index_gls)
#plot_env_index("Germany", env_index_gls)
#plot_env_index("China", env_index_gls)
#plot_env_index("Sweden", env_index_gls)

summary(final_panel_data$env_index_gls)
hist(final_panel_data$env_index_gls)

rankings_gls <- final_panel_data |>
  filter(year == 2010) |>
  arrange(desc(env_index_gls)) |>
  select(country, year, env_index_gls)

rankings_pca <- final_panel_data |>
  filter(year == 2010) |>
  arrange(desc(env_index_pca_raw)) |>
  select(country, year, env_index_gls)

# making another one that's centered / z-score by country
final_panel_data <- final_panel_data |>
  group_by(ccodecow) |>
  mutate(
    env_index_gls_centered = env_index_gls - mean(env_index_gls, na.rm = TRUE),
    env_index_gls_z = (env_index_gls - mean(env_index_gls, na.rm = TRUE)) / sd(env_index_gls, na.rm = TRUE)
  ) |>
  ungroup()

rankings_gls_z <- final_panel_data |>
  filter(year == 2010) |>
  arrange(desc(env_index_gls_z)) |>
  select(country, year, env_index_gls_centered, env_index_gls_z, env_index_gls)
  
plot_env_index("Russia", env_index_gls_z) 

# making faceted graphs for data section
index_countries <- c("Costa Rica", "Sweden", "Brazil", "China", "United States", "India")

# convert 'country' into a factor w/ specified order
plot_data <- final_panel_data |>
  filter(country %in% index_countries) |>
  mutate(country = factor(country, levels = index_countries))

ggplot(plot_data, aes(x = year, y = env_index_gls)) +
  geom_line(color = "#000000", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ country, scales = "fixed") +
  scale_y_continuous(limits = c(-3.5, 3.5)) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    x = "Year",
    y = "Environmental Performance Index"
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14), 
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12)
  )

# index comparisons
env_ranks <- final_panel_data |>
  filter(!country %in% c("Yugoslavia",
                         "Republic of Vietnam",
                         "Yemen People's Republic",
                         "Yemen Arab Republic",
                         "Taiwan",
                         "Czechoslovakia")) |>           
  group_by(country) |>
  summarise(
    env_index_gls = mean(env_index_gls, na.rm = TRUE),
    env_index_pca_raw = mean(env_index_pca_raw, na.rm = TRUE)
  ) |>
  ungroup()

# compute ranks (1 = best)
env_ranks <- env_ranks |>
  mutate(
    rank_gls = rank(-env_index_gls, ties.method = "min"),
    rank_pca = rank(-env_index_pca_raw, ties.method = "min"),
    rank_diff = rank_gls - rank_pca
  )

# extract top 5 and bottom 5 by constructed index
top_5 <- env_ranks |>
  arrange(rank_gls) |>
  slice(1:5)

bottom_5 <- env_ranks |>
  arrange(desc(rank_gls)) |>
  slice(1:5)

# combine into one table
rank_table <- bind_rows(top_5, bottom_5) |>
  select(country, rank_gls, rank_pca, rank_diff)

# print
rank_table

# calculate spearman correlation
correlation <- cor(final_panel_data$env_index_gls,
                   final_panel_data$env_index_pca_raw,
                   method = "spearman",
                   use = "complete.obs")

correlation

saveRDS(final_panel_data, file = "final_panel_data.rds")



# -------------------------------------------------------------
# comparing treatment and control groups
# -------------------------------------------------------------
# begin from here to not re-run mice, index creation, etc.
final_panel_data <- readRDS("/Users/annikadengel/final_panel_data.rds")

final_panel_data <- final_panel_data |>
  group_by(country) |>
  mutate(
    year_of_recognition = if (any(ehr_recognized == 1, na.rm = TRUE)) {
      min(year[ehr_recognized == 1], na.rm = TRUE)
    } else {
      NA_real_
    }
  ) |>
  ungroup() |>
  mutate(
    post_recognition = if_else(
      is.na(year_of_recognition), 0,
      if_else(year >= year_of_recognition, 1, 0)
    )
  )

view(final_panel_data) 

vdem_data <- readRDS("/Users/annikadengel/Downloads/V-Dem-CY-FullOthers-v15_rds/V-Dem-CY-Full+Others-v15.rds")

# variables to assess:
variables <- c(
  "env_index_gls",         # environmental index
  "NY.GNP.PCAP.PP.CD",     # gni per capita, pp (current international $)
  "NY.GDP.MKTP.KD.ZG",     # gdp growth (annual %)
  "SP.POP.TOTL",           # population total
  "SP.POP.GROW",           # population growth (annual %)
  "v2x_libdem",            # liberal democracy index
  "v2x_regime",            # regime type
  "capacity",              # state capacity
  "wdi_co2"                # co2 emissions
)

vdem_data_filt <- vdem_data |>
  select(year, COWcode, v2x_libdem, v2x_regime, v2x_jucon, v2x_rule, v2juaccnt)

data_for_expl <- final_panel_data

data_for_expl <- data_for_expl |>
  left_join(vdem_data_filt, by = c("year", "ccodecow" = "COWcode"))

data_for_expl <- data_for_expl |>
  left_join(original_qog_data, by = c("year", "ccodecow"))

#skim(data_for_expl)

# step 1: pre-treatment indicator
data_for_expl <- data_for_expl |>
  mutate(
    treated_group = case_when(
      !is.na(ehr_year) ~ "Treated",
      is.na(ehr_year) ~ "Control"
    ),
    pre_treatment = ifelse(year < ehr_year | is.na(ehr_year), 1, 0)  # keep pre-period for controls
  )

# step 2: filter for pre-treatment only
pre_treat_data <- data_for_expl |>
  filter(pre_treatment == 1)

# step 3: summary statistics by treatment group
pre_treat_summary <- pre_treat_data |>
  group_by(treated_group) |>
  summarise(
      # env. index
      mean_env_index = mean(env_index_gls, na.rm = TRUE),
      sd_env_index = sd(env_index_gls, na.rm = TRUE),
      
      # gni per capita
      mean_gni_pc = mean(NY.GNP.PCAP.PP.CD, na.rm = TRUE),
      sd_gni_pc = sd(NY.GNP.PCAP.PP.CD, na.rm = TRUE),
      
      # gdp growth
      mean_gdp_growth = mean(NY.GDP.MKTP.KD.ZG, na.rm = TRUE),
      sd_gdp_growth = sd(NY.GDP.MKTP.KD.ZG, na.rm = TRUE),
      
      # population total
      mean_population = mean(SP.POP.TOTL, na.rm = TRUE),
      sd_population = sd(SP.POP.TOTL, na.rm = TRUE),
      
      # population growth
      mean_pop_growth = mean(SP.POP.GROW, na.rm = TRUE),
      sd_pop_growth = sd(SP.POP.GROW, na.rm = TRUE),
      
      # liberal democracy index
      mean_libdem = mean(v2x_libdem, na.rm = TRUE),
      sd_libdem = sd(v2x_libdem, na.rm = TRUE),
      
      # regime type
      mean_regime = mean(v2x_regime, na.rm = TRUE),
      sd_regime = sd(v2x_regime, na.rm = TRUE),
      
      # state capacity
      mean_capacity = mean(capacity, na.rm = TRUE),
      sd_capacity = sd(capacity, na.rm = TRUE),
      
      # co2 emissions
      mean_co2 = mean(wdi_co2, na.rm = TRUE),
      sd_co2 = sd(wdi_co2, na.rm = TRUE)
    )

view(pre_treat_summary)

plot_boxplot_by_treatment <- function(var_name) {
  # can change these below to make them be passed in
  data <- pre_treat_data
  group_var <- "treated_group"
  
  var_sym <- sym(var_name)
  group_sym <- sym(group_var)
  
  ggplot(data, aes(x = "", y = !!var_sym, fill = !!group_sym)) +
    geom_boxplot() +
    facet_wrap(vars(!!group_sym)) +
    labs(
      title = paste("Distribution of", var_name, "by Treatment Group (Before Treatment)"),
      x = NULL,
      y = var_name
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )
}

# variables to assess:
variables <- c(
  "env_index_gls",         # environmental index
  "NY.GNP.PCAP.PP.CD",     # gni per capita, pp (current international $)
  "NY.GDP.MKTP.KD.ZG",     # gdp growth (annual %)
  "SP.POP.TOTL",           # population total
  "SP.POP.GROW",           # population growth (annual %)
  "v2x_libdem",            # liberal democracy index
  "v2x_regime",            # regime type
  "capacity",              # state capacity
  "wdi_co2"                # co2 emissions
)

pre_treat_data <- pre_treat_data |>
  mutate(log_pop = log(SP.POP.TOTL))

plot_boxplot_by_treatment("wdi_co2") # the two groups do differ but there is overlap for every covariate 



# -------------------------------------------------------------
# implementing generalized synthetic control
# -------------------------------------------------------------
# see xu 2017 paper: https://www.cambridge.org/core/journals/political-analysis/article/generalized-synthetic-control-method-causal-inference-with-interactive-fixed-effects-models/B63A8BD7C239DD4141C67DA10CD0E4F3
colnames(data_for_expl)

data_for_expl <- data_for_expl |> 
  mutate(treatment = ifelse(!is.na(ehr_year) & year >= ehr_year, 1, 0))

table(data_for_expl$treatment, data_for_expl$country)


# part I: preliminary attempt -> w/o imputation
# -------------------------------------------------------------
# gsynth() doesn't allow for missing data
data_gsynth <- data_for_expl |>
  filter(!is.na(env_index_gls)) |>
  filter(!is.na(v2x_libdem)) |>
  filter(!is.na(capacity)) |>
  filter(!is.na(NY.GNP.PCAP.PP.CD)) |>
  filter(!is.na(SP.POP.TOTL))

out <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + capacity + log(NY.GNP.PCAP.PP.CD),
  data = data_gsynth,
  index = c("country", "year"),
  force = "two-way",        # interactive fixed effects
  CV = TRUE,                # use cross-validation to choose number of factors
  r = c(0, 5),              # range of number of factors to test
  se = TRUE,                # get standard errors via bootstrap
  inference = "parametric", # or "nonparametric"
  parallel = TRUE,          # use multiple cores
  nboots = 1000,            # number of bootstrap samples
  min.T0 = 7                # min pre-treatment years -> >= 7 recommended
)

print(out) # parallel pre-trends
plot(out)

# this is w/ the 3688 remaining observations -> original data is 7820
#Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.1902 0.1511   -0.106   0.4864  0.2082


# part II: imputing the data for the covariates
# -------------------------------------------------------------
# this rewrites the previous target variables
target_vars <- c("env_index_gls","v2x_libdem", "capacity", "NY.GNP.PCAP.CD", "SP.POP.TOTL", "v2x_rule")

vdem_filtered <- vdem_data |>
  select(country_name, COWcode, year, starts_with("e"), starts_with("v2x_")) |>
  rename("ccodecow" = "COWcode")

auxilary_data <- final_panel_data |>
  left_join(vdem_filtered, by = c("ccodecow", "year")) |>
  select(-ehr_recognized, -env_index_pca_raw, -env_index_pca_scaled, -env_index_gls_z, -env_index_gls_centered, -year_of_recognition, -post_recognition) |>
  select(-ends_with("sd"), -ends_with("high"), -ends_with("low"), -ends_with("C"))

# clean up the auxiliary dataset -> higher thresholds b/c there's more options now
na_threshold <- 0.40
corr_threshold <- 0.50
collinear_threshold <- 0.85

always_keep <- c("ccodecow", "year", target_vars)

aux_filtered <- auxilary_data |>
  select(where(~ mean(is.na(.)) <= na_threshold | any(colnames(.) %in% always_keep)))

# keep only numeric columns excluding targets
numeric_vars <- aux_filtered |>
  select(where(is.numeric)) |>
  select(-any_of(always_keep))

# remove zero-variance columns except key variables
numeric_vars <- numeric_vars |>
  select(where(~ sd(., na.rm = TRUE) > 0))

# keep variables correlated w/ any of the targets but not collinear
keep_var <- function(var_name) {
  cors <- sapply(target_vars, function(tgt) {
    if (all(is.na(aux_filtered[[var_name]])) | all(is.na(aux_filtered[[tgt]]))) return(0)
    cor(aux_filtered[[var_name]], aux_filtered[[tgt]], use = "complete.obs")
  }) 
  
  any((abs(cors) >= corr_threshold) & (abs(cors) <= collinear_threshold))
}

correlated_vars <- names(numeric_vars)[sapply(names(numeric_vars), keep_var)]

# keep only selected and always_keep
aux_filtered <- aux_filtered |>
  select(all_of(c(always_keep, correlated_vars)))

colnames(aux_filtered) # left with variables! not bad

# checking the skews...
skew_threshold <- 1

# skip always-keep variables
vars_to_skip <- c("ccodecow", "year")
numeric_vars <- names(aux_filtered)[sapply(aux_filtered, is.numeric)]
numeric_vars <- setdiff(numeric_vars, vars_to_skip)

# apply skewness transformation
for (var in numeric_vars) {
  x <- aux_filtered[[var]]
  skew_val <- skewness(x, na.rm = TRUE)
  
  if (skew_val > skew_threshold) {
    if (all(x >= 0, na.rm = TRUE)) {
      message(paste("right-skewed:", var, "- applying log1p()"))
      aux_filtered[[var]] <- log1p(x)
    } else {
      warning(paste("skipped", var, "- contains negative values"))
    }
    
  } else if (skew_val < -skew_threshold) {
    max_val <- max(x, na.rm = TRUE)
    if (all(max_val + 1 - x > 0, na.rm = TRUE)) {
      message(paste("left-skewed:", var, "- reflecting and applying log1p()"))
      aux_filtered[[var]] <- log1p(max_val + 1 - x)
    } else {
      warning(paste("skipped", var, "- reflection would produce invalid values"))
    }
    
  } else {
    message(paste("not highly skewed:", var, "- no transformation"))
  }
}

skim(aux_filtered)

aux_filtered <- aux_filtered |>
  select(-wbregion)

# step 1: correlation matrix
cor_mat <- cor(aux_filtered, use = "pairwise.complete.obs")

# step 2: get variable pairs with high correlation
high_corr_pairs <- which(abs(cor_mat) > 0.99 & abs(cor_mat) < 1, arr.ind = TRUE)

# step 3: turn into variable name pairs
var_names <- colnames(cor_mat)
high_corr_vars <- data.frame(
  var1 = var_names[high_corr_pairs[, 1]],
  var2 = var_names[high_corr_pairs[, 2]],
  stringsAsFactors = FALSE
)

# step 4: exclude dropping target variables
vars_to_drop <- apply(high_corr_vars, 1, function(row) {
  if (row["var1"] %in% target_vars || row["var1"] %in% c("ccodecow", "year")) {
    if (row["var2"] %in% target_vars || row["var2"] %in% c("ccodecow", "year")) {
      return(NA)
    } else {
      return(row["var2"])
    }
  } else if (row["var2"] %in% target_vars || row["var2"] %in% c("ccodecow", "year")) {
    return(row["var1"])
  } else {
    return(row["var1"])
  }
})

# remove duplicates and NAs
vars_to_drop <- unique(na.omit(vars_to_drop))

# step 5: drop from data
cleaned_data <- aux_filtered[, !(names(aux_filtered) %in% vars_to_drop)]
colnames(cleaned_data)

# one last variable removal
aux_final_filtered <- cleaned_data |>
  select(-e_democracy_omitteddata)

# mice w/ 2l.pan clustered at country-level
cluster_var <- "ccodecow"

meth <- make.method(aux_final_filtered)
meth[target_vars] <- "2l.pan"
meth[cluster_var] <- ""

pred <- make.predictorMatrix(aux_final_filtered)

# let all vars predict all targets except for self-prediction
for (v in target_vars) {
  pred[v, ] <- 1
  pred[v, v] <- 0
  pred[v, cluster_var] <- -2  # -2 indicates the cluster/grouping variable
}

# final imputation 
gs_imp_panel <- mice(aux_final_filtered, method = meth, predictorMatrix = pred, m = 40, maxit = 10)

# [1] "env_index_gls" "v2x_libdem" "capacity" "NY.GNP.PCAP.CD" "SP.POP.TOTL" 
stripplot(gs_imp_panel, env_index_gls,
          main = "Convergence Diagnostics: Environmental Index",
          xlab = "Imputation Number",
          ylab = "Standardized Environmental Index Value")

stripplot(gs_imp_panel, v2x_libdem,
          main = "Convergence Diagnostics: Liberal Democracy (V-Dem)",
          xlab = "Imputation Number",
          ylab = "V-Dem Liberal Democracy Score")

stripplot(gs_imp_panel, capacity,
          main = "Convergence Diagnostics: State Capacity",
          xlab = "Imputation Number",
          ylab = "State Capacity Index")

stripplot(gs_imp_panel, NY.GNP.PCAP.CD,
          main = "Convergence Diagnostics: Log GNP per Capita",
          xlab = "Imputation Number",
          ylab = "Log(GNP per Capita)")

stripplot(gs_imp_panel, SP.POP.TOTL,
          main = "Convergence Diagnostics: Log Population",
          xlab = "Imputation Number",
          ylab = "Log(Total Population)")

stripplot(gs_imp_panel, v2x_rule,
          main = "Convergence Diagnostics: Rule of Law (V-Dem)",
          xlab = "Imputation Number",
          ylab = "V-Dem Rule of Law Score")

# number of imputations
m <- gs_imp_panel$m

# average all imputations
imputed_avg <- Reduce("+", lapply(1:m, function(i) {
  complete(gs_imp_panel, action = i)
})) / m

# add back id variables
id_vars <- complete(gs_imp_panel, action = 1) |>
  select(ccodecow, year)

# combine with averaged data
avg_imputed <- bind_cols(id_vars, imputed_avg |> select(-ccodecow, -year))

skim(imputed_avg)

# now let's use this for gsynth
add_in_data <- final_panel_data |>
  select(ccodecow, year, ehr_year, ehr_recognized)

# create imp_data_gs by joining imputed average with additional vars
imp_data_gs <- imputed_avg |>
  left_join(add_in_data, by = c("ccodecow", "year")) |>
  mutate(treatment = ifelse(!is.na(ehr_year) & year >= ehr_year, 1, 0)) |>
  rename("log_gnp" = "NY.GNP.PCAP.CD", "log_pop" = "SP.POP.TOTL", "ag_yield" = "AG.YLD.CREL.KG")

write_dta(imp_data_gs, "/Users/annikadengel/Desktop/imp_data_gs.dta")


# part IV: baseline analysis
# -------------------------------------------------------------
# start here to not re-run mice
imp_data_gs <- read_dta("/Users/annikadengel/Desktop/imp_data_gs.dta")

# now here's the model w/ the imputed data
out_imp <- gsynth(
  formula = env_index_gls ~ treatment + capacity + v2x_libdem + log_pop + log_gnp,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",    # interactive fixed effects
  CV = TRUE,            # use cross-validation to choose number of factors
  r = c(0, 5),          # range of number of factors to test
  se = TRUE,            # get standard errors via bootstrap
  inference = "parametric", # or "nonparametric"
  parallel = FALSE,      # use multiple cores
  nboots = 1000,        # number of bootstrap samples
  min.T0 = 7            # min pre-treatment years -> >= 7 recommended
)

skim(imp_data_gs)
setdiff(unique(imp_data_gs$ccodecow), out_imp$valid.id)

imp_data_gs |>
  group_by(ccodecow) |>
  summarise(pre_treat_years = sum(treatment == 0, na.rm = TRUE)) |>
  filter(pre_treat_years < 7)

print(out_imp) 
# Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.1533 0.1316  -0.1046   0.4111  0.2441

plot(out_imp) # parallel pre-trends
plot(out_imp, type = "counterfactual")

# this is the pre-imputation version, so results are consistent
#Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.1902 0.1511   -0.106   0.4864  0.2082

# here are some models
imp_data_gs <- imp_data_gs |>
  group_by(ccodecow) |>
  mutate(
    year_of_recognition = if (any(ehr_recognized == 1, na.rm = TRUE)) {
      min(year[ehr_recognized == 1], na.rm = TRUE)
    } else {
      NA_real_
    }
  ) |>
  ungroup() |>
  mutate(
    post_recognition = if_else(
      is.na(year_of_recognition), 0,
      if_else(year >= year_of_recognition, 1, 0)
    )
  )

capacity_model <- feols(
  env_index_gls ~ post_recognition * capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = imp_data_gs,
  cluster = ~ ccodecow + year
)

summary(capacity_model)

ehr_only_model <- feols(
  env_index_gls ~ post_recognition + capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = imp_data_gs,
  cluster = ~ ccodecow + year
)

summary(ehr_only_model) 


# part V: capacity strata analysis
# -------------------------------------------------------------
# the estimator on different levels of capacity
country_capacity <- imp_data_gs |>
  group_by(ccodecow) |>
  summarise(avg_capacity = mean(capacity, na.rm = TRUE))

# create tertiles on the average capacity
country_capacity <- country_capacity |>
  mutate(capacity_group = cut(
    avg_capacity,
    breaks = quantile(avg_capacity, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("low", "medium", "high")
  ))

imp_data_gs <- imp_data_gs |>
  left_join(country_capacity |> select(ccodecow, capacity_group), by = "ccodecow")

table(imp_data_gs$capacity_group)
# low medium   high 
#2622   2576   2622 

imp_data_gs |>
  group_by(capacity_group, ehr_recognized) |>
  summarize(n_countries = n_distinct(ccodecow)) |>
  view()

# low
data_lc <- imp_data_gs |>
  filter(capacity_group == "low") |>
  as.data.frame()

gsynth_results_lc <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

print(gsynth_results_lc)
# Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.1399 0.1514  -0.1569   0.4367  0.3557

plot(gsynth_results_lc) 

plot(gsynth_results_lc,
     type = "counterfactual",
     main = "Low-Capacity Countries Counterfactual Trajectories",
     xlab = "Year",
     ylab = "Environmental Index"
)

# medium
data_mc <- imp_data_gs |>
  filter(capacity_group == "medium") |>
  as.data.frame()

gsynth_results_mc <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 5000,
  min.T0 = 7
)

print(gsynth_results_mc) 
#Average Treatment Effect on the Treated:
#  Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg    0.288 0.1366   0.0202   0.5557 0.03504

plot(gsynth_results_mc,
     main = "Estimated Treatment Effect Over Time (Medium-Capacity Countries)",
     xlab = "Year",
     ylab = "Environmental Index",
)

plot(gsynth_results_mc,
     type = "counterfactual",
     main = "Medium-Capacity Countries Counterfactual Trajectory",
     xlab = "Year",
     ylab = "Environmental Index"
)

# examining the countries
imp_data_gs$country_name <- countrycode(imp_data_gs$ccodecow, origin = "cown", destination = "country.name")

unique(imp_data_gs$country_name[imp_data_gs$capacity_group == "medium"])

# high
data_hc <- imp_data_gs |>
  filter(capacity_group == "high") |>
  as.data.frame()

gsynth_results_hc <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

unique(data_hc$year)

print(gsynth_results_hc)
# Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg  0.02645 0.1506  -0.2687   0.3216  0.8606

low_capacity_model <- feols(
  env_index_gls ~ post_recognition * capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_lc,
  cluster = ~ ccodecow + year
)

summary(low_capacity_model)

low_ehr_only_model <- feols(
  env_index_gls ~ post_recognition + capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_lc,
  cluster = ~ ccodecow + year
)

summary(low_ehr_only_model) 

med_capacity_model <- feols(
  env_index_gls ~ post_recognition * capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_mc,
  cluster = ~ ccodecow + year
)

summary(med_capacity_model)

med_ehr_only_model <- feols(
  env_index_gls ~ post_recognition + capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_mc,
  cluster = ~ ccodecow + year
)

summary(med_ehr_only_model) 

high_capacity_model <- feols(
  env_index_gls ~ post_recognition * capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_hc,
  cluster = ~ ccodecow + year
)

summary(high_capacity_model)

high_ehr_only_model <- feols(
  env_index_gls ~ post_recognition + capacity + v2x_libdem + log_pop + log_gnp | 
    ccodecow + year,
  data = data_hc,
  cluster = ~ ccodecow + year
)

summary(high_ehr_only_model) 


# part VI: examining estimation variations
# -------------------------------------------------------------
# maybe there's more medium-capacity countries recognizing
table(imp_data_gs$treatment, imp_data_gs$capacity_group) 
#low medium high
#0 2134   1891 1928
#1  488    685  694

# strata w/o imputation...
# gsynth() doesn't allow for missing data
data_gsynth <- data_for_expl |>
  filter(!is.na(env_index_gls)) |>
  filter(!is.na(v2x_libdem)) |>
  filter(!is.na(capacity)) |>
  filter(!is.na(NY.GNP.PCAP.PP.CD)) |>
  filter(!is.na(NY.GNP.PCAP.CD)) |>
  filter(!is.na(SP.POP.TOTL))

country_capacity <- data_gsynth |>
  group_by(ccodecow) |>
  summarise(avg_capacity = mean(capacity, na.rm = TRUE))

country_capacity <- country_capacity |>
  mutate(capacity_group = cut(
    avg_capacity,
    breaks = quantile(avg_capacity, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("low", "medium", "high")
  ))

data_gsynth <- data_gsynth |>
  left_join(country_capacity |> select(ccodecow, capacity_group), by = "ccodecow")

table(data_gsynth$capacity_group)
#low medium   high 
#1230   1351   1357

# low (non-impouted)
data_lc <- data_gsynth |>
  filter(capacity_group == "low") |>
  as.data.frame()

gsynth_results_lc_ni <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

print(gsynth_results_lc_ni)
# Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg  -0.1126 0.1164  -0.3406   0.1155  0.3334

plot(gsynth_results_lc_ni) 
plot(gsynth_results_lc_ni, type = "counterfactual") 

# medium (non-impouted)
data_mc <- data_gsynth |>
  filter(capacity_group == "medium") |>
  as.data.frame()

gsynth_results_mc_ni <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

print(gsynth_results_mc_ni)
# Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg  -0.1356 0.2655   -0.656   0.3849  0.6097

plot(gsynth_results_mc_ni)
plot(gsynth_results_mc_ni, type = "counterfactual") 

plot(gsynth_results_mc_ni, type = "gap")

# high (non-impouted)
data_hc <- data_gsynth |>
  filter(capacity_group == "high") |>
  as.data.frame()

gsynth_results_hc_ni <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.PP.CD + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

print(gsynth_results_hc_ni)
#Average Treatment Effect on the Treated:
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg  -0.2615 0.2305  -0.7132   0.1902  0.2565

plot(gsynth_results_hc_ni)
plot(gsynth_results_hc_ni, type = "counterfactual")

# merge the imputed data w/ auxiliary_data to see if it improves the estimates
colnames(imp_data_gs)
colnames(auxilary_data)

imp_data_add <- imp_data_gs |>
  select(ccodecow, treatment, year, capacity, capacity_group, env_index_gls, v2x_libdem, log_pop, log_gnp)

auxiliary_add <- auxilary_data |>
  select(-env_index_gls, -v2x_libdem, -capacity, -NY.GNP.PCAP.CD, -SP.POP.TOTL)

bigger_gsynth_data <- imp_data_add |>
  left_join(auxiliary_add, by = c("ccodecow", "year"))

colnames(bigger_gsynth_data)

# medium capacity again
data_mc_big <- bigger_gsynth_data |>
  filter(capacity_group == "medium") |>
  as.data.frame()

gsynth_results_mc_big <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = data_mc_big,
  index = c("ccodecow", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "nonparametric",
  parallel = FALSE,
  nboots = 1000,
  min.T0 = 7
)

print(gsynth_results_mc_big)
# Average Treatment Effect on the Treated: -> w/ parametric
#Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.2795 0.1253    0.034    0.525 0.02565

# Average Treatment Effect on the Treated: -> w/ non-parametric
#  Estimate   S.E. CI.lower CI.upper p.value
#ATT.avg   0.2795 0.1268  0.03097   0.5281 0.02751

plot(gsynth_results_mc_big)
plot(gsynth_results_mc_big, type = "counterfactual")

# countries within each tertile
unique(imp_data_gs$country_name[imp_data_gs$capacity_group == "low"])
unique(imp_data_gs$country_name[imp_data_gs$capacity_group == "medium"])
unique(imp_data_gs$country_name[imp_data_gs$capacity_group == "high"])

# impact of the size of medium-capacity group
country_capacity <- imp_data_gs |>
  group_by(ccodecow) |>
  summarise(avg_capacity = mean(capacity, na.rm = TRUE))

cap_30 <- quantile(country_capacity$avg_capacity, probs = 0.30, na.rm = TRUE)
cap_70 <- quantile(country_capacity$avg_capacity, probs = 0.70, na.rm = TRUE)

imp_data_gs <- imp_data_gs |>
  left_join(country_capacity, by = "ccodecow") |>
  mutate(
    capacity_group_wide = case_when(
      avg_capacity < cap_30 ~ "low",
      avg_capacity >= cap_30 & avg_capacity <= cap_70 ~ "medium",
      avg_capacity > cap_70 ~ "high"
    )
  )

medium_wide_data <- imp_data_gs |> filter(capacity_group_wide == "medium")

gsynth_results_wide <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = medium_wide_data,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsynth_results_wide)

# even wider
cap_20 <- quantile(country_capacity$avg_capacity, probs = 0.20, na.rm = TRUE)
cap_80 <- quantile(country_capacity$avg_capacity, probs = 0.80, na.rm = TRUE)

imp_data_gs <- imp_data_gs |>
  mutate(
    capacity_group_wide = case_when(
      avg_capacity < cap_20 ~ "low",
      avg_capacity >= cap_20 & avg_capacity <= cap_80 ~ "medium",
      avg_capacity > cap_80 ~ "high"
    )
  )

medium_wide_data <- imp_data_gs |> filter(capacity_group_wide == "medium")

gsynth_results_wider <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = medium_wide_data,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsynth_results_wider)

# wider complete-case
data_gsynth

country_capacity_cc <- data_gsynth |>
  group_by(ccodecow) |>
  summarise(avg_capacity = mean(capacity, na.rm = TRUE))

cap_20_cc <- quantile(country_capacity_cc$avg_capacity, probs = 0.20, na.rm = TRUE)
cap_80_cc <- quantile(country_capacity_cc$avg_capacity, probs = 0.80, na.rm = TRUE)


country_capacity_cc <- country_capacity_cc |>
  mutate(
    capacity_group_wide = case_when(
      avg_capacity < cap_20_cc ~ "low",
      avg_capacity >= cap_20_cc & avg_capacity <= cap_80_cc ~ "medium",
      avg_capacity > cap_80_cc ~ "high"
    )
  )

med_cc <- country_capacity_cc$ccodecow[country_capacity_cc$capacity_group_wide == "medium"]

data_gsynth_mcc <- data_gsynth |> filter(ccodecow %in% med_cc)

unique(data_gsynth_mcc$ccodecow)

gsynth_results_wider_cc <- gsynth(
  formula = env_index_gls ~ treatment + v2x_libdem + capacity + log(NY.GNP.PCAP.PP.CD),
  data = data_gsynth_mcc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 5
)

print(gsynth_results_wider_cc) # roughly 30 units were dropped w/ r = 0:5 and min.T0 = 7


# part VII: unit-specific treatment effects
# -------------------------------------------------------------
unit_effects <- out_imp$est.ind
view(unit_effects) 

str(unit_effects) # three-dimensional array
eff_matrix <- unit_effects[ , "Eff", ] # extracting "eff" layer
dim(eff_matrix)

time_labels <- as.numeric(rownames(eff_matrix)) 
post_rows <- which(time_labels > 0) # identify post-treatment periods
eff_post <- eff_matrix[post_rows, ]

unit_att <- colMeans(eff_post, na.rm = TRUE) # computer per-unit att

unit_att_df <- data.frame(
  country = names(unit_att),
  avg_att = unit_att
)

# add the country names 
unit_att_df$country <- as.numeric(unit_att_df$country)
unit_att_df <- unit_att_df |>
  mutate(country_name = countrycode(country, origin = "cown", destination = "country.name")) |>
  rename(ccodecow = "country") |>
  select(country_name, everything())

view(unit_att_df)

# sort by ascending
unit_att_df <- arrange(unit_att_df, avg_att)
view(unit_att_df)

# which countries experienced any significant effects
pval_matrix <- unit_effects[ , "p.value", ]
time_labels <- as.numeric(rownames(pval_matrix))
post_rows <- which(time_labels >= 0)
pval_post <- pval_matrix[post_rows, ]

# using p-value threshold < 0.05
sig_any <- apply(pval_post, 2, function(x) any(x < 0.05, na.rm = TRUE))
sig_countries <- data.frame(
  country = names(sig_any)[sig_any]
)

sig_countries$country <- as.numeric(sig_countries$country)
sig_countries <- sig_countries |>
  mutate(country_name = countrycode(country, origin = "cown", destination = "country.name")) |>
  rename("ccodecow" = "country") |>
  select(country_name, everything())

view(sig_countries)

# adding capacity groups -> have to re-write some of the previous code
country_capacity <- country_capacity |>
  mutate(capacity_group = cut(
    avg_capacity,
    breaks = quantile(avg_capacity, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("low", "medium", "high")
  ))

sig_countries <- sig_countries |>
  left_join(country_capacity, by = "ccodecow")

# sort by capacity group
sig_countries_sorted <- sig_countries[order(sig_countries$capacity_group), ]

# view the sorted table
view(sig_countries_sorted)

view(sig_countries)
table(sig_countries$capacity_group) # most are in medium capacity

# low medium   high -> < 0.05 p-value threshold
#8     14      6 

#low medium   high -> < 0.01 p-value threshold 
#7     12      3


# part IIX: post-estimation analysis
# -------------------------------------------------------------
# assessing potential capacity ceiling -> seeing if before recognition high-capacity countries have better pre-treatment environmental performance than medium-capacity
colnames(imp_data_gs)

imp_data_gs$pre_recognition <- ifelse(imp_data_gs$year < imp_data_gs$ehr_year, 1, 0)
table(imp_data_gs$pre_recognition)

pre_recog_data <- imp_data_gs[imp_data_gs$pre_recognition == 1, ]
summary(pre_recog_data)

# making sure it's a factor variable
pre_recog_data$capacity_group <- factor(pre_recog_data$capacity_group,
                                  levels = c("low", "medium", "high"))

model <- lm(env_index_gls ~ capacity_group + log_gnp + log_pop + v2x_libdem + factor(year), data = pre_recog_data) # pooled 
summary(model)

# capacity_groupmedium -0.24962    0.03169  -7.876 5.06e-15 ***
# capacity_grouphigh   -0.87232    0.04248 -20.536  < 2e-16 ***
# high-capacity countries perform worse pre-recognition than medium-capacity

avg_trends <- imp_data_gs |>
  filter(!(is.na(ehr_year))) |>
  group_by(year, capacity_group) |>
  summarize(
    mean_env = mean(env_index_gls, na.rm = TRUE),
    sd_env = sd(env_index_gls, na.rm = TRUE),
    n = n(),
    se_env = sd_env / sqrt(n)
  )

# plot
ggplot(avg_trends, aes(x = year, y = mean_env, color = capacity_group, fill = capacity_group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_env - se_env, ymax = mean_env + se_env), alpha = 0.2) +
  labs(
    title = "Mean Environmental Performance over Time by Capacity Group",
    x = "Year",
    y = "Mean Environmental Performance"
  ) +
  theme_minimal()

# formally testing
imp_data_gs$year_c <- imp_data_gs$year - min(imp_data_gs$year) # centering year
imp_data_treat <- imp_data_gs |>
  filter(!is.na(ehr_year))

model_ceiling1 <- lm(env_index_gls ~ year_c * capacity_group + log_gnp + v2x_libdem + log_pop, data = imp_data_treat)
model_ceiling2 <- feols(env_index_gls ~ capacity + log_gnp + v2x_libdem + log_pop | factor(year), data = imp_data_treat) # continuous not categorical

summary(model_ceiling1)
summary(model_ceiling2)

model_ceiling3 <- feols(env_index_gls ~ capacity + log_gnp + v2x_libdem + log_pop | factor(year), data = imp_data_gs)
summary(model_ceiling3)

imp_data_gs$pre_recognition <- ifelse(imp_data_gs$year < imp_data_gs$ehr_year, 1, 0)
pre_data <- imp_data_gs[imp_data_gs$pre_recognition == 1, ]

unique(sig_countries$country_name[sig_countries$capacity_group == "high"])
unique(sig_countries$country_name[sig_countries$capacity_group == "medium"])


# -------------------------------------------------------------
# gsynth robustness checks
# -------------------------------------------------------------
# medium capacity
# normal: 
gsc_mc <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_mc)

# r = 0:3
gsc_mc_r <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_mc_r)

# inference = "nonparametric"
gsc_mc_inf <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "nonparametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_mc_inf)

# min.T0 = 5
gsc_mc_t <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3), # as some treated units have too few pre-treatment periods -> previously worked with (0, 4)
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 5
)

print(gsc_mc_t)

# no covariates 
gsc_mc_nc <- gsynth(
  env_index_gls ~ treatment,
  data = data_mc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_mc_nc)

# high capacity
# normal: 
gsc_hc <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_hc)

# r = 0:3
gsc_hc_r <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_hc_r)

# inference = "nonparametric"
gsc_hc_inf <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "nonparametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_hc_inf)

# min.T0 = 5
gsc_hc_t <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 4),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 5
)

print(gsc_hc_t)

# no covariates 
gsc_hc_nc <- gsynth(
  env_index_gls ~ treatment,
  data = data_hc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_hc_nc)

# low capacity
# normal: 
gsc_lc <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_lc)

# r = 0:3
gsc_lc_r <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_lc_r)

# inference = "nonparametric"
gsc_lc_inf <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "nonparametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_lc_inf)

# min.T0 = 5
gsc_lc_t <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 5
)

print(gsc_lc_t)

# no covariates 
gsc_lc_nc <- gsynth(
  env_index_gls ~ treatment,
  data = data_lc,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_lc_nc)

# full sample
# normal: 
gsc_fs <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_fs)

# r = 0:3
gsc_fs_r <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 3),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_fs_r)

# inference = "nonparametric"
gsc_fs_inf <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "nonparametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_fs_inf)

# min.T0 = 5
gsc_fs_t <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + SP.POP.TOTL + NY.GNP.PCAP.CD + capacity,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 4),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 5
)

print(gsc_fs_t)

# no covariates 
gsc_fs_nc <- gsynth(
  env_index_gls ~ treatment,
  data = imp_data_gs,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_fs_nc)



# -------------------------------------------------------------
# examining estimation w/o EU states 
# -------------------------------------------------------------
unique(medium_wide_data$ccodecow)

eu_ccodecow <- c(
  # eu member states
  305, # Austria
  211, # Belgium
  355, # Bulgaria
  344, # Croatia
  352, # Cyprus
  316, # Czech Republic
  390, # Denmark
  366, # Estonia
  375, # Finland
  220, # France
  255, # Germany
  350, # Greece
  310, # Hungary
  205, # Ireland
  325, # Italy
  367, # Latvia
  368, # Lithuania
  212, # Luxembourg
  338, # Malta
  210, # Netherlands
  290, # Poland
  235, # Portugal
  360, # Romania
  317, # Slovakia
  349, # Slovenia
  230, # Spain
  380, # Sweden
  
  # eu accession / candidate countries
  345, # Albania
  337, # Bosnia and Herzegovina
  412, # Montenegro
  328, # North Macedonia
  453, # Serbia
  495, # Kosovo (note: partial recognition, include if relevant)
  477, # Moldova
  804, # Ukraine
  268, # Georgia
  
  # potential candidate (frozen negotiations)
  792  # Turkey
)

medium_wide_data_neu <- medium_wide_data |>
  filter(!(ccodecow %in% eu_ccodecow))

gsynth_results_wider_neu <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = medium_wide_data_neu,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsynth_results_wider_neu) # still significant -> p-value is 0.001

data_mc_neu <- data_mc |>
  filter(!(ccodecow %in% eu_ccodecow))

gsc_mc_neu <- gsynth(
  env_index_gls ~ treatment + v2x_libdem + log_pop + log_gnp + capacity,
  data = data_mc_neu,
  index = c("ccodecow", "year"),
  force = "two-way",
  r = c(0, 5),
  CV = TRUE,
  se = TRUE,
  nboots = 1000,
  inference = "parametric",
  parallel = FALSE,
  min.T0 = 7
)

print(gsc_mc_neu)



# -------------------------------------------------------------
# creating a map of capacity groups
# -------------------------------------------------------------
# load world map data with country polygons from rworldmap and convert to sf
world_sp <- getMap(resolution = "coarse")  # resolution can be "coarse", "low", or "high"
world <- st_as_sf(world_sp)

# prepare capacity data with distinct countries and capacity groups
capacity_data <- imp_data_gs |>
  distinct(country_name, capacity_group) |>
  mutate(iso_a3 = countrycode(country_name, "country.name", "iso3c"))

# join spatial data with capacity groups by ISO3 codes
world_capacity <- world |>
  left_join(capacity_data, by = c("ISO3" = "iso_a3"))  # note rworldmap uses "ISO3"

# check column names and distribution of capacity groups
colnames(world_capacity)
table(world_capacity$capacity_group, useNA = "ifany")

# convert capacity_group to factor with correct levels (match keys in scale_fill_manual)
world_capacity$capacity_group <- factor(
  tolower(world_capacity$capacity_group), 
  levels = c("low", "medium", "high")
)

ggplot(data = world_capacity) +
  geom_sf(aes(fill = capacity_group), color = "gray70", size = 0.1) +
  scale_fill_manual(
    values = c("low" = "#000080", "medium" = "#4682B4", "high" = "lightskyblue2"),
    labels = c("Low", "Medium", "High")
  ) +
  theme_minimal() +
  labs(fill = "Capacity Group", title = "Global Distribution of State Capacity Groups") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # center title
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1, "cm"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5)
  )



# -------------------------------------------------------------
# looking at the political constraints
# -------------------------------------------------------------
pc_data <- read_excel("/Users/annikadengel/Desktop/POLCON_2021_VDEM.xlsx")

imp_data_pc <- imp_data_gs |>
  left_join(pc_data, by = c("ccodecow" = "ccode", "year"))

high_list <- unique(sig_countries$ccodecow[sig_countries$capacity_group == "high"])
med_list <- unique(sig_countries$ccodecow[sig_countries$capacity_group == "medium"])

t_test_data <- imp_data_pc |>
  filter(ccodecow %in% c(high_list, med_list)) |>
  mutate(capacity_group = case_when(
    ccodecow %in% high_list ~ "High",
    ccodecow %in% med_list ~ "Medium",
    TRUE ~ NA_character_
  ))

t_test_data <- t_test_data |> filter(!is.na(POLCONIII_VDEM))

t_test_result <- t.test(POLCONIII_VDEM ~ capacity_group, data = t_test_data, var.equal = FALSE)
print(t_test_result)

#Welch Two Sample t-test

#data:  POLCONIII_VDEM by capacity_group
#t = 23.115, df = 476.07, p-value < 2.2e-16
#alternative hypothesis: true difference in means between group High and group Medium is not equal to 0
#95 percent confidence interval:
#  0.2923152 0.3466307
#sample estimates:
#  mean in group High mean in group Medium 
#0.4255425            0.1060696 

# now for not just the treated 
t_test_data2 <- imp_data_pc |>
  filter(capacity_group != "Low") |>
  mutate(capacity_group = case_when(
    ccodecow %in% high_list ~ "High",
    ccodecow %in% med_list ~ "Medium",
    TRUE ~ NA_character_
  ))

t_test_data2 <- t_test_data2 |> filter(!is.na(POLCONIII_VDEM))

t_test_result2 <- t.test(POLCONIII_VDEM ~ capacity_group, data = t_test_data, var.equal = FALSE)
print(t_test_result2)

#Welch Two Sample t-test

#data:  POLCONIII_VDEM by capacity_group
#t = 23.115, df = 476.07, p-value < 2.2e-16
#alternative hypothesis: true difference in means between group High and group Medium is not equal to 0
#95 percent confidence interval:
#  0.2923152 0.3466307
#sample estimates:
#  mean in group High mean in group Medium 
#0.4255425            0.1060696 