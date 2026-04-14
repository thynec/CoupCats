rm(list = ls())

#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests_DV)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  dplyr::select(-country, -coup_attempt, -coup_successful, -pce, -pce2, -pce3, -pce_succ, -pce2_succ, -pce3_succ, -cw, -cw_onset, -pce_cw, -pce2_cw, -pce3_cw, -protests)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

# ------------------------------------------------------------#
# 1. Build time index
# ------------------------------------------------------------#
df <- base_data %>%
  arrange(ccode, year, month) %>%
  mutate(time_id = year * 12 + month)

#not trusting data before 1960 due to extrapolation; let's end it
df <- df %>%
  filter(year>=1960)

# ---------------------------------------------------------------------#
# 2. Drop unused OG variables (only if they exist)
# ---------------------------------------------------------------------#
drop_vars <- c(
  "wom_polpart_OG", "wom_civlib_OG", "gini_OG", "gini_wdi_OG", "gini_wiid_OG",
  "gini_swiid_OG", "vdem_gdppc_OG", "hc_OG", "oda_OG", "nr_rents_OG", "debt_OG",
  "wdi_gdppc_OG", "stability_OG", "mobilization_OG", "mobil_conc_OG", "acled_OG",
  "acledl", "acledlz", "milex_spliced_OG", "milper_spliced_OG", "trade_glob_OG",
  "pol_glob_OG", "gender_parity_OG", "women_polemp_OG", "interpersonal_glob_OG",
  "fdi_OG", "arrivals_OG", "ltrade_OG", "ldtrade_OG", "full", "associate",
  "observer", "full_OG", "associate_OG", "observer_OG", "IOs_sum_OG"
)

# ------------------------------------------------------------#
# 3. Define IV blocks
# ------------------------------------------------------------#
leader_IVs <- c("numleaders_10yr")
regime_IVs <- c("polyarchy", "polyarchy2")
milit_leader_IVs <- c("milreg") #"milit", "milreg_prop",,  "prop_milit_career"
milit_IVs <- c("solqual", "mutiny6") #"milex_spliced", "milper_spliced", , "mutiny12"
gender_IVs <- c("wom_polpart") #, "women_polemp", "wom_civlib", "gender_parity"
econ_IVs <- c("gini", "gdppc", "ch_gdppc", "oda", "nr_rents", "debt") #, "hc"
stability_IVs <- c("mobilization") #"stability_WB", , "mobil_conc", , "cw_lag", "brd", "protests"
glob_IVs <- c("signal", "mid", "trade_glob",   #"mid_primary", "pol_glob",
              "fdi", "arrivals", "cold", "visit") #"interpersonal_glob", , "ldtrade", "ltrade"
IO_IVs <- c("IOs_sum", "defense_alliance_active") #, "alliance_active"

coup_auto <- c("pce", "pce2", "pce3")
cw_auto   <- c("pce_cw", "pce2_cw", "pce3_cw")

shared_rhs <- unique(c(
  leader_IVs, regime_IVs, milit_leader_IVs, milit_IVs,
  gender_IVs, econ_IVs, stability_IVs, glob_IVs, IO_IVs
))

# ------------------------------------------------------------#
# 4. Model-specific RHS
# ------------------------------------------------------------#
coup_rhs <- unique(c(coup_auto, shared_rhs))

# ============================================================#
# STANDARDIZE CONTINUOUS RHS VARIABLES ONLY
# Leave IDs, DVs, and listed dummy vars alone
# ============================================================#
dummy_vars <- c("milreg", "mutiny6", "mid", "visit", "cold")

all_rhs_vars <- unique(c(coup_rhs))

vars_to_standardize <- setdiff(
  all_rhs_vars,
  c("ccode", "time_id", dummy_vars)
)

df_normal <- df

df <- df_normal %>%
  mutate(across(all_of(vars_to_standardize), ~ as.numeric(scale(.))))

# ------------------------------------------------------------#
# 4. Formulas
# ------------------------------------------------------------#
coup_fml <- as.formula(
  paste("coup_attempt ~", paste(coup_rhs, collapse = " + "))
)
# ------------------------------------------------------------#
# 5. Estimate models
# ------------------------------------------------------------#
coup_mod <- feglm(coup_fml, data = df, cluster=~ccode)

df$yhat <- predict(coup_mod, newdata=df, type="response")
years <- df %>%
  filter(!is.na(yhat))
summary(years$year) #these are the years that were included in the last regression; we want this to be 2025
months <-years %>%
  filter(year==2026)
summary(months$month) #this is the last month in the regression, assuming we get it updated to 2025; we want this to be 3 or 4 (March or April)
rm(years, months)
missing <- df %>%
  filter(year==2026 & month==4) %>%
  relocate(yhat) %>%
  filter(is.na(yhat))
table(missing$country) #these are the countries that won't show up in our map
df <- df %>%
  select(-yhat)
