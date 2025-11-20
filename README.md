**Replication Materials for "The Capacity Paradox: Rethinking When Environmental Human Rights Matter" by Annika Dengel**

These files support the replication of the analyses presented in the paper. The main analysis code, ehr_code.R, is designed to run with R version 4.5.0.

**Required Files:**
ehr recognition years.csv — Contains the recognition years for environmental rights by country.

V-Dem-CY-Full+Others-v15.rds — V-Dem dataset version 15.

qog_ei_ts_sept21.dta — University of Gothenburg’s Quality of Government Environmental Indicators.

POLCON_2021_VDEM.xlsx — POLCON data from V-Dem.

**Index Construction:**
The dependent variable, the environmental index, was created using creating_env_index.do in Stata 19.5. The resulting dataset is env_index_gls.csv.

The leave-one-out indices were generated with env_index_robustness.do. The resulting dataset is env_index_gls_robustness.csv.

**Data Handling:**
To streamline replication, the files final_panel_data.rds and imp_data_gs.dta can be directly loaded at specific lines in ehr_code.R (line 811 and 1214, respectively).

**Contact:**
For questions or clarifications regarding these materials, please contact adengel@ucsd.edu.
