These are the replication materials that accompany the paper "The Capacity Paradox: Rethinking When Environmental Human Rights Matter" by Annika Dengel. 

The following files are required to run the main analysis code, ehr_code.R, using R version 4.5.0. 

1) ehr recognition years.csv
2) V-Dem-CY-Full+Others-v15.rds
3) qog_ei_ts_sept21.dta
4) POLCON_2021_VDEM.xlsx

The environmental index, which serves as the dependent variable of the analysis, was contructed in Stata 19.5 with creating_env_index.do. The resulting dataset is env_index_gls.csv.

To avoid the first time-intensive imputation process, the file final_panel_data.rds can be read-in at line 811 of ehr_code.R. Similarly, for the second imputation process, the file imp_data_gs.dta can be read-in at line 1214. 

For any questions regarding these materials, please reach out to adengel@ucsd.edu. 
