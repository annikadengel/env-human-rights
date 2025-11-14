ssc install swindex

use "/Users/annikadengel/Desktop/imputed_data_filtered.dta", clear

// basic usage is "swindex x1 x2 x3, generate(my_index)" 

// variables for the index:
// wdi_fossil*
// edgar_pm25*
// renew_output
// wdi_co2*
// *will need to flip 

drop if missing(wdi_fossil, edgar_pm25, renew_output, wdi_co2)

swindex wdi_fossil edgar_pm25 renew_output wdi_co2, ///
    generate(env_index_gls) ///
    flip(wdi_fossil edgar_pm25 wdi_co2) ///
    displayw

// weights
// wdi_fossil       .34434826
// edgar_pm25       .23935157
//enew_output       .42924378
//   wdi_co2       -.01294362

// less correlation w/ the other variables = greater weight 

export delimited using "/Users/annikadengel/Desktop/env_index_gls.csv", replace
