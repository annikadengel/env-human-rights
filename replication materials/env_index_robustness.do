use "/Users/annikadengel/Desktop/imputed_data_filtered.dta", clear

// basic usage is "swindex x1 x2 x3, generate(my_index)" 

// variables for the index:
// wdi_fossil*
// edgar_pm25*
// renew_output
// wdi_co2*
// *will need to flip 

drop if missing(wdi_fossil, edgar_pm25, renew_output, wdi_co2)

// the normal index
swindex wdi_fossil edgar_pm25 renew_output wdi_co2, ///
    generate(env_index_gls) ///
    flip(wdi_fossil edgar_pm25 wdi_co2) ///
    displayw

// Weights
//   wdi_fossil       .3450802
//   edgar_pm25       .23706578
// renew_output       .42812825
//      wdi_co2       -.01027422

// less correlation w/ the other variables = greater weight 


// leave-one-out indices: 
// without wdi_fossil
swindex edgar_pm25 renew_output wdi_co2, ///
    generate(env_index_no_fossil) ///
    flip(edgar_pm25 wdi_co2) ///
    displayw

// Weights
//   edgar_pm25       .34395715
// renew_output       .23049097
//      wdi_co2       .42555188


// without edgar_pm25
swindex wdi_fossil renew_output wdi_co2, ///
    generate(env_index_no_pm25) ///
    flip(wdi_fossil wdi_co2) ///
    displayw


// without renew_output
swindex wdi_fossil edgar_pm25 wdi_co2, ///
    generate(env_index_no_renew) ///
    flip(wdi_fossil edgar_pm25 wdi_co2) ///
    displayw
	
// Weights
//   wdi_fossil       .43267964
// renew_output       .23282158
//      wdi_co2       .33449879


// without wdi_co2
swindex wdi_fossil edgar_pm25 renew_output, ///
    generate(env_index_no_co2) ///
    flip(wdi_fossil edgar_pm25) ///
    displayw

// Weights
//   wdi_fossil       .4137924
//   edgar_pm25       .42731221
// renew_output       .15889539

export delimited using "/Users/annikadengel/Desktop/env_index_gls_robustness.csv", replace
