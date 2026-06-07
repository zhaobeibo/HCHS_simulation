# Scripts:

import_sample_data.sas: 	Converts the samplemiss_i.csv files to sas format
					It also creates the variable strat_recoded and the adjusted bghhsub_s2_nr 
				
						Input:  data/raw/samplemiss_i.csv
						Output: data/derived/samplemiss_i.sas7bdat
 
pop_bin_sudaan.sas: 		Estimate the population parameters in the binomial model using SUDAAN procedures

						Input: data/raw/pop/population_3visits_Dec2024.csv
						Output: data/processed/betas/bin/betas_pop_bin_&corr

pop_cont_sudaan.sas:		Estimate the population parameters in the continuous model using SUDAAN procedures

						Input: data/raw/pop/population_3visits_Dec2024.csv
						Output: data/processed/betas/betas_pop.&corr
 
1_impute_sudaan.sas: 		Multiple imputation (MI - 10 imputed datasets) for all missing visits (V2 and V3) 
					 + parameter estimation using PROC REGRESS (SUDAAN) 

						Input:  data/derived/samplemiss_i.sas7bdat
						Output: data/processed/betas/betas_mi_&corr._&i

1_combine_mi_sudaan.sas:	Use the betas estimated in the imputed samples to estimate coverage probabilities.

						Input:  data/processed/betas/betas_mi_&corr._&i
						Output: output/reports/combine_mi_sudaan_&corr_&sysdate


regress_sudaan.sas:		No MI + V1 weight (Full sample) parameter estimates using SUDAAN procedures

						Input: data/derived/samplemiss_i.sas7bdat
						Output: data/processed/betas/betas_&corr._&i



combine_sudaan_pop.sas:	Use the betas estimated in the full (no MI) samples to estimate coverage probabilities.

						Input:  data/processed/betas/betas_&corr._&i
						Output: output/reports/combine_sudaan_&corr_&sysdate


