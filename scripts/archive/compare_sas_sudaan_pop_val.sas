/* Compare SAS vs SUDAAN results by outcome and correlation structure */
%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";
proc printto log = "&homepath./logs/compare_sas_sudaan_&sysdate..log"
			 print= "&homepath./lst/compare_sas_sudaan_&sysdate..lst" new; run; 

*********************************************************************************************************
        
        PROGRAM NAME: compare_sas_sudaan.sas
        SOURCE:        
        DESCRIPTION:   Compare SAS GENMOD vs SUDAAN results by outcome type and correlation structure
        VERSION CONTROL:
							27mar25: create file						
*********************************************************************************************************;

* Set system options;
options mergenoby=warn ls=95 ps=54 nodate mprint formchar="|----|+|---+=|-/\<>*";

* Macro to compare results for each outcome x correlation combination;
%macro compare_results(outcome=, corr=);
	
	* Read SAS and SUDAAN datasets;
	%if &outcome. = cont %then %do;
		data sas_data;
			set dt_betas.betas_pop_sas_&corr.;
			software = "SAS";
			outcome = "Continuous";
			correlation = "%upcase(&corr.)";
		run;
		
		data sudaan_data;
			set dt_betas.betas_pop_&corr.;
			software = "SUDAAN";
			outcome = "Continuous";
			correlation = "%upcase(&corr.)";
		run;
	%end;
	%else %if &outcome. = bin %then %do;
		data sas_data;
			set dt_betas.betas_pop_bin_sas_&corr.;
			software = "SAS";
			outcome = "Binary";
			correlation = "%upcase(&corr.)";
		run;
		
		data sudaan_data;
			set dt_betas.betas_pop_bin_&corr.;
			software = "SUDAAN";
			outcome = "Binary";
			correlation = "%upcase(&corr.)";
		run;
	%end;
	
	* Combine datasets;
	data combined_&outcome._&corr.;
		set sas_data sudaan_data;
	run;
	
	* Create comparison dataset;
	proc sort data=combined_&outcome._&corr.; by parm software; run;
	
	proc transpose data=combined_&outcome._&corr. out=wide_&outcome._&corr. prefix=;
		by parm outcome correlation;
		id software;
		var estimate stderr probz t;
	run;
	
	* Calculate differences and ratios;
	data comparison_&outcome._&corr.;
		set wide_&outcome._&corr.;
		
		* Calculate differences (SAS - SUDAAN);
		estimate_diff = SAS_estimate - SUDAAN_estimate;
		stderr_diff = SAS_stderr - SUDAAN_stderr;
		probz_diff = SAS_probz - SUDAAN_probz;
		t_diff = SAS_t - SUDAAN_t;
		
		* Calculate relative differences (as percentages);
		if SUDAAN_estimate ne 0 then estimate_rel_diff = (estimate_diff / SUDAAN_estimate) * 100;
		if SUDAAN_stderr ne 0 then stderr_rel_diff = (stderr_diff / SUDAAN_stderr) * 100;
		if SUDAAN_probz ne 0 then probz_rel_diff = (probz_diff / SUDAAN_probz) * 100;
		if SUDAAN_t ne 0 then t_rel_diff = (t_diff / SUDAAN_t) * 100;
		
		* Calculate absolute relative differences;
		abs_estimate_rel_diff = abs(estimate_rel_diff);
		abs_stderr_rel_diff = abs(stderr_rel_diff);
		abs_probz_rel_diff = abs(probz_rel_diff);
		abs_t_rel_diff = abs(t_rel_diff);
		
		format estimate_diff stderr_diff probz_diff t_diff 12.6
		       estimate_rel_diff stderr_rel_diff probz_rel_diff t_rel_diff 8.2
		       abs_estimate_rel_diff abs_stderr_rel_diff abs_probz_rel_diff abs_t_rel_diff 8.2;
	run;
	
	* Print comparison results;
	title1 "Comparison of SAS vs SUDAAN Results";
	title2 "Outcome: %upcase(&outcome.) | Correlation: %upcase(&corr.)";
	title3 "Raw Values";
	
	proc print data=combined_&outcome._&corr. noobs;
		var parm software estimate stderr probz t;
		format estimate stderr 12.6 probz t 8.4;
	run;
	
	title3 "Differences and Relative Differences (%)";
	proc print data=comparison_&outcome._&corr. noobs;
		var parm outcome correlation 
		    SAS_estimate SUDAAN_estimate estimate_diff estimate_rel_diff
		    SAS_stderr SUDAAN_stderr stderr_diff stderr_rel_diff
		    SAS_probz SUDAAN_probz probz_diff probz_rel_diff
		    SAS_t SUDAAN_t t_diff t_rel_diff;
	run;
	
	* Summary statistics of differences;
	title3 "Summary of Absolute Relative Differences (%)";
	proc means data=comparison_&outcome._&corr. n mean std min max;
		var abs_estimate_rel_diff abs_stderr_rel_diff abs_probz_rel_diff abs_t_rel_diff;
	run;
	
%mend compare_results;

* Create overall comparison dataset;
data all_comparisons;
	length outcome $10 correlation $12 software $10;
	stop; * Initialize empty dataset;
run;

* Run comparisons for all combinations;
%compare_results(outcome=cont, corr=ind);
data all_comparisons; set all_comparisons combined_cont_ind; run;

%compare_results(outcome=cont, corr=exch);
data all_comparisons; set all_comparisons combined_cont_exch; run;

%compare_results(outcome=bin, corr=ind);
data all_comparisons; set all_comparisons combined_bin_ind; run;

%compare_results(outcome=bin, corr=exch);
data all_comparisons; set all_comparisons combined_bin_exch; run;

* Overall summary comparison;
title1 "Overall Comparison Summary";
title2 "All Outcomes and Correlation Structures";

proc print data=all_comparisons;
	var parm outcome correlation software estimate stderr probz t;
	format estimate stderr 12.6 probz t 8.4;
run;

* Create side-by-side comparison for easy viewing;
proc sort data=all_comparisons; by outcome correlation parm software; run;

proc transpose data=all_comparisons out=final_comparison;
	by outcome correlation parm;
	id software;
	var estimate stderr probz t;
run;

data output.final_comparison_formatted;
	set final_comparison;
	
	* Calculate differences for final summary;
	if _name_ = 'Estimate' then do;
		diff = SAS - SUD;
		if SUD ne 0 then rel_diff_pct = (diff / SUD) * 100;
	end;
	else if _name_ = 'Stderr' then do;
		diff = SAS - SUD;
		if SUD ne 0 then rel_diff_pct = (diff / SUD) * 100;
	end;
	
	format diff 12.6 rel_diff_pct 8.2 SAS SUD 12.6;
run;

title1 "Final Side-by-Side Comparison";
title2 "SAS vs SUDAAN by Parameter, Outcome, and Correlation";

proc print data=output.final_comparison_formatted noobs;
	var outcome correlation parm _name_ SAS SUD diff rel_diff_pct;
run;

* Export results to Excel for further analysis;
proc export data=output.final_comparison_formatted
	outfile="&output./comparison_sas_sudaan_results.xlsx"
	dbms=xlsx replace;
	sheet="Comparison_Summary";
run;

* Clean up temporary datasets;
proc datasets library=work nolist;
	delete combined_: comparison_: wide_: sas_data sudaan_data;
quit;

proc printto; run;
