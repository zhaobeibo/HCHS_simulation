/* This program fits the binary GEE on the population level and get the true coefficients for inference */
%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";
proc printto log = "&homepath./logs/pop_bin_sas_&sysdate..log"
			 print= "&homepath./lst/pop_bin_sas_&sysdate..lst" new; run; 

*********************************************************************************************************
        
        PROGRAM NAME: pop_bin_sas.sas
        SOURCE:        
        DESCRIPTION:   Fit the binary model on the population data using SAS PROC GENMOD and 
							get the true coefficients for inference 
        VERSION CONTROL:
							27mar25: create file						
*********************************************************************************************************;

* Set system options;
options mergenoby=warn ls=95 ps=54 nodate mprint formchar="|----|+|---+=|-/\<>*";

* Import .csv dataset;
proc import datafile="&popfile" out=pop_data(keep=bgid hhid subid y_bmi y_gfr y_bin_gfr x12
        x17 x18 x14 x6 age_base strat hisp_strat v_num) dbms=csv replace;
        getnames=yes;
        guessingrows=100;
run;

* Derive age_strat_new and hisp_strat_new dataset; 
data pop;
	set pop_data;
	age_strat_new = 1*(age_base>=45);
	hisp_strat_new = 1*(hisp_strat='TRUE');
	
	if strat in (1,5) then strat_recoded = 1;
	else if strat in (2,6) then strat_recoded = 2;
	else if strat in (3,7) then strat_recoded = 3;
	else if strat in (4,8) then strat_recoded = 4;
run;

proc sort data = pop; by strat_recoded; run;

* Macro that fit a SAS GENMOD model with a binary logistic response;
%macro pop_sas(corr=);
	options pagesize=60 linesize=80;
	
	title "[SAS GENMOD] &corr. correlation structure - Binary Model";
	
	proc genmod data=pop descending;
		class subid;
		model y_bin_gfr = x17 x12 x18 y_bmi age_strat_new x6 / dist=binomial link=logit;
		repeated subject=subid / corr=&corr.;
		ods output GEEEmpPEst=betas_&corr._temp; 
	run;
	
	* Process and format the output to match SUDAAN format;
	data dt_betas.betas_pop_bin_sas_&corr.;
		set betas_&corr._temp;
		length Parm $ 20;
		
		* Map parameter names;
		if Parm = 'Intercept' then Parm = 'Intercept';
		else if Parm = 'x17' then Parm = 'x17';
		else if Parm = 'x12' then Parm = 'x12';
		else if Parm = 'x18' then Parm = 'x18';
		else if Parm = 'y_bmi' then Parm = 'y_bmi';
		else if Parm = 'age_strat_new' then Parm = 'age_strat_new';
		else if Parm = 'x6' then Parm = 'x6';
		
		* Rename variables to match SUDAAN output format;
		Estimate = Estimate;
		Stderr = StdErr;
		ProbZ = ProbZ;
		t = ZValue;
		
		keep Parm Estimate Stderr ProbZ t;
	run;
	
	* Clean up temporary dataset;
	proc datasets library=work nolist;
		delete betas_&corr._temp;
	quit;
%mend pop_sas;

* Run SAS GENMOD macro for different correlation structures;
title '[SAS GENMOD] Independent correlation structure - Binary Model';
	%pop_sas(corr=ind);

title '[SAS GENMOD] Exchangeable correlation structure - Binary Model';
	%pop_sas(corr=exch);

proc printto; run;
