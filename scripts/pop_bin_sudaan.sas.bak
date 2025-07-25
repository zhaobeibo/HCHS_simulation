%include "J:\HCHS\STATISTICS\GRAS\QAngarita\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/pop_bin_sudaan_&sysdate..log"
			 print= "&homepath./lst/pop_bin_sudaan_&sysdate..lst" new; run; 

*********************************************************************************************************
        
        PROGRAM NAME: pop_bin_sudaan.sas

        SOURCE:        

        DESCRIPTION:   Fit the binary model on the population data and 
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
        age_strat_new=1*(age_base>=45);
        hisp_strat_new=1*(hisp_strat='TRUE');
		
		if strat in (1,5) then strat_recoded = 1;
		else if strat in (2,6) then strat_recoded = 2;
		else if strat in (3,7) then strat_recoded = 3;
		else if strat in (4,8) then strat_recoded = 4;
run;
proc sort data = pop; by strat_recoded; run;

* Macro that fit a SUDAAN model with a continuous normal response;
%macro pop(corr=);
	options pagesize=60 linesize=80;
	proc rlogist data = pop filetype=sas r=&corr semethod=zeger;
		%if &corr. = exchangeable %then %do; 
			nest strat_recoded bgid hhid / psulev=2 ;
		%end;
		%else %do;
			nest strat_recoded bgid hhid;
		%end;
		weight _one_;
		model y_bin_gfr = x17 x12 x18 y_bmi age_strat_new x6;
		output beta sebeta p_beta t_beta / filename=betas_&corr. filetype=sas replace;
	run;
	* Append parameter names to sudaan output; 
	data betas_b.betas_pop_bin_&corr;
		merge betas_&corr.(rename=(BETA=Estimate SEBETA=Stderr P_BETA=ProbZ t_beta=t)) parms;  
		by modelrhs;
		length parm $ 20;
		drop procnum modelno modelrhs;
	run;
%mend pop;

* Create dataset with parms labels;
data parms;
length Parm $ 15;
input modelrhs parm $; 
datalines;
1 Intercept
2 x17
3 x12
4 x18
5 y_bmi
6 age_strat_new
7 x6
;
run;

* Run SUDAAN macro;
title '[SUDAAN]Independent correlation structure';
	%pop(corr=independent);
title '[SUDAAN]Exchangeable correlation structure';
	%pop(corr=exchangeable);

proc printto; run;
