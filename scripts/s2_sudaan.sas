%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/s2_sudaan_&sysdate..log"
			 print= "&homepath./lst/s2_sudaan_&sysdate..lst" new; run; 
*********************************************************************************************************
        
        PROGRAM NAME: regress_sudaan.sas

        PROGRAMMER: AQA    

        DESCRIPTION:  

        VERSION CONTROL:
						- 20FEB25: Initialize the code 	
			 			- 30may25: Uncomment the miss_ind_mar = 0 so we obtain the complete data case

*********************************************************************************************************;

* Run sudaan models for the 100 files;
%macro regress_sudaan(start=1, end=1000, corr=, corr_full=);
  %do i = &start %to &end;
	
	* use only available data; 	
 	proc sql;
		create table samplemiss_&i._ as
		select *
	    from sample.samplemiss_&i.
	    where miss_ind_mar = 0; /* Select only records with no missingness */
	quit;

	* order data;
	proc sort data = samplemiss_&i._; 
		by strat_recoded bgid hhid subid; 
	run;  	

	* Fit regress model from sudaan using &corr matrix;  
		* In a simulation group meeting it was requested to use hhid as PSUs instead of bgid;
	options pagesize=60 linesize=80;
	proc regress data = samplemiss_&i._ filetype=sas r=&corr_full semethod=zeger;
		%if  &corr. = exch %then %do; 
			nest strat_recoded hhid / psulev=2 ;
		%end;
		%else %do;
			nest strat_recoded hhid;
		%end;
		weight bghhsub_s2; 
		model y_gfr = x17 x12 x18 y_bmi age_strat_new x6;
		output beta sebeta p_beta t_beta / filename=betas_&corr._&i._ filetype=sas replace;
	run;
	
	* Append parameter names to sudaan output; 
	data dt_b_s2.&corr._&i;
		merge betas_&corr._&i._(rename=(BETA=Estimate SEBETA=Stderr P_BETA=ProbZ t_beta=t)) parms;  
		by modelrhs;
		length parm $ 20;
		drop procnum modelno modelrhs;
	run;
	
  %end;
%mend regress_sudaan;

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

/* Execute the macro for exchangeable correlation matrix */
%regress_sudaan(corr=ind, corr_full=independent);

/* Execute the macro with independent correlation matrix */
%regress_sudaan(corr=exch, corr_full=exchangeable);


proc printto; run;


