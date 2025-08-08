%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/s6_bin_sudaan_&sysdate..log"
			 print= "&homepath./lst/s6_bin_sudaan_&sysdate..lst" new; run; 

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

%macro impute_sudaan(start=, end=, corr=, corr_full=, rr=, miss=);
  %do i = &start. %to &end.;

	data samplemiss_&i._;	
		set sample.samplemiss_&i;

        /* weight adjusted for nomresponse */
        bghhsub_s2_nr = bghhsub_s2/ &rr.;
  
        where &miss. = 0;
	run;


	/* Order data */
	proc sort data = samplemiss_&i._; 
		by strat_recoded bgid hhid subid; 
	run;

* Fit rlogist model from sudaan using &corr matrix;  
		* In a simulation group meeting it was requested to use hhid as PSUs instead of bgid;
	options pagesize=60 linesize=80;
	proc rlogist data = samplemiss_&i._ filetype=sas r=&corr_full semethod=zeger;
		%if  &corr. = exch %then %do; 
			nest strat_recoded hhid / psulev=2 ;
		%end;
		%else %do;
			nest strat_recoded hhid;
		%end;
		weight bghhsub_s2_nr;    *use visit-specific adjusted weight;
		model y_bin_gfr = x17 x12 x18 y_bmi age_strat_new x6;
		output beta sebeta p_beta t_beta / filename=betas_&corr._&i._ filetype=sas replace;
	run;
	
	* Append parameter names to sudaan output; 
	data bs6.&corr._&rr._&i;
		merge betas_&corr._&i._(rename=(BETA=Estimate SEBETA=Stderr P_BETA=ProbZ t_beta=t)) parms;  
		by modelrhs;
		length parm $ 20;
		format Estimate Stderr 12.4;
		drop procnum modelno modelrhs;
	run;       


  %end;
%mend impute_sudaan;

* miss_ind_mar;
%impute_sudaan(start=1, end=500, corr=ind, corr_full=independent, rr=rr_glm, miss= miss_ind_mar);
%impute_sudaan(start=1, end=500, corr=ind, corr_full=independent, rr=RR_NRadj, miss= miss_ind_mar);

%impute_sudaan(start=1, end=500, corr=exch, corr_full=exchangeable, rr=rr_glm, miss= miss_ind_mar);
%impute_sudaan(start=1, end=500, corr=exch, corr_full=exchangeable, rr=RR_NRadj, miss= miss_ind_mar);

* miss_ind_mar_strat;
/*%impute_sudaan(corr=ind, corr_full=independent, rr=rr_glm_strat, miss= miss_ind_mar_strat);*/
/*%impute_sudaan(corr=ind, corr_full=independent, rr=RR_glm_agestrat_strat, miss= miss_ind_mar_strat);*/
/*%impute_sudaan(corr=ind, corr_full=independent, rr=RR_NRadj_strat, miss= miss_ind_mar_strat);*/
/**/
/*%impute_sudaan(corr=exch, corr_full=exchangeable, rr=rr_glm_strat, miss= miss_ind_mar_strat);*/
/*%impute_sudaan(corr=exch, corr_full=exchangeable, rr=RR_glm_agestrat_strat, miss= miss_ind_mar_strat);*/
/*%impute_sudaan(corr=exch, corr_full=exchangeable, rr=RR_NRadj_strat, miss= miss_ind_mar_strat);*/


proc printto; run;
