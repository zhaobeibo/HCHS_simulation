%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

libname sampmiss "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\with_miss";

proc printto log = "&homepath./logs/s4_sudaan_miss_&sysdate..log"
             print= "&homepath./lst/s4_sudaan_miss_&sysdate..lst" new; 
run; 

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

%macro impute_sudaan(start=, end=, corr=, corr_full=, miss=);
  %do i = &start. %to &end.;

  /*============================*
   | 1) Split to V1 / V2 / V3   |
   *============================*/
  data samplemiss_&i._;
/*    set  sampmiss.samplemiss_10pct_&i._;*/
      set  sampmiss.samplemiss_50pct_&i._;

	  	  where miss_ind_mar = 0; /* remove missing visits */

				if strat in (1,5) then strat_recoded = 1;
			else if strat in (2,6) then strat_recoded = 2;
			else if strat in (3,7) then strat_recoded = 3;
			else if strat in (4,8) then strat_recoded = 4;

  run;

  /* Order data */
  proc sort data = samplemiss_&i._; 
    by strat_recoded bgid hhid subid; 
  run;


  /* Fit regress model from SUDAAN using &corr matrix;
     In a simulation group meeting it was requested to use hhid as PSUs instead of bgid */
  options pagesize=60 linesize=80;
  proc regress data = samplemiss_&i._ filetype=sas r=&corr_full semethod=zeger;
    %if  &corr. = exch %then %do; 
      nest strat_recoded hhid / psulev=2 ;
    %end;
    %else %do;
      nest strat_recoded hhid;
    %end;
    weight bghhsub_s2_nr;    * use visit-specific adjusted weight;
    model y_gfr = x17 x12 x18 y_bmi age_strat_new x6;
    output beta sebeta p_beta t_beta / filename=betas_&corr._&i._ filetype=sas replace;
  run;
  
  /* Append parameter names to SUDAAN output */ 
  data test2.&corr._rr_glm_mask_&i;
    merge betas_&corr._&i._(rename=(BETA=Estimate SEBETA=Stderr P_BETA=ProbZ t_beta=t)) parms;  
    by modelrhs;
    length parm $ 20;
    format Estimate Stderr 12.4;
    drop procnum modelno modelrhs;
  run;       

  %end;
%mend impute_sudaan;

* miss_ind_mar;
%impute_sudaan(start=1, end=500, corr=ind, corr_full=independent, miss= miss_ind_mar);

proc printto; run;
