%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

libname sampmiss "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\with_miss";

proc printto log = "&homepath./logs/s4_genmod_miss_&sysdate..log"
             print= "&homepath./lst/s4_genmod_miss_&sysdate..lst" new;
run;


/*------------------------------------------------------------*
 | PARMS mapping (NO Level1)                                   |
 | - Use ONLY Parameter/parm mapping                           |
 | - age_strat_new: we will keep the row with non-missing Z     |
 *------------------------------------------------------------*/
data parms;
  length parm $20 Parameter $32;
  input modelrhs parm $ Parameter $;
  datalines;
1 Intercept     Intercept
2 x17           x17
3 x12           x12
4 x18           x18
5 y_bmi         y_bmi
6 age_strat_new age_strat_new
7 x6            x6
;
run;

%macro impute_genmod(start=, end=, corr = );


  %do i = &start %to &end;

    /*============================*
     | 0) Read sample + recode    |
     *============================*/
    data samplemiss_&i;
      /* set sampmiss.samplemiss_10pct_&i; */
      set sampmiss.samplemiss_50pct_&i._;

	  where miss_ind_mar = 0; /* remove missing visits */

      if strat in (1,5) then strat_recoded = 1;
      else if strat in (2,6) then strat_recoded = 2;
      else if strat in (3,7) then strat_recoded = 3;
      else if strat in (4,8) then strat_recoded = 4;
    run;

    proc sort data=samplemiss_&i;
      by strat_recoded bgid hhid subid;
    run;

	ods exclude all;
    ods output GEEEmpPEst = betas_&corr._&i;   /* robust (empirical) PE table */
    proc genmod data=samplemiss_&i ;
	  class hhid age_strat_new(ref = '0');
      weight bghhsub_s2_v3_nr;  * use visit-3 adjusted weight;
      model y_gfr = x17 x12 x18 y_bmi age_strat_new x6
        / dist=normal link=identity;
      repeated subject=hhid / type=ind ;
    run;
    ods output close;
	ods exclude none;



/*============================================================*
 | 2) Standardize GENMOD output to match downstream merge       |
 |    - Ignore Level1 entirely                                  |
 |    - Keep only rows with non-missing Z (drops ref rows)       |
 |    - For age_strat_new this keeps the estimable row (Z!=.)    |
 *============================================================*/

data betas_&corr._&i._prep;
  set betas_&corr._&i;

  length Parameter $32;

  /* Map GENMOD name -> Parameter used for merge */
  Parameter = strip(Parm);

  /* Keep only parameters we care about */
  if upcase(Parameter) not in ("INTERCEPT","X17","X12","X18","Y_BMI","AGE_STRAT_NEW","X6") then delete;

  /* Keep only estimable rows (reference rows have Z = .) */
  if missing(Z) then delete;

  /* Create t-stat placeholder (combine code expects 't') */
  t = Z;

  keep Parameter Estimate Stderr ProbZ t;
run;

/* Sort for merge */
proc sort data=betas_&corr._&i._prep; by Parameter; run;
proc sort data=parms;                 by Parameter; run;

/* Final output dataset matches your combine macro expectations */
data test2.&corr._rr_glm_mask_&i;
  merge betas_&corr._&i._prep(in=a)
        parms(in=b);
  by Parameter;
  if a;

  format Estimate Stderr 12.4;
  keep modelrhs parm Estimate Stderr ProbZ t;
run;


  %end;

%mend impute_genmod;

/* miss_ind_mar */
%impute_genmod(start=1, end=500, corr = ind);


proc printto;
run;
