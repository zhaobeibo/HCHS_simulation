%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/s4_sudaan_miglm_&sysdate..log"
			 print= "&homepath./lst/s4_sudaan_miglm_&sysdate..lst" new; run; 

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
data samp_1 samp_2 samp_3;
  set sample.samplemiss_&i;
  where &miss. = 0;   /* keep only non-missing per your flag */
  if      v_num = 1 then output samp_1;
  else if v_num = 2 then output samp_2;
  else if v_num = 3 then output samp_3;
run;

/* Sort all three and build unique ID lists for V2/V3 */
proc sort data = samp_1;                           by subid; run;
proc sort data = samp_2(keep = subid) out = v2_ids nodupkey; by subid; run;
proc sort data = samp_3(keep = subid) out = v3_ids nodupkey; by subid; run;

/*=====================================================*
 | 2) Add PARTICIPANT_V2 / PARTICIPANT_V3 to the V1   |
 |    dataset BEFORE missingness + MI                 |
 *=====================================================*/
data samp_1_w_flags;
  merge samp_1(in = in_v1)
        v2_ids(in = in_v2)
        v3_ids(in = in_v3);
  by subid;
  if in_v1;
  PARTICIPANT_V2 = (in_v2 = 1);
  PARTICIPANT_V3 = (in_v3 = 1);

    /* Strata indicators: 1/0 flags */
  length strata1 strata2 strata3 8;
  strata1 = (strat in (1, 5));
  strata2 = (strat in (2, 6));
  strata3 = (strat in (3, 7));
run;

/* Optional: quick check */
proc freq data = samp_1_w_flags;
  tables PARTICIPANT_V2 PARTICIPANT_V3 / missing;
run;

/*====================================================*
 | 3) Inject ~20% MCAR missingness for V1 variables   |
 *====================================================*/
data samp_1_miss;
  set samp_1_w_flags;
  array missvars age_base x12 x14;   /* variables to blank */
  do over missvars;
    if rand('uniform') < 0.05 then missvars = .;
  end;
run;

/* Optional: sanity check */
proc means data = samp_1_miss n nmiss;
  var age_base x12 x14;
run;

/*========================================*
 | 4) Multiple Imputation on V1 dataset   |
 *========================================*/
proc mi data = samp_1_miss seed = 2021 nimpute = 5 out = samp_complete;
  class strata1 strata2 strata3 x12 x14;
  var strata1 strata2 strata3 age_base x12 x14 bghhsub_s2;
  fcs reg(age_base);          /* continuous */
  fcs logistic(x12 x14);      /* binary/categorical */
run;

/*===========================*
 | 5) IPW prediction models  |
 *===========================*/

/* = Visit 2: logistic */
proc logistic data = samp_complete descending noprint;
  by _Imputation_;
  class strata1 strata2 strata3 x12 x14 PARTICIPANT_V2;
  model PARTICIPANT_V2 = strata1 strata2 strata3 age_base x12 x14 ;
  output out = pred_v2_imp(keep = _Imputation_ subid xb_v2) xbeta = xb_v2;
run;

proc means data = pred_v2_imp nway noprint;
  class subid;
  var xb_v2;
  output out = pred_v2_bar(drop = _type_ _freq_) mean = xb_v2_bar;
run;

/* = Visit 3: logistic */
proc logistic data = samp_complete descending noprint;
  by _Imputation_;
  class strata1 strata2 strata3 x12 x14 PARTICIPANT_V2 PARTICIPANT_V3;
  model PARTICIPANT_V3 = strata1 strata2 strata3 age_base x12 x14 PARTICIPANT_V2;
  output out = pred_v3_imp(keep = _Imputation_ subid xb_v3) xbeta = xb_v3;
run;

proc means data = pred_v3_imp nway noprint;
  class subid;
  var xb_v3;
  output out = pred_v3_bar(drop = _type_ _freq_) mean = xb_v3_bar;
run;

/*===========================*
 | 6) Compute response rates |
 *===========================*/
proc sort data = pred_v2_bar; by subid; run;
proc sort data = pred_v3_bar; by subid; run;

data samp_ipw;
  merge pred_v2_bar(rename = (xb_v2_bar = lp_v2))
        pred_v3_bar(rename = (xb_v3_bar = lp_v3));
  by subid;

  /* Predicted response rates */
  if not missing(lp_v2) then RR_V2 = 1 / (1 + exp(-lp_v2));
  if not missing(lp_v3) then RR_V3 = 1 / (1 + exp(-lp_v3));
run;




/* --- Starting from samp_ipw already created --- */
data samp_ipw_long(keep = subid v_num rr_glm_mask);
  set samp_ipw;

  /* Visit 1: rr_glm_mask = 1 */
  v_num  = 1; rr_glm_mask = 1;      output;

  /* Visit 2: rr_glm_mask = RR_V2 */
  v_num  = 2; rr_glm_mask = RR_V2;  output;

  /* Visit 3: rr_glm_mask = RR_V3 */
  v_num  = 3; rr_glm_mask = RR_V3;  output;
run;

proc sort data = samp_ipw_long; by subid v_num; run;

/* Bring in the base sample restricted by &miss. */
data sampmiss;
  set sample.samplemiss_&i;
  where &miss. = 0;
run;

proc sort data = sampmiss; by subid v_num; run;

/* Merge the rr_glm_mask and compute nonresponse-adjusted weight */
data samp;
  merge sampmiss(in = a)
        samp_ipw_long(in = b);
  by subid v_num;

  if a;

  /* weight adjusted for nonresponse */
  bghhsub_s2_nr = bghhsub_s2 / rr_glm_mask;
run;


	/* Sort samp by subid for merging */
	proc sort data = samp;
		by subid;
	run;

	/* Step 1: Create a dataset with subid in V3 */
	proc sql;
	   create table valid_subid as
	   select distinct subid
	   from samp
	   where v_num = 3;
	quit;

	/* Sort valid_subid by subid for merging */
	proc sort data = valid_subid;
		by subid;
	run;
			
	/* Step 2: Merge samp with valid_subid to keep only valid subids */
	data samp_filtered;
	   merge samp(in=a) valid_subid(in=b);
	   by subid;
	   if b;  /* Only keep records from samp where subid is in valid_subid */
	run;

	/* Step 3: Create a dataset with the value of bghhsub_s2_nr for v_num = 3 for each subid */
	proc sql;
	   create table temp as
	   select subid, bghhsub_s2_nr as bghhsub_s2_v3_nr
	   from samp_filtered
	   where v_num = 3;
	quit;

	/* Step 4: Merge the new variable back into the filtered dataset */
	data samplemiss_&i._;
	   merge samp_filtered temp;
	   by subid;
	run;

	/* Order data */
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
		weight bghhsub_s2_v3_nr;    *use visit-3 adjusted weight;
		model y_gfr = x17 x12 x18 y_bmi age_strat_new x6;
		output beta sebeta p_beta t_beta / filename=betas_&corr._&i._ filetype=sas replace;
	run;
	
	* Append parameter names to sudaan output; 
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
%impute_sudaan(start=101, end=200, corr=ind, corr_full=independent, miss= miss_ind_mar);


proc printto; run;
