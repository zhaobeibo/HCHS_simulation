

%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

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

%macro impute_genmod(start=, end=, corr = , rr_glm = );


  %do i = &start %to &end;


	/*============================*
	 | 0a) Import CSV + handle NA |
	 *============================*/
	proc import datafile="&datapath./samplemiss_&i..csv"
	    out=samplemiss_&i._raw
	    dbms=csv
	    replace;
	    guessingrows=max;
	run;


	/*========================================================*
 | Semi-automatic conversion of character vars containing |
 | "NA" to numeric vars with SAS missing (.)              |
 *========================================================*/

/* 1) Get all character variable names */
proc contents data=samplemiss_&i._raw out=_vars_(keep=name type) noprint;
run;

proc sql noprint;
    select name
    into :char_vars separated by ' '
    from _vars_
    where type = 2;
quit;

/* 2) Find which character variables contain at least one "NA" */
%macro find_na_vars;
    %global na_vars;
    %let na_vars=;

    %let n=%sysfunc(countw(&char_vars));
    %do j=1 %to &n;
        %let v=%scan(&char_vars, &j);

        proc sql noprint;
            select count(*)
            into :has_na trimmed
            from samplemiss_&i._raw
            where strip(&v) = "NA";
        quit;

        %if &has_na > 0 %then %do;
            %let na_vars=&na_vars &v;
        %end;
    %end;
%mend;

%find_na_vars

%put NOTE: Variables containing NA = &na_vars;

/* 3) Convert only those variables to numeric */
data samplemiss_&i._raw1;
    set samplemiss_&i._raw;

    %let n_na=%sysfunc(countw(&na_vars));

    %do j=1 %to &n_na;
        %let v=%scan(&na_vars, &j);
        &v._num = input(strip(&v), best.);
    %end;

    drop
    %do j=1 %to &n_na;
        %let v=%scan(&na_vars, &j);
        &v
    %end;
    ;

    rename
    %do j=1 %to &n_na;
        %let v=%scan(&na_vars, &j);
        &v._num = &v
    %end;
    ;
run;

	/*============================*
	 | 0) Read sample + recode    |
	 *============================*/
	data samplemiss_&i;
	  set samplemiss_&i._raw1;

	  where miss_ind_mar = 0; /* remove missing visits */

	  if strat in (1,5) then strat_recoded = 1;
	  else if strat in (2,6) then strat_recoded = 2;
	  else if strat in (3,7) then strat_recoded = 3;
	  else if strat in (4,8) then strat_recoded = 4;

		if upcase(strip(age_strat)) = 'TRUE' then age_strat_new = 1;
		else if upcase(strip(age_strat)) = 'FALSE' then age_strat_new = 0;
		else age_strat_new = .;
	run;

    proc sort data=samplemiss_&i;
      by strat_recoded bgid hhid subid;
    run;

	ods exclude all;
    ods output GEEEmpPEst = betas_&corr._&i;   /* robust (empirical) PE table */
    proc genmod data=samplemiss_&i ;
	  class hhid age_strat_new(ref = '0');
      weight &rr_glm.;  
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
data missout.&corr._rr_glm_&i;
  merge betas_&corr._&i._prep(in=a)
        parms(in=b);
  by Parameter;
  if a;

  format Estimate Stderr 12.4;
  keep modelrhs parm Estimate Stderr ProbZ t;
run;


  %end;

%mend impute_genmod;



%let datapath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\sample\sample_miss00pct\;
libname missout "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\betas\s4_00miss\";
libname sampmiss "&datapath.";
/* miss_ind_mar */
/*%impute_genmod(start=1, end=100, corr = ind, rr_glm = IPW_V3_ACROSS_VISIT_v1);*/
%impute_genmod(start=1, end=100, corr = ind, rr_glm = IPW_V3_ACROSS_VISIT_v2);

