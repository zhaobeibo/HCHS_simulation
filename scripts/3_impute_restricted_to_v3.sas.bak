%include "J:\HCHS\STATISTICS\GRAS\QAngarita\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/3_impute_restricted_v3_&sysdate..log"
			 print= "&homepath./lst/3_impute_restricted_v3_&sysdate..lst" new; run; 

data vars_labels;
    input modelrhs variable $20.; 
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

%macro impute_sudaan_v3(start=1, end=100, corr=exchangeable, rr=glm, nimpute=5);

* start loop;
%do i = &start. %to &end.;
			
  			* Temp subsets observations from v3 participants with no missing data;
   			* Samp contains all the ids from temp but includes those with both missing and non-visit at visit 2;
			proc sql;
					create table temp as
					select subid, miss_ind_mar
					from sample.samplemiss_&i.
					where subid in (select subid 
									from sample.samplemiss_&i. 
									where v_num = 3 and miss_ind_mar = 0 
									);
					create table samp as
					select *
					from sample.samplemiss_&i.
					where miss_ind_mar = 0 and subid in (select subid 
									from temp
									);
			quit;

			* create the sampling weight, age_strat_new and hisp_strat_new;
			data samp;
                set samp;

                /* weight adjusted for nomresponse */
                bghhsub_s2_nr = bghhsub_s2/ RR_&rr.;

				/* Create dummy indicators */
                age_strat_new = 1*(age_base>=45);
                hisp_strat_new=1*(hisp_strat='TRUE');
			run;


            /* Transform from long to wide */
            	/* This step is the preparation step for multiple imputation */
			data samp_1(rename=(x6=x6_1 y_bmi=y_bmi_1 y_gfr=y_gfr_1 bghhsub_s2_nr=w_nr_1)) 
					samp_2(rename=(x6=x6_2 y_bmi=y_bmi_2 y_gfr=y_gfr_2 bghhsub_s2_nr=w_nr_2)) 
					samp_3(rename=(x6=x6_3 y_bmi=y_bmi_3 y_gfr=y_gfr_3 bghhsub_s2_nr=w_nr_3));
			    set samp;
			    if v_num = 1 then output samp_1;
			    else if v_num = 2 then output samp_2;        
			    else if v_num = 3 then output samp_3;
			run;

			* Sort the 3 datasets;
			proc sort data = samp_1; by subid; run;
			proc sort data = samp_2; by subid; run;
			proc sort data = samp_3; by subid; run;

				* merging the 3 new datasets;
			data samp_wide;
                merge samp_1-samp_3;
                by subid;
            run;

				* sort the output datasets;
			proc sort data = samp_wide;
                 by subid;
            run;

                * obtain the # of missing values by var ; 
			proc means data = samp_wide noprint;
                 var  x17 x12 x18 y_bmi_1-y_bmi_3 age_strat_new
                            x6_2-x6_3 y_gfr_1-y_gfr_3
                            x14 w_nr_1 w_nr_2 w_nr_3;
            	output out= mi_miss nmiss=;
            run;

				* transpose the values from proc means;
            proc transpose data = mi_miss(drop=_TYPE_ _FREQ_) out=mi_long;
            run;

				* sort the values by # missing;
            proc sort data = mi_long;
                    by col1;
            run;

            proc sql noprint;
                    select distinct _name_ into:var separated by ' ' 
					from mi_long;
            quit;
            %put &var.;

			data samp_wide;
				set samp_wide;
				strat_psu = cats(strat_recoded,bgid);
			run;

			proc mi data=samp_wide seed=2021 nimpute=&nimpute out=samp_complete;	
				class strat_psu;
				fcs reg(x6_2 y_gfr_2 y_bmi_2 w_nr_2);
				var &var. strat_psu; 
            run;

			    /* Transform the imputed datasets from wide format to long */
				/* use the visit-specific adjusted weight */
            data samp_long;
                    set samp_complete;
                    v_num = 1;
                    x6 = x6_1;
                    y_bmi = y_bmi_1;
                    y_gfr = y_gfr_1;
					bghhsub_s2_nr = w_nr_1;
					bghhsub_s2_nr_v3 = w_nr_3;
                    output;

                    v_num = 2;
                    x6 = x6_2;
                    y_bmi = y_bmi_2;
                    y_gfr = y_gfr_2;
					bghhsub_s2_nr = w_nr_2;
					bghhsub_s2_nr_v3 = w_nr_3;
                    output;

                    v_num = 3;
                    x6 = x6_3;
                    y_bmi = y_bmi_3;
                    y_gfr = y_gfr_3;
					bghhsub_s2_nr = w_nr_3;
					bghhsub_s2_nr_v3 = w_nr_3;
                    output;
            run;

					* Fit regress model from sudaan using corr matrix; 
				* Looping over the 5 simulated datasets;
				%do j = 1 %to &nimpute; 
					data temp;
						set samp_long;
						if _imputation_ = &j;
					run;
				proc sort data = temp; 
					by strat_recoded bgid hhid subid; 
				run;  	
					options pagesize=60 linesize=80;
					proc regress data = temp filetype=sas r=&corr semethod=zeger;
						%if &corr. = exchangeable %then %do; 
							nest strat_recoded hhid / psulev=2 ;
						%end;
						%else %do;
							nest strat_recoded hhid;
						%end;
						weight bghhsub_s2_nr_v3; 
						model y_gfr = x17 x12 x18 y_bmi age_strat_new x6;
						output beta sebeta p_beta t_beta / filename=betas_mi_&corr._&i._&j filetype=sas replace;
					run;

					data betas_mi_&corr._&i._&j; 
						merge vars_labels betas_mi_&corr._&i._&j ;
						by modelrhs;
						_imputation_ = &j;
						ClassVal0 = '';
						DF = 1;
						rename beta = Estimate sebeta = StdErr p_beta = ProbChisq t_beta = WaldChisq;
						drop procnum modelno modelrhs;
					run;
				%end;
		* merging the &nimpute datasets;
		data outparms;
			set betas_mi_&corr._&i._1 - betas_mi_&corr._&i._&nimpute.;
		run;
		* combine estimates;
		proc mianalyze parms = outparms; 
			modeleffects intercept x17 x12 x18 y_bmi age_strat_new x6; 
			ods output ParameterEstimates = betas_mi_&corr._&i;
		run;			
		data dt_betas.bt_mi_v2_&corr._&rr._&i;
                        SET betas_mi_&corr._&i(RENAME=(UCLMEAN = UPPERCL LCLMEAN=LOWERCL ));
        run;
  %end;
%mend impute_sudaan_v3;

* glm;
* %impute_sudaan_v3();
* %impute_sudaan_v3(corr=independent);

* NRadj;
%impute_sudaan_v3(start=10, rr=NRadj);
* %impute_sudaan_v3(corr=independent, rr=NRadj);
