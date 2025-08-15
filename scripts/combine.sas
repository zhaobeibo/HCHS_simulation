%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/combine_&sysdate..log"
             print= "&homepath./lst/combine_&sysdate..lst" new; run; 

*********************************************************************************************************
        
        PROGRAM NAME: combine_sudaan.sas
        PROGRAMMER: Beibo Zhao (updated from AQA)    
        DESCRIPTION: Combine results and calculate performance metrics for HCHS V3 simulation
        VERSION CONTROL:
                        - 24APR25: Initialize the code (AQA)
                        - Aug2025: Updated for consistency with R simulation framework (BZ)
*********************************************************************************************************;

%macro combine_con_betas(start=, end=, corr=, corr_full = , rr=, 
                       input_lib=, pop_lib=dt_betas, output_lib=outpath);

    /* Merge files containing beta estimates using sample data */
    data merge_betas;
        set &input_lib..&corr.&rr.&start. - &input_lib..&corr.&rr.&end.;
        if parm = 'intercept' then parm = 'Intercept';
    run;

    /* Append the true value coming from population estimates */
    proc sql;
        create table betas_samp_pop as
        select a.*, b.estimate as true 
        from merge_betas as a
        right join 
        &pop_lib..betas_pop_&corr. as b 
        on a.parm=b.parm;
    quit;

    /* Compute 95% confidence intervals */
    data betas_samp_pop; 
        set betas_samp_pop;
        uppercl = Estimate + 1.975 * Stderr; 
        lowercl = Estimate - 1.975 * Stderr; 
    run;

    /* Estimate quantities of interest: bias, ... */
    data betas_samp_pop_;
        set betas_samp_pop;
        /* Calculate statistics to summarize */
        inci = (uppercl >= true & lowercl <= true);
        bias = estimate - true;
        relbias = bias / true;
        /* PROBT */
        rejecth0 = (abs(t) > quantile('NORMAL',.975));
    run;

    /* Calculate summary statistics */
    proc means data=betas_samp_pop_ noprint nway;
        class parm;
        var true estimate bias relbias stderr inci rejecth0;
        output out=output_1 mean(true estimate bias relbias stderr inci
                rejecth0)=true estimate empbias relbias estse coverage
                prejecth0 std(estimate)=empse;
    run;

    /* Final calculations */
    data output;
        set output_1;
        relse = estse/empse - 1;
    run;

    /* Save output dataset */
    data &output_lib..&input_lib._&corr.&rr.;
        set output;
    run;


%mend combine_con_betas;


* s1;
* Complete %combine_con_betas macro calls for all scenarios;

* miss_ind_mar scenarios;
%combine_con_betas(start=1, end=500, corr=ind, corr_full=independent, rr=_rr_glm_, 
                 input_lib=s1);

%combine_con_betas(start=1, end=500,corr=ind, corr_full=independent, rr=_rr_nradj_, 
                 input_lib=s1);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_glm_, 
                 input_lib=s1);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_nradj_, 
                 input_lib=s1);

* miss_ind_mar_strat scenarios;
/*%combine_con_betas(corr=ind, corr_full=independent, rr=_rr_glm_strat_, */
/*                 input_lib=s1);*/
/**/
/*%combine_con_betas(corr=ind, corr_full=independent, rr=_rr_glm_agestrat_strat_, */
/*                 input_lib=s1);*/
/**/
/*%combine_con_betas(corr=ind, corr_full=independent, rr=_rr_nradj_strat_, */
/*                 input_lib=s1);*/
/**/
/*%combine_con_betas(corr=exch, corr_full=exchangeable, rr=_rr_glm_strat_, */
/*                 input_lib=s1);*/
/**/
/*%combine_con_betas(corr=exch, corr_full=exchangeable, rr=_rr_glm_agestrat_strat_, */
/*                 input_lib=s1);*/
/**/
/*%combine_con_betas(corr=exch, corr_full=exchangeable, rr=_rr_nradj_strat_, */
/*                 input_lib=s1);*/

* s2;
%combine_con_betas(start=1, end=500, corr=ind, corr_full=independent, rr=_, 
                       input_lib= s2 );
%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable,  rr=_, 
                       input_lib= s2 );


* s3;
%combine_con_betas(start=1, end=500, corr=ind, corr_full=independent, rr=_rr_glm_, 
                 input_lib=s3);

%combine_con_betas(start=1, end=500,corr=ind, corr_full=independent, rr=_rr_nradj_, 
                 input_lib=s3);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_glm_, 
                 input_lib=s3);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_nradj_, 
                 input_lib=s3);

* s4;
%combine_con_betas(start=1, end=500, corr=ind, corr_full=independent, rr=_rr_glm_, 
                 input_lib=s4);

%combine_con_betas(start=1, end=500,corr=ind, corr_full=independent, rr=_rr_nradj_, 
                 input_lib=s4);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_glm_, 
                 input_lib=s4);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_nradj_, 
                 input_lib=s4);

* s5;
%combine_con_betas(start=1, end=100, corr=ind, corr_full=independent, rr=_rr_glm_, 
                 input_lib=s5);

%combine_con_betas(start=1, end=100,corr=ind, corr_full=independent, rr=_rr_nradj_, 
                 input_lib=s5);

%combine_con_betas(start=1, end=100, corr=exch, corr_full=exchangeable, rr=_rr_glm_, 
                 input_lib=s5);

%combine_con_betas(start=1, end=100, corr=exch, corr_full=exchangeable, rr=_rr_nradj_, 
                 input_lib=s5);


 * s6;
%combine_con_betas(start=1, end=500, corr=ind, corr_full=independent, rr=_rr_glm_, 
                 input_lib=s6);

%combine_con_betas(start=1, end=500,corr=ind, corr_full=independent, rr=_rr_nradj_, 
                 input_lib=s6);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_glm_, 
                 input_lib=s6);

%combine_con_betas(start=1, end=500, corr=exch, corr_full=exchangeable, rr=_rr_nradj_, 
                 input_lib=s6);

proc printto; run;
