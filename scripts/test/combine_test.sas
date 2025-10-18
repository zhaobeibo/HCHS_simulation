%include "J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/combine_test_&sysdate..log"
             print= "&homepath./lst/combine_test_&sysdate..lst" new; run; 

*********************************************************************************************************
        
        PROGRAM NAME: combine_sudaan.sas
        PROGRAMMER: Beibo Zhao (updated from AQA)    
        DESCRIPTION: Combine results and calculate performance metrics for HCHS V3 simulation
        VERSION CONTROL:
                        - 24APR25: Initialize the code (AQA)
                        - Aug2025: Updated for consistency with R simulation framework (BZ)
                        - Modified: Handle missing datasets from 1-1000, skip non-existent files
*********************************************************************************************************;

%macro combine_con_betas(start=, end=, corr=, corr_full = , rr=, 
                       input_lib=, pop_lib=dt_betas, output_lib=outpath);

    /* Create a temporary dataset to hold all existing files */
    data merge_betas;
        /* Initialize empty dataset with correct structure */
        length parm $20;
        if 0; /* This creates structure but no observations */
    run;
    
    /* Counter for successful merges */
    %let merge_count = 0;
    
    /* Loop through each file number and check if it exists */
    %do i = &start %to &end;
        /* Check if dataset exists using SASHELP.VTABLE or by attempting to open */
        %if %sysfunc(exist(&input_lib..&corr.&rr.&i.)) %then %do;
            /* Dataset exists, append it */
            data merge_betas;
                set merge_betas &input_lib..&corr.&rr.&i.;
                if parm = 'intercept' then parm = 'Intercept';
            run;
            %let merge_count = %eval(&merge_count + 1);
            %put NOTE: Successfully merged dataset &input_lib..&corr.&rr.&i. (File &merge_count);
        %end;
        %else %do;
            %put WARNING: Dataset &input_lib..&corr.&rr.&i. does not exist - skipping;
        %end;
    %end;
    
    %put NOTE: Total datasets merged: &merge_count out of %eval(&end - &start + 1) possible files;
    
    /* Check if we have any data to process */
    %let dsid = %sysfunc(open(merge_betas));
    %let nobs = %sysfunc(attrn(&dsid, nobs));
    %let rc = %sysfunc(close(&dsid));
    
    %if &nobs = 0 %then %do;
        %put ERROR: No datasets found to merge for &input_lib..&corr.&rr. - exiting macro;
        %return;
    %end;

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
        /* Add information about number of files merged */
        files_merged = &merge_count;
    run;

    /* Save output dataset */
    data &output_lib..&input_lib._&corr.&rr.;
        set output;
    run;

    /* Clean up temporary datasets */
    proc datasets library=work nolist;
        delete merge_betas betas_samp_pop betas_samp_pop_ output_1 output;
    quit;

%mend combine_con_betas;


* s1 - MI scenarios;
* miss_ind_mar scenarios;
/*%combine_con_betas(start=1, end=100, corr=ind, corr_full=independent, rr=_double_rr_, */
/*                 input_lib=test1);*/

%combine_con_betas(start=1, end=200, corr=ind, corr_full=independent, rr=_rr_glm_mask_, 
                 input_lib=test2);

proc printto; run;
