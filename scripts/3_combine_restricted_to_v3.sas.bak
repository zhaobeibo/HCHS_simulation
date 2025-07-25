%include "J:\HCHS\STATISTICS\GRAS\QAngarita\HCHS_simulation\scripts\_init.sas";

proc printto log = "&homepath./logs/3_combine_restricted_to_v3_&sysdate..log"
			 print= "&homepath./lst/3_combine_restricted_to_v3_&sysdate..lst" new; run; 
*********************************************************************************************************
        
        PROGRAM NAME: 3_combine_restricted_to_v3.sas

        PROGRAMMER: AQA    

        DESCRIPTION:  

        VERSION CONTROL:
						- 29may25: Initialize the code 	

*********************************************************************************************************;


%macro combine_betas_rr(start=1, end=100, corr=exchangeable, rr=glm);
* merge files containing beta estimates using sample data;

data merge_betas;
	%if &corr = exchangeable %then %do;
			set dt_betas.betas_mi_v2_exch_&rr._&start. - dt_betas.betas_mi_v2_exch_&rr._&end.;
	%end;
	%else %do;
			set dt_betas.betas_mi_v2_ind_&rr._&start. - dt_betas.betas_mi_v2_ind_&rr._&end.;
	%end;

	if parm = 'intercept' then parm = 'Intercept';
run;

* append the true value coming from population estimates; 
proc sql;
     create table betas_samp_pop as
       select a.*, b.estimate as true 
	   from
       merge_betas as a
       right join 
       dt_betas.betas_pop_&corr. as b 
	   on a.parm=b.parm;
quit;

* Compute 95% confidence intervals;
data betas_samp_pop; 
	set betas_samp_pop;
	uppercl = Estimate + 1.975 * Stderr; 
	lowercl = Estimate - 1.975 * Stderr; 
run;

* Estimate quantities of interest: bias, ...; 
data betas_samp_pop_;
                set betas_samp_pop;
                /*calculate statistics to summarize*/
                inci=(uppercl>=true & lowercl<=true);
                bias=estimate-true;
                relbias=bias/true;
                /*PROBT*/
                rejecth0=(abs(t)>quantile('NORMAL',.975));
run;

proc means data=betas_samp_pop_
                noprint nway;
                class parm;
                var true estimate bias relbias stderr inci rejecth0;
                output out=output_1 mean(true estimate bias relbias stderr inci
                        rejecth0)=true estimate empbias relbias estse coverage
                        prejecth0 std(estimate)=empse;
run;


data output;
      set output_1;
      relse=estse/empse-1;
run;

ods rtf file="&homepath./output/reports/3_combine_mi_restricted_&corr._&rr._&sysdate..rtf" style=journal bodytitle;
        proc print data=output noobs label;
		var parm true estimate empbias relbias empse estse relse coverage prejecth0;
        label parm='Effect' true='True Value' estimate='Estimate'
                        empbias='Empirical Bias' relbias='Relative Bias'
                        empse='Empirical SE' estse='Estimated SE'
                        coverage='Coverage' prejecth0='P(reject H0)'
                        relse='Relative SE difference';
run;
ods rtf close;
%mend combine_betas_rr;


%combine_betas_rr();
%combine_betas_rr(corr=independent);
%combine_betas_rr(rr=nradj, corr=independent);
%combine_betas_rr(rr=nradj);


proc printto; run;


