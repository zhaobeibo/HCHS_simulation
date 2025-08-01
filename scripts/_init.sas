
* set macro variables;
%let homepath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\;
%let output = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\output;
%let popfile = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\raw\pop\population_3visits_Dec2024.csv;

* Set library names;
/*libname v3data "&homepath./v3data";*/
libname sample "&homepath./data/derived/sample";
/*libname v3_outpt "&homepath./v3/sasdata";*/
libname dt_betas "&homepath./data/derived/betas";
libname dt_b_s1 "&homepath./data/derived/betas/s1";
libname dt_b_s2 "&homepath./data/derived/betas/s2";
libname dt_b_s3 "&homepath./data/derived/betas/s3";
libname dt_b_s4 "&homepath./data/derived/betas/s4";
libname dt_b_s5 "&homepath./data/derived/betas/s5";
libname dt_b_s6 "&homepath./data/derived/betas/s6";
/*libname betas_b "&homepath./data/derived/betas/bin";*/
libname output "&output.";


