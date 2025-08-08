
* set macro variables;
%let homepath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\;
%let outpath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\derived\summary\;
%let output = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\output\;
%let popfile = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\raw\pop\population_3visits_Dec2024.csv;

* Set library names;
/*libname v3data "&homepath./v3data";*/
libname sample "&homepath./data/derived/sample";
/*libname v3_outpt "&homepath./v3/sasdata";*/
libname dt_betas "&homepath./data/derived/betas";

libname s1 "&homepath./data/derived/betas/s1";
libname s2 "&homepath./data/derived/betas/s2";
libname s3 "&homepath./data/derived/betas/s3";
libname s4 "&homepath./data/derived/betas/s4";
libname s5 "&homepath./data/derived/betas/s5";
libname s6 "&homepath./data/derived/betas/s6";

libname bs1 "&homepath./data/derived/betas/bin/s1";
libname bs2 "&homepath./data/derived/betas/bin/s2";
libname bs3 "&homepath./data/derived/betas/bin/s3";
libname bs4 "&homepath./data/derived/betas/bin/s4";
libname bs5 "&homepath./data/derived/betas/bin/s5";
libname bs6 "&homepath./data/derived/betas/bin/s6";
/*libname betas_b "&homepath./data/derived/betas/bin";*/
libname output "&output.";
libname outpath "&outpath.";


