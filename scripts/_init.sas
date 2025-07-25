
* set macro variables;
%let homepath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\;
%let output = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\output;
%let popfile = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\raw\pop\population_3visits_Dec2024.csv;

* Set library names;
/*libname v3data "&homepath./v3data";*/
libname sample "&homepath./data/derived/sample";
/*libname v3_outpt "&homepath./v3/sasdata";*/
libname dt_betas "&homepath./data/derived/betas";
/*libname betas_b "&homepath./data/derived/betas/bin";*/
libname output "&output.";


