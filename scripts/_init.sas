
* set macro variables;
%let homepath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\;
%let outpath = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\summary\;
%let popfile = J:\HCHS\STATISTICS\GRAS\Beibo\Computing Requests\HCHS_simulation\data\pop_values\population_3visits_Dec2024.csv;

* Set library names;
/*libname v3data "&homepath./v3data";*/
libname sample "&homepath./data/sample";
/*libname v3_outpt "&homepath./v3/sasdata";*/
libname popvalue "&homepath./data/pop_values";

libname s4_00miss "&homepath./data/betas/s4_00miss";
libname s4_20miss "&homepath./data/betas/s4_20miss";
libname s6_00miss "&homepath./data/betas/s6_00miss";
libname s6_20miss "&homepath./data/betas/s6_20miss";

libname outpath "&outpath.";


