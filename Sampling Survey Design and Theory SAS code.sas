
libname w  "D:\courses\BSTcourse\sampling survey and theory\project\data";
proc contents data=w.demo_h;
run;

proc sort data=w.demo_h;
by seqn;
run;
proc sort data=w.dpq_h;
by seqn;
run;

proc sort data=w.fsq_h;
by seqn;
run;
proc sort data=w.ocq_h;
by seqn;
run;
proc sort data=w.slq_h;
by seqn;
run;


data w.nhams_survey;
merge w.demo_h w.dpq_h w.fsq_h w.ocq_h w.slq_h;
by seqn;
run;



PROC FREQ Data=w.nhams_survey;
	TABLES DPQ020 OCQ260 FSDHH SLD010H RIAGENDR DMDCITZN/ Nocum Missing;
RUN;

proc contents data=nhams_survey;
run;

data w.nhams_project;
length depression $15. employment $15. sleep $15. marital $15. education $30.citizenship $10. gender $10. ;
set nhams_survey;
if DPQ020 in (7,9) then Depression="";
else if DPQ020 = 0 then Depression="No";
else if DPQ020 in (1,2,3) then Depression="Yes";

if RIDRETH1=1 then race="Hispanic";
else if RIDRETH1=2 then race="Hispanic";
else if RIDRETH1=3 then race="NHW";
else if RIDRETH1=4 then race="NHB";
else if RIDRETH1=5 then race="Other";

if OCQ260 =1 then employment="Private";
else if OCQ260 in (2,3,4) then employment="Government";
else if OCQ260 in (5,6) then employment="Self-employed";
else if OCQ260 in (77,99) then employment="";

if SLD010H in (77,99) then sleep="";
else if SLD010H in (2,3,4,5,6) then sleep="Deprivation";
else if SLD010H in (7,8,9,10,11,12) then sleep="Enough" ; 

if RIAGENDR=1 then gender="Male";
else if RIAGENDR=2 then gender="Female";

if DMDEDUC2 in (1,2,3) then education="Less than College";
else if DMDEDUC2 in (4,5) then education="College";
else if DMDEDUC2 in (7,9) then education="";

if DMDMARTL =1 then marital="Married";
else if DMDMARTL in (2,3,4,6) then marital="Widowed/Divorced/Separated/living with partner";
else if DMDMARTL =5 then marital="Never Married";
else if DMDMARTL in (77,99) then marital="";

if DMDCITZN in (7,9) then citizenship="";
else if DMDCITZN=. then citizenship="";
else citizenship=DMDCITZN;
run;

**Table 1 the frequency;
PROC SURVEYFREQ Data=w.nhams_project ;
	TABLES depression race employment sleep gender education marital citizenship FSDHH ;
	WEIGHT WTINT2YR;
RUN;

**Table 1 the percentage;
PROC SURVEYFREQ Data=w.nhams_project;
	TABLES (race employment sleep gender education marital citizenship FSDHH)*depression /CHISQ COL NOSTD ;
	WEIGHT WTINT2YR;
RUN;

*Table 2;
proc surveylogistic data= w.nhams_project;
class employment (ref="Government") 
      FSDHH (ref="1")
      sleep (ref="Enough") 
      gender (ref="Female")
      education (ref="College")
      citizenship (ref="1")
      marital (ref="Married")
	  race (ref="Hispanic")
      /param=ref;
model depression (Event="Yes")=employment FSDHH sleep gender education citizenship marital race;
weight  WTINT2YR;
run;

*we drop the variables that not significant;
proc surveylogistic data= w.nhams_project;
class FSDHH (ref="1")
      sleep (ref="Enough") 
      gender (ref="Female")
      marital (ref="Married")
      /param=ref;
model depression (Event="Yes")= FSDHH sleep gender  marital ;
weight  WTINT2YR;
run;

