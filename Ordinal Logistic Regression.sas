***************************************************************************************************
 Filename:		Homework Week 4.sas

 Date:			September 2022

 Author:		Lagos

 Purpose:		Test the hypothesis that there is no association between general health and poverty 
				after adjusting for physical activity and depression in CHIS 2009;
****************************************************************************************************;

/* Bring in your data */
libname s '/home/u61014676/ANA 630/Data';
data hm4; set s.adult; run;
proc contents data=hm4;

* Research Question:  Can health be predicted by poverty levels after adjusting for physical 
	activity and depression?;

* Null hypothesis: Health cannot be predicted by poverty levels after adjusting for physical 
	activity and depression in CHIS 2009?;
	
* general health(1=good or better, 2=neutral, 3=bad) = poverty(1=0-99% FPL, 2=100-199% FPL, 3=200-299% FPL
														4=300% FPL and above) + depression(1=no, 2=yes) + 
														physical activity(1=no/low, 2=yes);

proc means data=hm4;
	var AB1 POVLL AJ32 AE25AMIN;
run;

proc freq data=hm4;
	tables AB1 POVLL AJ32 AE25AMIN;
run;

data fl; set hm4
  (where = 	((AB1 in (1,2,3,4,5))		/* health var */
  			and (POVLL in (1,2,3,4))	/* poverty */
  			and (AJ32 in (1,2,3,4,5))	/* depression */
  			and (AE25AMIN>=0)));		/* activity */
  			
	if 1 <= AB1 <=3 then HEALTH = 1;	/* HEALTH(1) = good or better health */
	if AB1 = 4 then HEALTH = 2;			/* HEALTH(2) = nuetral health */
  	if AB1 = 5 then HEALTH = 3;			/* HEALTH(3) = bad health */
  	
  	/* leaving POVLL as is */ 			/* POVLL(1) = 0-99% FPL */
  										/* POVLL(2) = 100-199% FPL */
  										/* POVLL(3) = 200-299% FPL */
  										/* POVLL(4) = 300% FPL AND ABOVE */
  										
  	if AJ32 = 5 then DEPRESSED = 1;		/* DEPRESSED(1) = not felt depressed in the last 30 days */
 	if 1<=AJ32<=4 then DEPRESSED = 2;	/* DEPRESSED(2) = felt depressed in the last 30 days */
  	
  	if 0<=AE25AMIN<=29 then PHYS = 1; 	/* PHYS(1) = no to low physical activity 0-29 mins per day on average*/
  	if 30<=AE25AMIN then PHYS = 2;		/* PHYS(2) = high physical activity, > 30 per day on average*/
run;

/* check data, n = 12,944*/
proc freq data=fl;
	table HEALTH POVLL DEPRESSED PHYS;
run;

proc means data=fl n nmiss min max mean std kurt skew;
	var HEALTH POVLL DEPRESSED PHYS;
run;

proc univariate data=fl noprint;
	histogram HEALTH POVLL DEPRESSED PHYS;
run;

proc corr data=fl noprob;
	var HEALTH POVLL DEPRESSED PHYS;
run;

/* Variables */

%let allVar = HEALTH POVLL DEPRESSED PHYS;
%let depVar = HEALTH;
%let contVar = DEPRESSED PHYS;
%let exposVar = POVLL;
%let modelVar = POVLL DEPRESSED PHYS;
%let model2Var = POVLL PHYS;

/* Template */
proc freq data=fl;  *table 1;
  tables (&contVar) * &exposVar / chisq;
   title ' Table 1';
run;

proc freq data=fl;  *table 2;
  tables (&modelVar) * &depVar / chisq;
  title ' Table 2';
run;

*Multicollinearity;
proc reg data=fl ;
  model &depVar= &modelVar /  vif;
run;
quit;

*******************************************************************************************************
*This does the ordinal regression in proc logistic.  This gives a p-value to test proportional odds 
assumption.  If we reject the null hypothesis, we reject the assumption of proportionnal odds and need 
to do polychotomous/multinomial;

proc logistic data=fl descending;
  class POVLL (ref = '4') DEPRESSED (ref='1') PHYS (ref='2')
		/ param=reference;  
  model &depVar= &modelVar /  rl lackfit;
  title 'Model 1';
run;

proc logistic data=fl descending;
  class POVLL (ref = '4') DEPRESSED (ref='1') PHYS (ref='2')
		/ param=reference;  
  model &depVar= &model2Var /  rl lackfit;
  title 'Model 2';
run;

*proportional odds assumption is not violated based on failing to reject the null that the we have 
proportional odds (p-value =  0.2728);

/* example statement for ordinal log regression statement: for a one 
unit decrease in poverty, we expect a 5.320 (increase) in the odds 
of being in a higher level of general health (good and above), 
given all of the other variables in the model are held constant. */ 

/* not required, only here as part of template */
proc logistic data=fl descending;
  class POVLL (ref = '4') DEPRESSED (ref='1') PHYS (ref='2')
		/ param=reference;  
  model &depVar= &modelVar /  link=glogit rl lackfit;
run;