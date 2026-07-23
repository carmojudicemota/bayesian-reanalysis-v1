* Encoding: UTF-8.
*****Data preperation

COMPUTE estimate_total=(PercentRember + PercentCorrectMC + PercentCorrectOE)/3.
EXECUTE. 

COMPUTE TotalCorrectPercent=(TotalCorrect/5).
Execute. 

COMPUTE teaching_quality=MEAN(Clear,Prepared,Understood,FairAss,Excellent).
EXECUTE.


****Demographics

FREQUENCIES VARIABLES=Age Gender Race PsychCourses YearCollege TotalCorrect
  /STATISTICS=STDDEV MEAN MODE
  /FORMAT=DFREQ
  /ORDER=ANALYSIS.

****Evaluation scores by condition 

T-TEST GROUPS=Condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=TotalCorrect
  /CRITERIA=CI(.95).

***Mixed ANOVA of testing conditon and predictions about performance 

GLM PercentRember PercentCorrectMC PercentCorrectOE BY Condition
  /WSFACTOR=factor1 3 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Condition.

