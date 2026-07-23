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

T-TEST GROUPS=Condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=estimate_total
  /CRITERIA=CI(.95).

T-TEST PAIRS=PercentRember PercentRember PercentCorrectMC WITH PercentCorrectMC PercentCorrectOE 
    PercentCorrectOE (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

***Perceptions of teaching effectivness 

T-TEST GROUPS=Condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=  Clear Prepared Understood FairAss Excellent
  /CRITERIA=CI(.95).


***Pearsons's Correlations 

CORRELATIONS
  /VARIABLES=TotalCorrect PercentRember PercentCorrectOE PercentCorrectMC  Clear Prepared Understood 
    FairAss Excellent
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

***Nonparametric analyses


NPAR TESTS
  /M-W= TotalCorrect BY Condition(1 2)
  /MISSING ANALYSIS.

NPAR TESTS
  /M-W= estimate_total BY Condition(1 2)
  /MISSING ANALYSIS.

NPAR TESTS
  /FRIEDMAN=PercentRember PercentCorrectMC PercentCorrectOE
  /KENDALL=PercentRember PercentCorrectMC PercentCorrectOE
  /MISSING LISTWISE.

NPAR TESTS
  /M-W= Clear Prepared Understood FairAss Excellent BY Condition(1 2)
  /MISSING ANALYSIS.

CORRELATIONS
  /VARIABLES=TotalCorrect PercentRember PercentCorrectOE PercentCorrectMC  Clear Prepared Understood 
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
NONPAR CORR
  /VARIABLES=TotalCorrect PercentRember PercentCorrectOE PercentCorrectMC  Clear Prepared Understood 
  /PRINT=BOTH TWOTAIL NOSIG
  /MISSING=PAIRWISE.


*****Reanalyses requested in review process*******

***Repeated measures analysis of predictions about performance 

GLM PercentRember PercentCorrectMC PercentCorrectOE
  /WSFACTOR=factor1 3 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1.
  
  
  ***MANOVA of testing conditon and predictions about performance 

GLM PercentRember PercentCorrectMC PercentCorrectOE BY Condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ
  /CRITERIA=ALPHA(.05)
  /DESIGN= Condition.


****Analysis of overall teaching quality 
  
  CORRELATIONS 
  /VARIABLES=Clear Prepared Understood FairAss Excellent 
  /PRINT=TWOTAIL NOSIG 
  /MISSING=PAIRWISE.

RELIABILITY
  /VARIABLES=Clear Prepared Understood FairAss Excellent
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

T-TEST GROUPS=Condition(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=teaching_quality
  /CRITERIA=CI(.95).

***Pearsons's Correlations 

CORRELATIONS
  /VARIABLES=TotalCorrect PercentRember PercentCorrectOE PercentCorrectMC  teaching_quality
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
