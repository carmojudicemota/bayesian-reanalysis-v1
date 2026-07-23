* Encoding: UTF-8.
*The authors stated: Note. N= 194. Paired-sample t tests for attitudinal measures pre- and postlecture. M= mean; SD= standard deviation.
*p < .05. **p < .01. ***p < .001.

*To get the means and SD of trust before and after intervention
 
       DESCRIPTIVES VARIABLES=trust1
  /STATISTICS=MEAN STDDEV SEMEAN.

DESCRIPTIVES VARIABLES=trust2
  /STATISTICS=MEAN STDDEV SEMEAN.


*So I just ran the paired sample t-test for the trust result, the paired sample t test (as seen on the output, gives us different 
    *cohen's d, while the authors reported -.36. This is because they chose the M1-M2/SD pooled formula, which I tried and got 
    *the same result from it

DATASET ACTIVATE DataSet1.
T-TEST PAIRS=trust1 WITH trust2 (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.


*Exploratory analyses, same formula but for the (would like to learn more about 2008 study)
    
 DESCRIPTIVES VARIABLES=interest1
  /STATISTICS=MEAN STDDEV SEMEAN.

DESCRIPTIVES VARIABLES=interest2
  /STATISTICS=MEAN STDDEV SEMEAN.

DATASET ACTIVATE DataSet1.
T-TEST PAIRS=interest1 WITH interest2 (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*After getting the results, I realized that the authors accidentally messed up. They reported the surprise means for the interest
    *means, Because the numbers I got for interest are almost identical to the res    DESCRIPTIVES VARIABLES=surprise1
  /STATISTICS=MEAN STDDEV SEMEAN.

DESCRIPTIVES VARIABLES=surprise2
  /STATISTICS=MEAN STDDEV SEMEAN.

DATASET ACTIVATE DataSet1.
T-TEST PAIRS= surprise1 WITH surprise2 (PAIRED)
    /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

  
