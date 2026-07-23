* Encoding: UTF-8.

*Authors said: Speci cally,
we conducted paired-samples t-tests using a Bonferroni-
corrected alpha level (α = .003). Effect sizes were calculated
using Cohen’s d and interpreted according to traditional con-
ventions (Cohen, 1992). 

*There is a big table of the t-test results in the article (table 1) the result we want to reproduce: 
    * t(36)= 8.53, p < .001, Cohen’s d = 1.56

DATASET ACTIVATE DataSet2.
T-TEST PAIRS=PRE_Analyze WITH POST_Analyze (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*we reproduced it properly, but df's are off: t(29)= -8.532, p < .001, Cohen’s d = −1.56 
    *I will include the missing data participants and see what is happening

RMV /PRE_Analyze_1=SMEAN(PRE_Analyze) /POST_Analyze_1=SMEAN(POST_Analyze).

T-TEST PAIRS=PRE_Analyze_1 WITH POST_Analyze_1 (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*I now included all the cases and numbers have changed now. The t value turned from reproducible into no longer reproducible
    *t(36)= -9.809, p < .001, Cohen’s d = −1.61
*However I realized that the authors did not specifically mention what the d.f was in the table, and the means/sd in the table corresponds to the results I got when removing the extra data


*Extra key result: Students showed a significant improvement in their ability to interpret data by relating results to the original hypothesis from pretest... to posttest…  t(36) = 6.15, p < .001, d = 1.14.
    
DATASET ACTIVATE DataSet2.
T-TEST PAIRS=PRE_Intrdata WITH POST_Intrdata (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*reproduced:   t(28) = -6.151, p < .001, d = -1.14
