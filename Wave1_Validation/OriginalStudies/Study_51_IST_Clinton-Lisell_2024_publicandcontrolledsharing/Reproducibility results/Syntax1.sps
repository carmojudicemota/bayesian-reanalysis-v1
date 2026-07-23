* Encoding: UTF-8.
*Authors did an independent sample t-test of "perceivewd learning" which was labelled as "knowledge skills" in this dataset. The comparison was for
controlled / public sharing, which I assumed was "control0share1" because it had 2 groups "0/1" and was obviously named

*Result reported:  t(100) = 2.79, p < .01, Cohen’s d = 0.55

DATASET ACTIVATE DataSet2.
T-TEST GROUPS=control0share1(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=knowledgeskills
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*results were negative, which made sense, I switched it around:

DATASET ACTIVATE DataSet2.
T-TEST GROUPS=control0share1(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=knowledgeskills
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*Fully reproducible

*Extra result was anxiety, I just replaced knowledge skills variable with anxiety variable:

T-TEST GROUPS=control0share1(1 0)
  /MISSING=ANALYSIS
  /VARIABLES=anxiety
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*Fully reproducible

