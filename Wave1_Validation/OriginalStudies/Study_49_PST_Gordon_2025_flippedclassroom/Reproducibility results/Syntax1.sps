* Encoding: UTF-8.
*I used the qualtrics clean analysis data excel sheet. It took a few minutes to copy it and remove the other sheet. But variables are present as
*average variables that I could directly plug in:

*key result chosen: 
A paired-samples t-test on final multiple-choice items showed no significant difference in participants’ performance for questions on lectured...versus nonlectured content ... t(27) = 0.47, p = .319, d = 0.09

DATASET ACTIVATE DataSet2.
T-TEST PAIRS=VIDEO_MC_AVG WITH NONVIDEO_MC_AVG (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*Extra result: Similarly, a paired-samples t-test on the final short essay test did not show a signi cant difference in participants’ average accuracy for questions on lectured... versus nonlectured content ... t(27)= 1.20, p = .119, d= 0.23.

T-TEST PAIRS=NONVIDEO_WRITTEN_AVG WITH VIDEO_WRITTEN_AVG (PAIRED)
  /ES DISPLAY(TRUE) STANDARDIZER(SD)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*T stat came out negative because I put nonvideoxvideo instead of the other way around. All results are fully reproducible
