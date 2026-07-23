* Encoding: UTF-8.
*key result we want: However, psychology students performed better
on the items as seniors, answering significantly
more items correctly (M 81%, SD 14.3%)
than nonpsychology students (M 69%, SD
17.0%), t(154) 3.26, p .001, Mdiff 12.3%,
95% CI [4.8%, 19.7%], d 0.74.

*We want t(154) = 3.26, p = .001, Cohen’s d = 0.74


*** in the first 6 pieces of code, i was trying to figure out why i kept getting 26 participants instead of 156. 
T-TEST GROUPS=Speciality(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=@16ItemQuizFollowupPerformance
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

FREQUENCIES VARIABLES=Speciality
  /ORDER=ANALYSIS.

*Ignore this, it was before I came to the realization I clarified underneath

RECODE Speciality (0=1) (1=2) INTO class.
EXECUTE.

T-TEST GROUPS=class(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=@16ItemQuizFollowupPerformance
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

FREQUENCIES VARIABLES=@16ItemQuizFollowupPerformance
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=@16ItemQuizFollowupPerformance
  /ORDER=ANALYSIS.

*At this point, I realized that the csv was not imported correctly. So I manually copied and pasted the Specialty section (psychology
*vs non psychology) and the @16ItemQuizFollowupPerformance row data). This got me really close but the results are off
*by a few numbers. When you open the data. Copy paste the csv data relevant rows into spss. make sure you are deleting
*the extra rows under row 156 as the numbers are spaced out for some reason.

T-TEST GROUPS=Speciality(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=@16ItemQuizFollowupPerformance
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*Additional result analyses: The superior quiz performance of the psychol-
ogy students is likely driven by the higher number
of additional psychology courses they took... compared to nonpsychology
students... t(154)= 16.06, p <.001, d= 3.63.

*I did the same here, highlighted the whole column from csv and pasted into spss, then deleted the extra cases under row 156
    
T-TEST GROUPS=Speciality(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=NumPsychClass
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
