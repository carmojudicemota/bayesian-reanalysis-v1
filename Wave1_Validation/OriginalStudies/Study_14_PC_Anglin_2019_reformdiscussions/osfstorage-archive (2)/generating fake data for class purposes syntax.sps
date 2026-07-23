* Encoding: UTF-8.
**Generating fake data for class demonstrations and projects


FREQUENCIES VARIABLES=intro_gen_data 
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(intro_gen_data = 1).
VARIABLE LABELS filter_$ 'intro_gen_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=intro_gen_data_coded
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.


FREQUENCIES VARIABLES=intro_st_data
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(intro_st_data = 1).
VARIABLE LABELS filter_$ 'intro_st_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=intro_st_data_coded
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.


FREQUENCIES VARIABLES=adv_gen_data
  /ORDER=ANALYSIS.


USE ALL.
COMPUTE filter_$=(adv_gen_data = 1).
VARIABLE LABELS filter_$ 'adv_gen_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


FREQUENCIES VARIABLES=adv_gen_data_coded
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.

FREQUENCIES VARIABLES=adv_st_data
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(adv_st_data = 1).
VARIABLE LABELS filter_$ 'adv_st_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


FREQUENCIES VARIABLES=adv_st_data_coded
  /ORDER=ANALYSIS.


FILTER OFF.
USE ALL.
EXECUTE.

FREQUENCIES VARIABLES=grad_gen_data
  /ORDER=ANALYSIS.


USE ALL.
COMPUTE filter_$=(grad_gen_data = 1).
VARIABLE LABELS filter_$ 'grad_gen_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


FREQUENCIES VARIABLES=grad_gen_data_coded
  /ORDER=ANALYSIS.


FILTER OFF.
USE ALL.
EXECUTE.

FREQUENCIES VARIABLES=grad_st_data
  /ORDER=ANALYSIS.


USE ALL.
COMPUTE filter_$=(grad_st_data = 1).
VARIABLE LABELS filter_$ 'grad_st_data = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


FREQUENCIES VARIABLES=grad_st_data_coded
  /ORDER=ANALYSIS.

FILTER OFF.
USE ALL.
EXECUTE.
