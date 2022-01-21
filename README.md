# SHOtolithSelection

These are the files used for selecting silver hake otoliths. 

There are two sets of selections: one for the otolith backlog and one for the calculation of the number of otoliths that should be aged moving forward.

For the backlog:
<br> SH_BacklogOtoSummary.R summarizes the otoliths that have not yet been aged.
<br> SH_BacklogSurveySelection.R selects the survey otoliths from the backlog that should be aged.
<br> SH_BacklogPortSelection.R selects the port sample otoliths from the backlog that should be aged.
<br> SH_BacklogObsSelection.R selects the observer sample otoliths from the backlog that should be aged.

Looking forward:
<br> SH_OtoYearSelect.R was used to select a year to use for the CV calculation for otolith selection going forward.
<br> SH_DataInputs.R summarizes the data inputs that are used in the CV calculation for otolith selection going forward.
<br> SH_SurveyOtoSelect.R is the CV calculation used to determine the number of survey otoliths that should be aged going forward.
<br> SH_PortOtoSelect.R is the CV calculation used to determine the number of port sample otoliths that should be aged going forward.
<br> SH_ObsOtoSelect.R is the CV calculation used to determine the number of observer sample otoliths that should be aged going forward.

The number of otoliths that should be aged going forward was used as a reference for the backlog otolith selection.
