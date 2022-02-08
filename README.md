# misc-data

### Dataset Mapping

Main code: DatasetMapping.m 
Dependent functions: fixshoename.m, getShoeNames.m, grabVariables.m, and grabfilename.m

This code was developed to join the Excel file that holds the perception and mechanical properties data with the set of MATLAB files that contain biomechanical data calculated in and exported from Visual3D. This code is broken up into 3 sections so that any section can be re-run without having to run the whole thing again (especially section 2).  While the code works, it is not very pretty and needs some cleaning.

Descriptions of the sections are below.

Section 1:  Locates the Excel file with the mechancial and perception data and pulls it into MATLAB.  From there, columns of interest are selected.  New columns are also created for subject initials initials, prototype shoe name (test shoe) + generation, and season + year.  Each of these columns is created from two additional columns in the Excel file. All of the columns of interest are then joined into a new table.

Section 2: This section pulls the biomechanical data (from Visual3D) into MATLAB and pulls out the name of the shoe of interest, subject's initials, prototype shoe (name, generation, season), and all variables of interest from the data. 

NOTE:  The naming of shoes in the file names was highly irregular and full of typos, so this section of the data goes through all of the naming errors and corrects them.  This is needed to match up this data with the Excel Data.  A few extra lines are included in comments that aided in figuring out the correct versions of the errors.  

Since each season of testing has its own folder, it loops through each folder before moving onto the next season's folder.  As a result, a structure is created that contains 4 tables - one for each season that contains shoe/subject information and biomechanical variables.

Section 3: This section creates a combined table from all seasons.  The code then joins this table with the perception/mechanical data table by matching up rows that contain the same: name of shoe of interest, subject's initials, prototype shoe name and generation, and test season.


### GPS Watch Data Analysis

Code file #1: WatchDataAnalysisCode.ipynb

This code used Python and Jupyter Notebooks to merge several datasets and perform some exploratory data analysis, using histograms and simple linear regression.  Code files is still in progress and needs cleaning.

Code file #2: WatchData_LinearMixedModel.R

This code was built in R to test if time of day for a run influenced specific metrics of a run.  Code builds several linear mixed models to answer the question, with various co-factors (e.g. average velocity of run, duration of run, type of runner).

Code file #3: WatchData_LDA_PCA.R

This code was built in R to see if types of runners (specified from questions in a survey) can be identified from running metrics.  This code also contains a portion to see if sub-groups can be distinguied within a given type of runner.
