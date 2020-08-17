# PG-400 Scripts for Data Sampling, Preprocessing and Model runs
 
The codes can be split into four camps of Sample Creation (samplecreator...), dataprocessing, the model creation and runs (kfoldcnnredo...) and Misc.

Many of the files are simply copies of the three core scripts with variations to fit the different datasets (alpha, beta, theta, delta etc.) and as such the main files with commenting are "SampleCreatorAll.R", "DataProcessing.R" and "kfoldcnnReDo.R". In these files, only the first ~100 lines are correctly commented as beyond this, they are typically repeated lines of codes with variations to fit the various folds etc.

Whilst not the most efficient code (which should have used loops and lists) it works well for an 8GB memory machine with limited storage as it allows for more flexibility in which scripts are run and how much capacity they take up.
