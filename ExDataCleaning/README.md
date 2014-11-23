Running script: In R, set your working directory to the folder named "UCI HAR Dataset". Run the R script run_analysis.R. 

1. Script will read in each relevant data file via read.table().

2. Data files will be recombined into single dataset, excluding variables that do not contain "mean()" or "std()" in the original names.

3. Activity numbers will be changed to character strings reflecting actual activities.

4. Functional variable names will be assigned from the features.txt file.

5. Data will be analysed, calculating the mean of each variable by activity and subject via group_by() and summarise() (dplyr functions).

6. Readable variable names, explained in codebook.md in this directory, will be constructed via gsub().

7. Data will be printed to a .txt file via write.table().