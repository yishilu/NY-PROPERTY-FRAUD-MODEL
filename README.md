NY-PROPERTY-FRAUD-MODEL
Steps:
1. Perform data quality analysis to understand % of populated, value of 0, median and etc of 32 fields.
2. Select the most important 9 fields, including ZIP, BLDDEPTH, BLDFRONT, LTDEPTH, LTFRONT, FULLVAL, AVLAND, AVTOT to fill in the missing values followed by some logics.
3. Create 45 new variables with the above 9 fields.
4. Z-scaling and principal component analysis to reduce dimensionality. Selected 8 PCs and z-scaled again for model application.
5. Build heuristic algorithm and autoencoder to generate two scores
6. Quantile binning to combine the above two scores to produce the final score for each property.
