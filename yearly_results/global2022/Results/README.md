If there is a change of score do the following to update all the figures/documents:

1. Run the ohi-global/globalxxxx/Results/first_look.Rmd
   - on ~ line 88 change: `update_data = TRUE`
   - on ~ line 84 change the dateFile to today's date: `dateFile = '2018-10-03'`
   - change link to correct date: [Data](https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/global2018/OHI_final_formatted_scores_2018-10-03.csv) are the OHI scores for the eez regions of 220 countries and territories from 2012 to current assessment year.
   - read through to correct references to scores/regions
2. Generate Croscon data: ohi-global/global2018/croscon_scores_prepare.R, and send newly generated Croscon data to Croscon, change dateFile variable to correct date.
3. Run ohi-global/global2018/Results/Results.Rmd, change `saveDate = '2018-10-10'` to date that corresponds to latest dataset
4. Run ohi-global/global2018/Results/trend_barplot fig4.R
5. Run ohi-global/global2018/Results/Supplement_Results.Rmd, change the 'radicalFile = '2018-10-10'' (~line 92) to correct date 
6. Clean up years of data that are no longer needed