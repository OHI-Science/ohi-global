If there is a change of score do the following to update all the figures/documents:

1. Run the first_look.Rmd
   - on ~ line 98 change: `update_data = TRUE`
   - on ~ line 81 change the dateFile to today's date: `dateFile = '2017-11-20'`
   - change link to correct date: [Data](https://raw.githubusercontent.com/OHI-Science/ohi-global/draft/global2017/OHI_final_formatted_scores_2017-11-15.csv) are the OHI scores for the eez regions of 220 countries and territories from 2012 to 2017.
   - read through to correct references to scores/regions
2. Generate Croscon data: ohi-global/global2017/croscon_scores_prepare.R, and send newly generated Croscon data to Croscon
3. Run ohi-global/global2017/Results/Results.Rmd, change `saveDate = '2017-11-20'` to date that corresponds to latest dataset
4. Run ohi-global/global2017/Results/trend_barplot fig4.R
5. Run ohi-global/supplement_Results.Rmd, change the 'radicalFile = '2017-11-20'' (~line 92) to correct date 