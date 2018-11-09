## Some notes about Livelihoods and Economies subgoal calculations and data

Previously these functions and layers were incorporated into ohi-global, but because
the data have not been updated since 2013, we have decided to simplify.  These items
are preserved for reference.

Livelihoods is comprised of job and wage data for a series of marine sectors.
Economies is comprised of revenue data for a series of marine sectors.

There are 4 basic datasets used in the status calculation for job, wage, and revenue:
  le_xxxx_cur_adj_value
le_xxxx_ref_adj_value
le_xxxx_cur_base_value
le_xxxx_ref_base_value

where, "xxxx" is "jobs", "rev", "wage".

Status is calculated as:
  (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)

I would have to look through the notes again to see how the "base" values are corrected to obtain the "adj" values in each case. For "wages" the adjustment is made in functions.R using le_gdp_pc_ppp to multiply the wages for each country/year to make values across countries comparable.

The weights for each sector are based on: le_workforcesize_adj (livelihoods) and le_revenue_adj (economies).

To calculate trend, the following data are used:
  le_wage_sector_year (livelihoods: wage trend)
le_jobs_sector_year (livelihoods: job trend)
le_rev_sector_year (economies trend)

Adjustments were made to the above trend data using these data:
  le_unemployment (livelihoods: jobs)
le_gdp (economies)

Adjustments were made by dividing the trend data by the adjusted values:
  base_value / adj_value

the le_popn is used to adjust status/trend scores when dealing with regions with no humans.

le_sector_weight is used to weight pressure/resilience data.