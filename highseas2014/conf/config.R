# layers ----
layers_id_fields = c('rgn_id','cntry_key','saup_id','fao_id','fao_saup_id')
layer_region_labels = 'rgn_global'
layer_region_areas  = 'rgn_area'

# pressures & resilience matrices ----
# components describe the layer and level with which to aggregate resilience and pressures matrices for goals with categories
resilience_components = list('NP'  = c('layer'='rnk_np_weights_combo'       , 'level'='region_id-category'),  # old: rnk_np_product_weight
                             'CS'  = c('layer'='rnk_cs_habitat_extent'      , 'level'='region_id'),
                             'CP'  = c('layer'='rnk_cp_habitat_extent_rank' , 'level'='region_id'),           # old: rnk_cp_habitat_extent
                             'HAB' = c('layer'='rnk_hab_presence'           , 'level'='region_id'))
pressures_components  = list('NP'  = c('layer'='rnk_np_weights_combo'       , 'level'='region_id-category'),
                             'CS'  = c('layer'='rnk_cs_habitat_extent'      , 'level'='region_id'),
                             'CP'  = c('layer'='rnk_cp_habitat_extent_rank' , 'level'='region_id'),
                             'LIV' = c('layer'='rnk_le_sector_weight'       , 'level'='region_id'),
                             'ECO' = c('layer'='rnk_le_sector_weight'       , 'level'='region_id'),
                             'HAB' = c('layer'='rnk_hab_presence'           , 'level'='region_id'))
pressures_categories = list(environmental=c('po','hd','fp','sp','cc'), social='ss')
resilience_categories = c('environmental', 'regulatory', 'social')

# constants
pressures_gamma = 0.5
goal_discount = 1.0
goal_beta = 0.67
default_trend = 0

# extra descriptions not covered by goals.description or layers.description, used in ohigui
index_description = 'The overall Index represents the weighted average of all goal scores.'
dimension_descriptions = c('score' = 'This dimension is an average of the current status and likely future.',
                           'status' = 'This dimension represents the current value of a goal or sub-goal relative to its reference point.',
                           'future' = 'For this dimension, the likely future is calculated as the projected status in 5 years, informed by the current status, continued trend, inflected upwards by resilience and downwards by pressures.',
                           'trend' = 'This dimension represents the recent change in the value of the status. Unlike all other dimensions which range in value from 0 to 100, the trend ranges from -1 to 1, representing the steepest declines to increases respectively.',
                           'pressures' = 'This dimension represents the anthropogenic stressors that negatively affect the ability of a goal to be delivered to people. Pressures can affect either ecological or social (i.e. human) systems.',
                           'resilience' = 'This dimension represents the social, institutional, and ecological factors that positively affect the ability of a goal to be delivered to people.')
