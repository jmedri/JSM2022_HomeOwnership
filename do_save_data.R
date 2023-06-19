source("initialize.R")
initialize()

save_rds(MODEL_DATA, file.path(INPUT_PROCESSED_DIR, "data_rds", "model_data.rds"))
save_rds(CENSUS_DATA, file.path(INPUT_PROCESSED_DIR, "data_rds", "census_data.rds"))
save_rds(STATE_SHAPE_DATA, file.path(INPUT_PROCESSED_DIR, "data_rds", "state_shape_data.rds"))
save_rds(COUNTY_SHAPE_DATA, file.path(INPUT_PROCESSED_DIR, "data_rds", "county_shape_data.rds"))
