&RunSpecification
 strict_nl_parsing  = .true.
 n_ompthread_total  = 1
 verbosity          = "high"
 diagnostic_length  = 110
 strict_usage = .false.
/

&GlobalResource
 dictionary           = "@FXTR_HOME@/resources/dictionary_icon.txt",
 grib_definition_path = "@FXTR_HOME@/resources/eccodes_definitions_cosmo", "@FXTR_HOME@/resources/eccodes_definitions_vendor"
 grib2_sample         = "@FXTR_HOME@/resources/eccodes_samples/COSMO_GRIB2_default.tmpl"
/

&GlobalSettings
 default_model_name   = "icon"
 originating_center   = "roma"
/

&ModelSpecification
 model_name         = "icon"
 precip_all         = "RAIN_GSP", "SNOW_GSP"
 precip_snow        = "SNOW_GSP"
 precip_rain        = "RAIN_GSP"
 earth_axis_large   = 6371229.
 earth_axis_small   = 6371229.
/
