@INCLUDE fxtr_common.nl@

!*************************************************************************************
! Fieldextra Cookbook
!-------------------------------------------------------------------------------------
! merge_fields
!   Conditionaly merge fields from different sources, considering also the land-sea
!   mask, and produce a GRIB 1 file suited for starting an assimilation cycle.
!*************************************************************************************


! INCORE storage
!======================================================================
! * The field HSURF is required to specify the model base grid when
!   working with fields defined on a staggered grid (like U and V).
! * The field FR_LAND is used to defined the land-sea mask (for later
!   conditional merge of surface temperature).
! * The COSMO surface temperature from a previous COSMO cycle is 
!   collected for later merge with the IFS sea surface temperature.
!----------------------------------------------------------------------
&Process
  in_file="@MODEL_PRE_WORKDIR@/icon_2022110823_+00005500.grb"
  out_type="INCORE"
/
&Process in_field = "HSURF", tag="base_grid" / 
&Process in_field = "FR_LAND", tag="land_sea_mask" /
&Process in_field = "T_SO", levlist=0, tag="cosmo_tso" /



! Product generation
!======================================================================
! * This product is part of a typical assimilation cycle at MeteoSwiss.
! * Three sources of information are merged to create a new GRIB 1
!   file suited to start the next assimilation cycle:
!     1) output from snow analysis (T_SNOW, W_SNOW, W_I, FRESHSNW, 
!        RHO_SNOW);
!     2) interpolated external parameters (VIO3, HMO3, PLCOV, LAI, 
!        ROOTDP, FOR_D, FOR_E) and interpolated IFS sea surface
!        temperature over sea points (T_SO), computed 6 hours earlier;
!     3) fields from previous assimilation cycle (all other fields,
!        in particular T_SO over land points).
! * The meta-information of all contributing fields is corrected to
!   be compatible with the start of the next assimilation cycle
!   (set_reference_date). This is justified by the fact that the
!   fields of source (2) are valid for the specified day, but the
!   hour of the day is not relevant.
!----------------------------------------------------------------------
!!! Collect fields produced by the snow analysis software
&Process
  in_file="./support/input/cosmo-7/laf2009083112_snowana"
  out_file="./support/results/merge_fields.gb1"
  out_type="GRIB1", in_size_field=950
/
&Process in_field = "T_SNOW"   / 
&Process in_field = "W_SNOW"   / 
&Process in_field = "W_I"      / 
&Process in_field = "FRESHSNW" / 
&Process in_field = "RHO_SNOW" / 

!!! Collect interpolated external parameters (all but T_SO)
!!! Collect fields coming from interpolated IFS sea surface temperature (T_SO,levlist=0)
!!! Merge IFS with COSMO surface temperature, using land sea mask to define the source (merge_with)
!!! Re-set generatingProcessIdentifier of surface temperature (set_auxiliary_metainfo)
!!! Re-set reference date (set_reference_date)
&Process
  in_file="./support/input/cosmo-7/laf2009083112_intpl"
  out_file="./support/results/merge_fields.gb1"
  out_type="GRIB1", in_size_field=950
/
&Process in_field = "VIO3", set_reference_date=200908311200 /
&Process in_field = "HMO3", set_reference_date=200908311200 / 
&Process in_field = "PLCOV", set_reference_date=200908311200 / 
&Process in_field = "LAI", set_reference_date=200908311200 / 
&Process in_field = "ROOTDP", set_reference_date=200908311200 / 
&Process in_field = "FOR_D", set_reference_date=200908311200 / 
&Process in_field = "FOR_E", set_reference_date=200908311200 / 
&Process in_field = "T_SO", levlist=0, 
                            merge_with="cosmo_tso", merge_mask="land_sea_mask>0.5", 
                            set_auxiliary_metainfo="generatingProcessIdentifier=5",
                            set_reference_date=200908311200  / 

!!! Collect remaining fields from previous COSMO run (EXCLUDE mode)
!!! Specify model base grid (in_regrid_target)
&Process
  in_file="./support/input/cosmo-7/laf2009083112_cosmo", 
  in_regrid_target="base_grid"
  out_file="./support/results/merge_fields.gb1"
  out_type="GRIB1", in_size_field=950
  selection_mode="EXCLUDE"
/
&Process in_field = "T_SNOW" /
&Process in_field = "W_SNOW" /
&Process in_field = "W_I" /
&Process in_field = "FRESHSNW" /
&Process in_field = "RHO_SNOW" /
&Process in_field = "VIO3"   /
&Process in_field = "HMO3"   /
&Process in_field = "PLCOV"  /
&Process in_field = "LAI"    /
&Process in_field = "ROOTDP" /
&Process in_field = "FOR_D"  /
&Process in_field = "FOR_E"  /
&Process in_field = "T_SO", levlist = 0 /



!======================================================================
! Remarks
!======================================================================
! * By default in_field defines fields to collect from the input file;
!   this behaviour can be changed by setting selection_mode to one of
!   "EXCLUDE" or "INCLUDE_ALL". When "EXCLUDE" mode is chosen, as
!   in the processing of laf2009083112_cosmo above, all fields are
!   collected _except_ the ones specified by in_field.
! * In this case, a mix of fields defined on the model base grid and
!   on staggered grids are collected, and it is required to specify
!   the model base grid (in_regrid_target). No re-gridding is applied,
!   because in_regrid_method is not set (the default in_regrid_method
!   is the identity transformation), but fieldextra is able to track
!   staggered fields.
! * In this example, the validation time of the fields contained in
!   laf2009083112_intpl are not the same as in the other data sources
!   (they are valid six hours earlier due to the timing of the IFS
!   analysis). To produce an output with uniform validation time, the
!   reference time of the fields collected from laf2009083112_intpl
!   is re-set (set_reference_date).
! * Other meta-information values can also be reset, as examplified
!   here by the set_auxiliary_metainfo operator (see README.user for
!   a full list).
! * Besides collecting fields from different sources, this example
!   illustrates how to create a new field by merging two fields
!   sharing the same identity (here T_SO, level=0), under the control
!   of a condition defined by some other fields (here FR_LAND - see
!   merge_with and merge_mask).
! * The names defined in merge_with and in merge_mask refers to the
!   tags of INCORE fields (unlike many other situations where 
!   operator arguments refer to tags of fields available in the
!   current processing iteration).
! * Another way of patching fields together, this time using a 
!   spatial condition, is presented in patch_fields.nl.
!----------------------------------------------------------------------




