## FIA states Dict
fia_states_dictionary <- FIESTA::ref_statecd

ffi_growth_habit <- iris

# use internal data
usethis::use_data(
  fia_states_dictionary,
  ffi_growth_habit,
  overwrite = TRUE, internal = TRUE
)
