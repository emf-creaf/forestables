## FIA states Dict
fia_states_dictionary <- FIESTA::ref_statecd



# use internal data
usethis::use_data(
  fia_states_dictionary,
  overwrite = TRUE, internal = TRUE
)
