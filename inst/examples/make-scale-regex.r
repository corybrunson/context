# state name modifiers
make_scale_regex(objs = state.name,
                 patterns = c("^New", "^(North|South|East|West)"))

# letters in state names
make_scale_regex(objs = state.abb, split = "")
