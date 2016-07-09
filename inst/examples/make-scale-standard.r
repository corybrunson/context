titanic_data <- as.data.frame(Titanic)

# nominal scale
print(make_scale_nominal(objs = titanic_data$Class))

# ordinal scale
print(make_scale_ordinal(objs = titanic_data$Class))

# other scales
for (scale in c("boolean", "revordinal", "interordinal")) {
  print(make_scale_standard(objs = titanic_data$Class, scale = scale))
}

# biordinal scale with custom cut value
print(make_scale_biordinal(objs = titanic_data$Class, cut = 3))
