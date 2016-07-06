titanic_data <- as.data.frame(Titanic)

# nominal scale
print(scale_nominal(objs = unique(titanic_data$Class)))

# ordinal scale
print(scale_ordinal(objs = unique(titanic_data$Class)))

# other scales
for (scale in c("boolean", "revordinal", "interordinal")) {
  print(scale_standard(x = titanic_data$Class, scale = scale))
}

# biordinal scale with custom cut value
print(scale_biordinal(objs = unique(titanic_data$Class), cut = 3))
