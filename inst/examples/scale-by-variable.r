titanic_data <- as.data.frame(Titanic)

# custom scale for Class variable
class_scale <- rbind(
  c(1, 1, 1, 0),
  c(0, 1, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 1, 1)
)
rownames(class_scale) <- levels(titanic_data$Class)
colnames(class_scale) <- paste0(">", levels(titanic_data$Class))
print(class_scale)
scale_by_variable(data = titanic_data, var = "Class", scale = class_scale)

# nominal scale for Sex variable
scale_by_variable(data = titanic_data, var = "Sex", scale = "nominal")

# ordinal scale for Age variable
scale_by_variable(data = titanic_data, var = "Age", scale = "ordinal")

# multiple scales applied at once
both_vars <- c("Class", "Sex")
sex_scale <- make_scale_standard(objs = titanic_data$Sex, scale = "nominal")
both_scales <- list(class_scale, sex_scale)
scale_by_variables(data = titanic_data, vars = both_vars, scales = both_scales)
