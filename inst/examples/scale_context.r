titanic_data <- as.data.frame(Titanic)

class_scale <- rbind(
  c(1, 1, 1, 0),
  c(0, 1, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 1, 1)
)
rownames(class_scale) <- levels(titanic_data$Class)
colnames(class_scale) <- paste0(">", levels(titanic_data$Class))

scale_context(data = titanic_data, var = "Class", scale = class_scale)

scale_context(data = titanic_data, var = "Sex", scale = "nominal")

scale_context(data = titanic_data, var = "Age", scale = "ordinal")

both_vars <- c("Class", "Sex")

sex_scale <- rbind(
  c(1, 0),
  c(0, 1)
)
rownames(sex_scale) <- levels(titanic_data$Sex)
colnames(sex_scale) <- c("Male", "Female")

both_scales <- list(class_scale, sex_scale)

scales_context(data = titanic_data, vars = both_vars, scales = both_scales)
