# scale all variables using nominal standard scales
data(ejem)
variables <- colnames(ejem)
ejem_scale <- scale_by_variables(ejem,
                                 vars = variables,
                                 scales = as.list(rep('nominal',
                                                      length(variables))))
rownames(ejem_scale) <- rownames(ejem)
print(ejem_scale)
