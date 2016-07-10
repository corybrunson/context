# logical expressions involving single variables
scale_by_predicate(mtcars, cyl == 8)
scale_by_predicate(mtcars, mpg > 20)

# logical expressions involving multiple variables
scale_by_predicate(mtcars, cyl > 4 & wt < 3)

# multiple logical expressions
scale_by_predicate(mtcars, list(cyl > 4, wt < 3, mpg > 20))

# singleton list
scale_by_predicate(mtcars, list(cyl == 6))
