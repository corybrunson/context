# create data frame
ejem <- data.frame(
  De = c('poor','good','excellent','excellent','excellent'),
  Dl = c('good','poor','excellent','excellent','excellent'),
  R = c('good','excellent','very poor','good','good'),
  S = c('understeering','understeering','oversteering',
        'neutral','understeering/neutral'),
  E = c('good','excellent','poor','very poor','good'),
  C = c('medium','very low','low','low','high'),
  M = c('excellent','good','very poor','very poor','poor'),
  row.names = c('Conventional','Front-Wheel','Rear-wheel',
                'Mid-engine','All-wheel'))

# include in data folder
devtools::use_data(ejem)
