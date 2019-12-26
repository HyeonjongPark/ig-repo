
colnames(train)
str(train)
nrow(train)

train_sample = train[sample(1:nrow(train), (nrow(train))/100 ) , ]

nrow(train_sample)
train_sample
train_sample$event_datetime = as.Date(train_sample$event_datetime)

unique(train_sample$device_city)
unique(train_sample$device_region)
unique(train_sample$device_country)
unique(train_sample$device_language)

str(train_sample)

plot_str(train_sample)
plot_missing(train_sample)
plot_histogram (train_sample)
plot_density (train_sample)
plot_correlation (train_sample, type = 'continuous', 'Review.Date')
plot_bar(train_sample)

