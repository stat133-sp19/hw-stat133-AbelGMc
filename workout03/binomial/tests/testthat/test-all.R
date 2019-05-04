expect_error(check_prob(10), 'invalid prob value')
expect_true(check_prob(0.2))
expect_error(check_prob(c(1,2)), 'invalid prob value')

expect_error(check_trials(1.3), 'invalid trials value')
expect_true(check_trials(3))
expect_error(check_trials(c(1,2)), 'invalid trials value')
expect_error(check_trials(-213), 'invalid trials value')

expect_error(check_success(c(-1,2,3,4),5), 'invalid success value')
expect_true(check_success(c(1,2,3,4,5),5))
expect_error(check_success(21,5), 'invalid success value')

expect_equal(aux_mean(5,0.2), 1)
expect_equal(aux_mean(5,0.3), 1.5)
expect_equal(aux_mean(11,1),11)

expect_equal(aux_variance(4,0.5), 1)
expect_equal(aux_variance(8,0.5), 2)
expect_equal(aux_variance(12,0.5),3)

expect_equal(aux_mode(4,0.5), 2)
expect_equal(aux_mode(5,0.5), c(3,2))
expect_equal(aux_mode(5,0.2),1)

expect_equal(aux_skewness(4,0.5), 0)
expect_equal(aux_skewness(5,0.5), 0)
expect_equal(aux_skewness(5,0),Inf)

expect_equal(aux_kurtosis(4,0.5),-0.5)
expect_equal(aux_kurtosis(5,0.5),-0.4)
expect_equal(aux_kurtosis(5,0.2),0.05)

expect_equal(bin_choose(5,1),5)
expect_equal(bin_choose(5,2),10)
expect_equal(bin_choose(5,c(3,4)),c(10,5))
expect_error(bin_choose(5,6),'k cannot be greater than n')

expect_equal(bin_probability(2,5,0.5),0.3125)
expect_equal(bin_probability(2,3,0.5),0.375)
expect_equal(bin_probability(2,2,0.5),0.25)
expect_error(bin_probability(5,2,0.5),'invalid success value')
expect_error(bin_probability(5,2,-0.5),'invalid prob value')
expect_error(bin_probability(2,-2,-0.5),'invalid trials value')

expect_equal(bin_distribution(5,1)$probability,c(0,0,0,0,0,1))
expect_equal(bin_distribution(5,1)$success,0:5)
expect_error(bin_distribution(5,1.2),'invalid prob value')
expect_error(bin_distribution(-1,1.2),'invalid trials value')

expect_equal(bin_cumulative(5,1)$probability,c(0,0,0,0,0,1))
expect_equal(bin_cumulative(5,1)$success,0:5)
expect_equal(bin_cumulative(5,1)$cumulative,c(0,0,0,0,0,1))
expect_error(bin_cumulative(5,1.2),'invalid prob value')
expect_error(bin_cumulative(-1,1.2),'invalid trials value')
