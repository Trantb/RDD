# RDD
# Because of privacy issues, I cannot upload the data on GitHub.
# However, if you are interested, please contact me and I will direct you
# to my professor email.

# Imagine that two towns having identical circumstances such as population, income, etc, 
# but one town passes a tax at 51% while the other does not pass it at 49%. If five years later, the town passed
# the taxes have higher income than its counterpart. It should suggest that the new tax is the causes of
# increasing income right?

# The idea of this project is quite simple. Does a passed tax have any impact on 
# building new houses, increasing personal income, etc? If so, how long will the effect take place?
# There are many types of taxes such as recreation, road, current expense,...

# This project uses Regression Discontinuity Design (RDD) to answer the questions above.
# The dependent variables are things we mentioned above such as income, houses, etc.
# The running variable is the percentage of people vote to pass the taxes. 
# The covariances are controls such as percentage of labor participation, having bachelor degree, etc.

# One of the challenging in this project is the difference in types of bandwidth. At the beginning,
# we were not sure what type of bandwidth we should use, because they often give different results.
# Also, with roughly 400 dependent variables, 10 bandwidth and 2 options of lower and higher bandwidth, 
# it will take a very long time to go through all of them.
# Therefore, I come up with a code to automate the process and save some time at the initial stage of
# the project.

