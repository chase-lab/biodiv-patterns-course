# First, we need to install swirl to run tutorials interactively.
install.packages('swirl')

# Then, we load the swirl library
library('swirl')

# Eventually, we install our course materials about data analysis by
install_from_swirl("Exploratory Data Analysis")

# We are set! Let's start our tutorial
swirl()

# Exit swirl by typing
## bye()