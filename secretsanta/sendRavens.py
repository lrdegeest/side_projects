import pandas as pd
from ravens import send_ravens

# set directory variable for the function
directory = "[folder where you store the cards]"

# import your amigos
# first column should be email addresses, second column should be names
addresses_names = pd.read_csv("amigos.csv")

#send ravens
addresses_names.apply(send_ravens, axis = 1);
