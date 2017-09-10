# secretsanta
Scripts to run a secret santa with your friends

How it works:

1. Create a csv file with your friends' email addresses, names, and any past picks (e.g. "amigos.csv")
2. Run `get_picks.R` It has a function to pick secret santas based on past picks ("sortingHat()") and a function to create a postcard for each santa informing them of their pick ("littleHelpers()"). The postcards will look like "pigpen.pdf". 
3. The postcards are now in your directory. Each one will be named for the santa and not the pick. Don't open them! To send each santa's card, run `sendRavens.py` It imports a function to send an email with an attachment to a list of people ("send_ravens()"). Note: requires a gmail address since the function is coded to connect to the gmail server.
