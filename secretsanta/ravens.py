import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
from email.MIMEBase import MIMEBase
from email import encoders
# to find files
import os.path

def send_ravens(x,email,password):
    ""
    Function to send emails with unique attachments to multiple addresses
    Make sure to enter your email and password as strings
    ""
    # from and to
    from_address = str(email)
    to_address = x[0]
    msg = MIMEMultipart()
    msg['From'] = from_address
    msg['To'] = to_address
    msg['Subject'] = "Your secret santa assignment!" 
    
    # email body 
    first_name = x[1].rsplit(" ")[1] # extract first name
    text1 = " ".join(["Hi",first_name])+","
    text2 = """Your secret santa assignment is enclosed. 
                Burn after reading.
                -Santa
            """
    body = "\n\n".join([text1,text2])
    
    # attachment
    msg.attach(MIMEText(body, 'plain'))
    ## create the filename
    ### extract the email handle
    to_handle = x[0].rsplit("@")[0]
    santa_card_handle = "CARD.pdf"
    #ps = "ps1_GRADED.pdf"
    filename = "_".join([to_handle, santa_card])
    ## input the directory
    ## put them together to get the file path
    directory_filename = "/".join([directory, filename])

    # upload attachment
    attachment = open(directory_filename, "rb")
    part = MIMEBase('application', 'octet-stream')
    part.set_payload((attachment).read())
    encoders.encode_base64(part)
    part.add_header('Content-Disposition', "attachment; filename= %s" % filename)
    msg.attach(part)
    # connect to server and send 
    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(from_address, str(password))
    text = msg.as_string()
    server.sendmail(from_address, to_address, text)
    server.quit() 
