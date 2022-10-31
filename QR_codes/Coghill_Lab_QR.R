## Script to generate QR codes for the Coghill Lab GitHub page

library(qrcode)

cl_code <- qr_code("https://github.com/Coghill-Lab")

plot(cl_code) ## export to "QR_codes/Coghill_Lab_QR.png"

generate_svg(cl_code, here::here("QR_codes/GitHub_Coghill-Lab_QR_code.svg"))
