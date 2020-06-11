#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server
chmod -R 777 /var/log/shiny-server

exec shiny-server >> /var/log/shiny-server/shiny-server.log 2>&1

# https://aws-blog.de/2019/11/r-can-not-be-pushed-in-production-deprecated.html
# Whether on Fargate or not, write credentials to .Renviron
# env | grep AWS_ >> /home/shiny/.Renviron
# chown shiny.shiny /home/shiny/.Renviron
