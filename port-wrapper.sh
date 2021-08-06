#!/bin/bash
if [[ -n $PORT ]]; then
    sed -i -e 's/listen [0-9]\{4\}\;/listen '"$PORT"';/g' /etc/shiny-server/shiny-server.conf
fi
sudo --user=shiny /usr/bin/shiny-server
