FROM rocker/shiny-verse:4.1.0
COPY ./port-wrapper.sh /
RUN install2.r \
  HDInterval \
  ggrepel

RUN rm -r /srv/shiny-server
COPY ./apps /srv/shiny-server

RUN Rscript -e "devtools::install('/srv/shiny-server/ablab_setup/ablabsetup')"

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

CMD ["/bin/bash", "/port-wrapper.sh"]