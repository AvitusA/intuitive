FROM rocker/shiny:4.1.0
RUN install2.r \
    devtools \
    dplyr \
    magrittr \
    purrr \
    rlang \
    tidyr \
    tibble

COPY apps/* /srv/shiny-server/
RUN Rscript -e "devtools::install('/srv/shiny-server/ablab_setup/ablabsetup')"

# sudo rm /srv/shiny-server/index.html
# sudo rm -rf /srv/shiny-server/sample-apps
USER shiny

CMD ["/usr/bin/shiny-server"]