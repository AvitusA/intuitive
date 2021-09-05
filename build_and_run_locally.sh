#!/bin/bash
docker build -t intuitive .
docker run -p 3838:3838 -it intuitive
