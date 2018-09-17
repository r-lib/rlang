#!/usr/bin/env sh

CODE="if (getRversion() < '3.3') stop('No macro support')"

"${R_HOME}/bin/R" -e "${CODE}" > /dev/null
