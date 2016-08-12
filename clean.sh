#!/bin/sh
rm -fr dist && rm -fr .stack-work && stack clean && stack build
