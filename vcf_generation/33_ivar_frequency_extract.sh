#!/usr/bin/env bash


bcftools query -f '%POS\t%FORMAT/ALT_FREQ\n' $1
