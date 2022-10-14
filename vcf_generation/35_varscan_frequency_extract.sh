#!/usr/bin/env bash


bcftools query -f '%POS\t%FORMAT/FREQ\n' $1
