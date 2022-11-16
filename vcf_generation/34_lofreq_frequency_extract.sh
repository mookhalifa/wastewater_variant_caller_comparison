#!/usr/bin/env bash


bcftools query -f '%POS\t%INFO/AF\n' $1
