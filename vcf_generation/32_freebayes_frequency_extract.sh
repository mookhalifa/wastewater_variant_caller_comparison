#!/usr/bin/env bash


bcftools query -f '%POS\t%INFO/DP\t%INFO/DPB\t%INFO/SRF\t%INFO/SRR\t%INFO/SAF\t%INFO/SAR\n' $1
