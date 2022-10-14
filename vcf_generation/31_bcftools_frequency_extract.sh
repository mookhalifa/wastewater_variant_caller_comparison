#!/usr/bin/env bash


bcftools query -f '%POS\t%INFO/DP\t%INFO/DP4\n' $1
