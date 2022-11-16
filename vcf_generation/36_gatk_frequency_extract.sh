#!/usr/bin/env bash


bcftools query -f '%POS\t%FORMAT\n' $1
