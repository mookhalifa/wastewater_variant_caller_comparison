#! /usr/bin/env bash


usage() { echo "Usage: $0 [-r reference fasta] [-b input bam file]" 1>&2; exit 1; }

while getopts "h:r:b:" o; do
    case "${o}" in
        r)
            r=${OPTARG}
            ;;
        b)
            b=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${r}" ] || [ -z "${b}" ]; then
    usage
fi

echo "r = ${r}"
echo "b = ${b}"


#echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n\3\n\1/'  # sed '\.\(.*\)$'
#Define output file names
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_freebayes.vcf

#Run freebayes and output vcf file
freebayes -f ${r} ${b} > $output_name_vcf
#filter the vcf file
freebayes -f ${r} --min-coverage 8 -F 0.01 -q 15 -P 0.05  ${b} > $output_name_vcf

# rm $output_name_bcf
