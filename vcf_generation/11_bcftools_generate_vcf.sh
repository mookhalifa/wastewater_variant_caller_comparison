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
output_name_bcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_bcftools.bcf
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_bcftools.vcf

#Run bcftools and output a bcf file
bcftools mpileup -Ou -f ${r} ${b} | bcftools call -mv -Ob -o $output_name_bcf
# Convert the bcf file to vcf
bcftools convert -O v -o $output_name_vcf $output_name_bcf

# rm $output_name_bcf
