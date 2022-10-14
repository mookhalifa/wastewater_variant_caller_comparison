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
output_name_dedup=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_gatk_dedup.bam
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_gatk.vcf

picard AddOrReplaceReadGroups I=${b} O=$output_name_dedup RGID=4 RGLB=lib1 RGPL=ILLUMINA RGPU=unit1 RGSM=20

gatk --java-options "-Xmx4g" MarkDuplicatesSpark -I $output_name_dedup -O $output_name_dedup.bam
gatk --java-options "-Xmx4g" HaplotypeCaller -R ${r} -I $output_name_dedup.bam -O $output_name_vf

# rm $output_name_dedup $output_name_dedup.bam
