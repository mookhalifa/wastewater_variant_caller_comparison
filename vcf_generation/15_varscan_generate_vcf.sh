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
output_name_pile_up=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_varscan.pileup
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_varscan.vcf
output_name_base=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_varscan

samtools mpileup -f ${r} -q 10 -d 1000000 ${b} > $output_name_pile_up
cat $output_name_pile_up | awk 'BEGIN {FS="\t";OFS="\t";print "#chr","position","refbase","coverage"} {print $1,$2,$3,$4}' > $output_name_base.cov.tsv
varscan pileup2snp $output_name_pile_up --p-value 0.05 > $output_name_base.tsv
sequenza-utils pileup2acgt -p $output_name_pile_up > $output_name_base.acgt.tsv
varscan pileup2indel $output_name_pile_up --p-value 0.05 > $output_name_base.indel.tsv
varscan mpileup2snp $output_name_pile_up --p-value 0.05 --output-vcf 1 > $output_name_base-snps.vcf
varscan mpileup2indel $output_name_pile_up --p-value 0.05 --output-vcf 1 > $output_name_base-indel.vcf
vcfcat $output_name_base-snps.vcf $output_name_base-indel.vcf > $output_name_vcf
bgzip $output_name_base-snps.vcf
bgzip $output_name_base-indel.vcf
bcftools index $output_name_base-snps.vcf.gz
bcftools index $output_name_base-indel.vcf.gz
bcftools concat $output_name_base-snps.vcf.gz $output_name_base-indel.vcf.gz -o $output_name_base-merged.vcf


# rm $output_name_tsv
