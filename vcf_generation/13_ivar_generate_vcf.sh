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
output_name_tsv=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_ivar.tsv
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_ivar.vcf

#Run samtools mpileup and pipe to ivar
samtools mpileup -aa -A -d 1000000 -B -Q 0 ${b} | ivar variants -p $output_name_tsv -q 20 -t 0.03 -r ${r}
#iVar generates tsv file. This python script converts tsv to vcf format
python3 ivar_tsv_to_vcf.py $output_name_tsv $output_name_vcf
#python3 ivar_tsv_to_vcf.py $ sample1-ivar.tsv $ sample1-ivar.vcf

# rm $output_name_tsv
