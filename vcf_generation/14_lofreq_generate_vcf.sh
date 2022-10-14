#! /usr/bin/env bash


usage() { echo "Usage: $0 [-r reference fasta] [-b input bam file] [-t threads (defaults to 10)]" 1>&2; exit 1; }

while getopts "h:r:b:t:" o; do
    case "${o}" in
        r)
            r=${OPTARG}
            ;;
        b)
            b=${OPTARG}
            ;;
        t)
           t=${OPTARG}
	    ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "${t}" ]; then $(set 10); fi
if [ -z "${r}" ] || [ -z "${b}" ]; then
    usage
fi

echo "r = ${r}"
echo "b = ${b}"
echo "t = $t{t}"

#echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n\3\n\1/'  # sed '\.\(.*\)$'
#Define output file names
output_name_tsv=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_lofreq.tsv
output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_lofreq.vcf


#add indel quality score into bam file
lofreq indelqual -f ${r} --dindel -o ${b}.tmp ${b}

lofreq call-parallel --pp-threads ${t} --call-indels -f ${r} -o $output_name_vcf ${b}.tmp

# rm ${b}.tmp
