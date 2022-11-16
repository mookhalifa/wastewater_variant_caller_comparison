#! /usr/bin/env bash


usage() { echo "Usage: $0 [-r reference fasta] [-r2 second reference to contrast] [-v vcf file list, seperated by commas: (file1.vcf,file2.vcf,file3.vcf,file4.vcf,file5.vcf)] [-o output diretcory name] [-t CPU threads defaults to 20]" 1>&2; exit 1; }

while getopts "h:r:b:t:r2:" o; do
    case "${o}" in
        r)
            r=${OPTARG}
            ;;
	r2)
	    r2=${OPTARG}
	    ;;
        v)
            v=${OPTARG}
            ;;
        o)
           o=${OPTARG}
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

if [ -z "${t}" ]; then $(set 20); fi
if [ -z "${o}" ] || [ -z "${r}" ] || [ -z "${r2}" ] || [ -z "${v}" ]; then
    usage
fi

echo "r = ${r}"
echo "b = ${b}"
echo "o = ${o}"

#echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n\3\n\1/'  # sed '\.\(.*\)$'
#Define output file names
#output_name_tsv=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_lofreq.tsv
#output_name_vcf=$(echo $b | sed 's/\(.*\)\/\(.*\)\.\(.*\)$/\2\n/')_lofreq.vcf

python3 run_benchmark.py vareval --novenn -t ${t} -c ~/miniconda3/envs -v ${v} -r ${r},${r2} -o ${o}

