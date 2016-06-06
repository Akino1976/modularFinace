#!/usr/bin/env bash
###################################################################################
#: Title: presentValue.bash
#: Synopsis: presentValue
scriptname=${0##*/}
description="Present value of companie"
usage="Usage: ${0##*/} script, options [-(c) company] "
date_of_creation=2015-11
version=1.0
author="Serdar Akin"
#: In the script the -G will be set by multiplying with -f
#: presentValue.bash -c Novus  -G 333 -a 12.4 -f 0.82 -m 24
# USAGE: presentValue.bash -c Novus  -G 444 -a 18.4 -f 0.82
#: G = The initial investment fee, will be a percentage of -f
#: c = Company brand, will be outputed into the file
#: a = Monthly payments
#: f = The agrees percentage
#: m = The matuarity time
###################################################################################

# Lock the current dir
_HOME=$PWD
# Placement of the log file
_INFO="${_HOME}/INFO"

#------------------------------------------------------------
# Function needed to enable the analysis
#------------------------------------------------------------
GREEN="\033[0;32m"
NO_COLOUR="\033[0m"
RED="\033[1;31m"
BLUE="\033[0;34m"
LIGHT_BLUE="\033[1;34m"
PURPLE="\033[1;35m"
BLACK="\033[1;30m"
RR="/usr/bin/Rscript --verbose"
dirtFiles="*.aux *.fls *.log *.out *.toc *.*_*"

_repeat() { #@ USAGE: _repeat string number
    _REPEAT=$1
    while [ ${#_REPEAT} -lt $2 ]
    ## Loop until string exceeds desired length
    do
        _REPEAT=$_REPEAT$_REPEAT$_REPEAT
    ## 3 seems to be the optimum number
    done
    _REPEAT=${_REPEAT:0:$2}
    ## Trim to desired length
}

alert() #@ USAGE: alert message border
{
    _repeat "${2:-#}" $(( ${#1} + 8 ))
    printf '\a%s\n' "$_REPEAT" ## \a = BEL
    printf '%2.2s  %s  %2.2s\n' "$_REPEAT" "$1" "$_REPEAT"
    printf '%s\n' "$_REPEAT"
}
#------------------------------------------------------------
# Parse command line options/arguments
#------------------------------------------------------------
if (! getopts "hvc:" name); then
    echo "Usage: $usage "
exit $E_OPTERROR
fi

while getopts hvc: option
do
    case ${option} in
    h) echo $usage;
    exit;;
    v) echo $version;
    exit ;;
    c) script="$OPTARG" ## Returns the argumen -o "Serdar"
    ;;
    \?) Syntax;
    alert "${scriptname}: usage: [-m (run Test) ] | [-u (run survey)]"
    exit 2
    ;;
    esac
done
shift "$(( OPTIND - 1 ))"

# Check if both condition exist # -o is if either is empty then stop the script
if [ -z "$company" ] ;
    then
    echo "============================================================="
    printf "$GREEN## Will stop the script, must give a value for -c: [%s] ##${BLACK}\n" "${company}"
    echo "============================================================="
    exit 0
fi

echo "============================================================="
printf "$GREEN## Will run company: $RED[%s] ${BLACK}##\n" "${company}"
echo "============================================================="
sleep 2s


$RR "${script}"

[[ -d "${company}" ]] || mkdir "${company}"

R CMD Sweave --encoding=utf8 finance.Rnw
latexmk -g -f presentValue.tex
mv finance.pdf "${company}/${company}.pdf"
rm -f $dirtFiles
