#!/bin/bash
 
#PBS -N ICCB_extract_monthly
#PBS -A qccceclim
#PBS -p 10
#PBS -j oe
#PBS -l walltime=24:00:00
#PBS -l mem=10gb
#PBS -q ccamamdq

# Load Modules
module purge
module load modpathQCCCE
module load QCEstandard

# Activate virtual environment 
. /export/home/qclim/env-3.6.5/bin/activate

#cd ${PBS_O_WORKDIR}

# Arguments ------------------------------------------------------------------------------------ #

models=("ACCESS-CM2" "EC-Earth3" "GFDL-ESM4")
variants=("r2i1p1f1" "r1i1p1f1" "r1i1p1f1")
experiments=("CCAM10oc" "CCAM10" "CCAM10")

scen="ssp370"

outdir="/export/home/qclim/repositories/projects/collaborations/workshops/ICCB/data/monthly/"
base_dir="/scratch/qclimdata/CCAM/CORDEX-CMIP6/AUS-10i/post-processed/"

if [ ! -d ${outdir} ]; then
	 mkdir -p ${outdir}
fi

for m in {2..2}; do
	
    model=${models[${m}]}
    variant=${variants[${m}]}
    experiment=${experiments[${m}]}
    
 
    tmax_file="${base_dir}${model}_${experiment}/${scen}/${variant}/mon/tasmax_${model}_*.nc"
    tmin_file="${base_dir}${model}_${experiment}/${scen}/${variant}/mon/tasmin_${model}_*.nc"
    pr_file="${base_dir}${model}_${experiment}/${scen}/${variant}/mon/pr_${model}_*.nc"
    
    tmax_out="${outdir}tasmax_${model}_${scen}_${variant}_${experiment}_aus-10i_10km_mon_1981-2100.nc"
    tmin_out="${outdir}tasmin_${model}_${scen}_${variant}_${experiment}_aus-10i_10km_mon_1981-2100.nc"
    pr_out="${outdir}pr_${model}_${scen}_${variant}_${experiment}_aus-10i_10km_mon_1981-2100.nc"
    
    cdo -setattribute,tasmax@units=degC -addc,-273.15 -select,name='tasmax' -selyear,1981/2100 -sellonlatbox,151,155,-26,-29.5 ${tmax_file} ${tmax_out}
    cdo -setattribute,tasmin@units=degC -addc,-273.15 -select,name='tasmin' -selyear,1981/2100 -sellonlatbox,151,155,-26,-29.5 ${tmin_file} ${tmin_out}
    cdo -setattribute,pr@units=mm -mulc,86400 -select,name='pr' -selyear,1981/2100 -sellonlatbox,151,155,-26,-29.5 ${pr_file} ${pr_out}
    
done
