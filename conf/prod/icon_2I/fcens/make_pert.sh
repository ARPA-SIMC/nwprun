#/bin/bash

typeset -a par
i=0
for p in tur_len \
           rlam_heat \
           tkhmin \
           tkmmin \
           rat_sea \
           q_crit \
           mu_rain \
           rain_n0_factor \
           gkwake; do
    par[$i]=`grep $p pertens.in.0`
    i=$(($i+1))
done
echo ${par[*]}

#par=("  tur_len       = 300." "  rlam_heat     = 10" "  tkhmin        = 0.5" "  tkmmin        = 0.75" "  rat_sea       = 0.8" "  q_crit        = 2.0" "mu_rain                      = 0.5" "rain_n0_factor               = 0.1" "tune_gkwake                  = 0.25")
set -x

sed -e "s/${par[0]}/tur_len = 500./" pertens.in.0 > pertens.in.1
sed -e "s/${par[1]}/rlam_heat = 3/" -e "s/${par[6]}/mu_rain = 0./" pertens.in.0 > pertens.in.2
sed -e "s/${par[2]}/tkhmin = 0.3/" pertens.in.0 > pertens.in.3
sed -e "s/${par[3]}/tkmmin = 0.5/" -e "s/${par[5]}/q_crit = 3./" pertens.in.0 > pertens.in.4
sed -e "s/${par[4]}/rat_sea = 1.5/" pertens.in.0 > pertens.in.5
sed -e "s/${par[8]}/tune_gkwake = 1.5/" pertens.in.0 > pertens.in.6
sed -e "s/${par[1]}/rlam_heat = 3/" pertens.in.0 > pertens.in.7
sed -e "s/${par[2]}/tkhmin = 0.7/" pertens.in.0 > pertens.in.8
sed -e "s/${par[3]}/tkmmin = 0.5/" pertens.in.0 > pertens.in.9
sed -e "s/${par[8]}/tune_gkwake = 1.5/" pertens.in.0 > pertens.in.10
sed -e "s/${par[0]}/tur_len = 150./" -e "s/${par[6]}/mu_rain = 0./" pertens.in.0 > pertens.in.11
sed -e "s/${par[3]}/tkmmin = 0.9/" -e "s/${par[7]}/rain_n0_factor = 1./" pertens.in.0 > pertens.in.12
sed -e "s/${par[1]}/rlam_heat = 18/" pertens.in.0 > pertens.in.13
sed -e "s/${par[2]}/tkhmin = 0.3/" -e "s/${par[5]}/q_crit = 3./" pertens.in.0 > pertens.in.14
sed -e "s/${par[4]}/rat_sea = 1.5/" pertens.in.0 > pertens.in.15
sed -e "s/${par[0]}/tur_len = 150./" pertens.in.0 > pertens.in.16
sed -e "s/${par[3]}/tkmmin = 0.9/" pertens.in.0 > pertens.in.17
sed -e "s/${par[2]}/tkhmin = 0.7/" -e "s/${par[7]}/rain_n0_factor = 1./" pertens.in.0 > pertens.in.18
sed -e "s/${par[1]}/rlam_heat = 18/" pertens.in.0 > pertens.in.19
sed -e "s/${par[0]}/tur_len = 500./" pertens.in.0 > pertens.in.20
