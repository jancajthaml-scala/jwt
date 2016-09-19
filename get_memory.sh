#!/bin/sh

# currently only for OSX

while read B; do
  [ $B -lt 1024 ] && echo ${B}B && break
  KB=$(((B+512)/1024))
  [ $KB -lt 1024 ] && echo ${KB}K && break
  MB=$(((KB+512)/1024))
  [ $MB -lt 1024 ] && echo ${MB}M && break
  GB=$(((MB+512)/1024))
  [ $GB -lt 1024 ] && echo ${GB}G && break
  echo $(((GB+512)/1024))T
done <<< $(sysctl -a | grep hw.memsize | rev | cut -d: -f1)