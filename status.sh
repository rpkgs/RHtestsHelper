#!/bin/bash

# Define a timestamp function
timestamp() {
  date +"%T" # current time
}

# bjobs -u all
while true
do
  timestamp # print timestamp
  # echo Keep running
  bhist -a | tail  # 查看历史用时
  sleep 20
done
