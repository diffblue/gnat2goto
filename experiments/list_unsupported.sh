#!/bin/sh
if ! command -v gnat2goto > /dev/null; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

if [ "$#" -ne 1 ]; then
   echo >&2 "Provide folder name to start"
   exit 2
fi

path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/$1"
gtk_path="/home/diffblue/petr.bauch/code/gtkada/src/"
unit_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/sparkunit/"
include_path=""

for foldername in $(find ${path}/ -type d -name "*"); do
   count=`ls -1 ${foldername}/*.ads 2>/dev/null | wc -l`
   if [ $count != 0 ]
   then
#      echo $foldername
      include_path="${include_path} -I ${foldername}"
   fi
done

# for filename in $(find ${path}/ -name *.ads); do
#    path="$(dirname "$filename")"
#    include_path="${include_path} -I ${path}"
# done

d1_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/arch/ARM/STM32/svd/stm32f7x9/"
d2_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/screen/otm8009a/"
d3_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/touch_panel/ft6x06/"
d4_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/motion/l3gd20/"
d5_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/touch_panel/stmpe811/"
d6_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/screen/ili9341/"
d7_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/motion/lis3dsh/"
d8_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/hal/src/"
d9_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/touch_panel/ft5336/"
d10_path="/home/diffblue/petr.bauch/code/gnat2goto/experiments/Ada_Drivers_Library/components/src/audio/W8994/"

#   echo "gnat2goto ${include_path} ${filename}$"
for filename in $(find ${path}/ -name *.adb); do
   gnat2goto ${include_path} -I "${d1_path}" -I "${d2_path}" -I "${d3_path}" -I "${d4_path}" -I "${d5_path}" -I "${d6_path}" -I "${d7_path}" -I "${d8_path}" -I "${d9_path}" -I "${d10_path}" "${filename}" >"$1".txt 2>&1
   if grep "raised" "$1".txt > /dev/null
   then
      echo "gnat2goto ${include_path} ${filename}$"
      exit 3
   fi
done

#sed '/^\[/ d' < "$1".txt > "$1"_redacted.txt

#gnat2goto -I "$path/" "$path/main.adb"
