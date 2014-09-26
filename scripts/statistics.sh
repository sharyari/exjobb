#!/bin/bash
echo "ABP" > report/result1.tex
make stat r=Examples/ABP.xml
echo "\\\\" >> report/result1.tex
echo "ABP_F" >> report/result1.tex
make stat r=Examples/ABP_F.xml
echo "\\\\" >> report/result1.tex
echo "SW3" >> report/result1.tex
make stat r=Examples/SlidingWindow.xml
echo "\\\\" >> report/result1.tex 
echo "SW3_F" >> report/result1.tex
make stat r=Examples/SlidingWindow_F.xml
echo "\\\\"
echo "BRP" >> report/result1.tex
make stat r=Examples/BRP.xml
echo "\\\\" >> report/result1.tex
echo "BRP_F" >> report/result1.tex
make stat r=Examples/BRP_F.xml
