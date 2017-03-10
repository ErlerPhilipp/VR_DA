cd bin\Release\
REM renderingQuality pointSetFile referenceOperationsFile loadReference storagePath autoCompareInSec
PointcloudEditor.exe 1.0 "..\..\resources\pointclouds\JBs_Haus.pts" "..\..\resources\pointclouds\JBs_Haus_groundTruth.xml" true "C:\bla\vgmCache" 0
REM PointcloudEditor.exe 1.0 "..\..\resources\pointclouds\Laserscan-MS60_Beiglboeck-2015.pts" "..\..\resources\pointclouds\JBs_Haus_groundTruth.xml" false "C:\bla\vgmCache" 0