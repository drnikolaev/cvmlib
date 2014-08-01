@@echo off
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

devenv test_ilp64_2010.vcxproj /%BUILDCMD% "Debug|x64"
devenv test_ilp64_2010.vcxproj /%BUILDCMD% "Release|x64"


