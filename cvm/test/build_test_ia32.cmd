@@echo off
SET BUILDCMD=Build
IF [%1] NEQ [] SET BUILDCMD=%1

devenv test_2013.vcxproj /%BUILDCMD% "Debug|Win32"
devenv test_2013.vcxproj /%BUILDCMD% "Release|Win32"


