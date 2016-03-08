@@echo off
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

devenv ..\test_2015.sln /%BUILDCMD% Debug /Project test_2015.vcxproj /ProjectConfig "Debug|x64"
devenv ..\test_2015.sln /%BUILDCMD% Release /Project test_2015.vcxproj /ProjectConfig "Release|x64"

