@@echo off
REM As a parameters pass one of the follwing: Clean, Build, Rebuild
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

devenv ..\cvmlib_2015.sln /%BUILDCMD% Debug   /Project ftn_lapack_2015.vfproj /ProjectConfig "Debug|Win32"
devenv ..\cvmlib_2015.sln /%BUILDCMD% Release /Project ftn_lapack_2015.vfproj /ProjectConfig "Release|Win32"
devenv ..\cvmlib_2015.sln /%BUILDCMD% Debug   /Project ftn_mkl_2015.vfproj    /ProjectConfig "Debug|Win32"
devenv ..\cvmlib_2015.sln /%BUILDCMD% Release /Project ftn_mkl_2015.vfproj    /ProjectConfig "Release|Win32"
