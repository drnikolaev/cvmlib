@@echo off
REM As a parameters pass one of the follwing: Clean, Build, Rebuild
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

devenv ftn_mkl_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_mkl_2010.vfproj /%BUILDCMD% "Release|x64"
devenv ftn_mkl_ilp64_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_mkl_ilp64_2010.vfproj /%BUILDCMD% "Release|x64"

devenv ftn_acml_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_acml_2010.vfproj /%BUILDCMD% "Release|x64"
devenv ftn_acml_ilp64_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_acml_ilp64_2010.vfproj /%BUILDCMD% "Release|x64"

devenv ftn_lapack_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_lapack_2010.vfproj /%BUILDCMD% "Release|x64"
devenv ftn_lapack_ilp64_2010.vfproj /%BUILDCMD% "Debug|x64"
devenv ftn_lapack_ilp64_2010.vfproj /%BUILDCMD% "Release|x64"
