@@echo off
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1

pushd ..\ftn
call build_ftn_em64t.cmd %BUILDCMD%
popd

devenv src_mkl_2013.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_2013.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_ilp64_2013.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_ilp64_2013.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg


GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit
