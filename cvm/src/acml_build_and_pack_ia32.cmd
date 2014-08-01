@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIBDIR=..\..\lib\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_ia32.dll cvm_ia32.lib cvm_ia32_debug.dll cvm_ia32_debug.lib cvm_ia32_debug.pdb regtest_cvm_ia32.exe regtest_cvm_ia32_debug.exe regtest_cvm_ia32_debug.pdb

SET CVMVER=8.1
SET ACML_PATH=C:\AMD\acml4.4.0
SET INTEL_REDIST="C:\Program Files (x86)\Intel\Composer XE 2013 SP1\redist"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\redist\x86\Microsoft.VC120.CRT"

rem SET BUILDCMD=Build
SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2

SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------
pushd ..\ftn
call build_ftn_ia32.cmd %BUILDCMD%
popd
rem ----------------------------------------------------------------------------------------------

devenv src_acml_2013.vcxproj /%BUILDCMD% "Debug|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_2013.vcxproj /%BUILDCMD% "Release|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call build_test_ia32.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIBDIR%
rmdir /S /Q acml32
mkdir acml32
copy %ACML_PATH%\ifort32\lib\libacml_dll.dll acml32
copy %INTEL_REDIST%\ia32\compiler\libifcoremd.dll acml32
copy %INTEL_REDIST%\ia32\compiler\libmmd.dll acml32
copy %MSREDIST%\msvcp120.dll acml32
copy %MSREDIST%\msvcr120.dll acml32
SET PATH=.;acml32
regtest_cvm_ia32 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIBDIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f /s %OUTDIR%cvmlib.%CVMVER%.acml.ia32.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml.ia32.zip %FILESET% acml32\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)


devenv src_acml_mp_2013.vcxproj /%BUILDCMD% "Debug|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_mp_2013.vcxproj /%BUILDCMD% "Release|Win32"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call build_test_ia32.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIBDIR%
rmdir /S /Q acml_mp32
mkdir acml_mp32
copy %ACML_PATH%\ifort32_mp\lib\libacml_mp_dll.dll acml_mp32
copy %INTEL_REDIST%\ia32\compiler\libifcoremd.dll acml_mp32
copy %INTEL_REDIST%\ia32\compiler\libmmd.dll acml_mp32
copy %INTEL_REDIST%\ia32\compiler\libiomp5md.dll acml_mp32
copy %MSREDIST%\msvcp120.dll acml_mp32
copy %MSREDIST%\msvcr120.dll acml_mp32
SET PATH=.;acml_mp32
regtest_cvm_ia32 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIBDIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f /s %OUTDIR%cvmlib.%CVMVER%.acml_mp.ia32.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml_mp.ia32.zip %FILESET% acml_mp32\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)


GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit
