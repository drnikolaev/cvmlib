@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIB64DIR=..\..\lib64\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_em64t.dll cvm_em64t.lib cvm_em64t_debug.dll cvm_em64t_debug.lib cvm_em64t_debug.pdb regtest_cvm_em64t.exe regtest_cvm_em64t_debug.exe regtest_cvm_em64t_debug.pdb
SET FILESETILP64=LICENSE_1_0.txt cvm_em64t_ilp64.dll cvm_em64t_ilp64.lib cvm_em64t_ilp64_debug.dll cvm_em64t_ilp64_debug.lib cvm_em64t_ilp64_debug.pdb regtest_cvm_em64t_ilp64.exe regtest_cvm_em64t_ilp64_debug.exe regtest_cvm_em64t_ilp64_debug.pdb

SET CVMVER=7.0.2010
SET ACML_PATH=C:\AMD\acml5.3.1
SET INTEL_REDIST="C:\Program Files (x86)\Intel\Composer XE 2013\redist"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\redist\x64\Microsoft.VC100.CRT"

SET BUILDCMD=Rebuild
rem SET BUILDCMD=Build
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2

SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------
pushd ..\ftn
call 2010_build_ftn_em64t.cmd %BUILDCMD%
popd
rem ----------------------------------------------------------------------------------------------

devenv src_acml_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIB64DIR%
rmdir /S /Q acml64
mkdir acml64
copy %ACML_PATH%\ifort64\lib\libacml_dll.dll acml64
copy %INTEL_REDIST%\intel64\compiler\libifcoremd.dll acml64
copy %INTEL_REDIST%\intel64\compiler\libifportmd.dll acml64
copy %INTEL_REDIST%\intel64\compiler\libmmd.dll acml64
copy %INTEL_REDIST%\intel64\compiler\svml_dispmd.dll acml64
copy %MSREDIST%\msvcp100.dll acml64
copy %MSREDIST%\msvcr100.dll acml64
SET PATH=.;acml64
regtest_cvm_em64t -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f %OUTDIR%cvmlib.%CVMVER%.acml.em64t.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml.em64t.zip %FILESET% acml64\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

devenv src_acml_ilp64_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_ilp64_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t_ilp64.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIB64DIR%
rmdir /S /Q acml64_ilp64
mkdir acml64_ilp64
copy %ACML_PATH%\ifort64_int64\lib\libacml_dll.dll acml64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libifcoremd.dll acml64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libifportmd.dll acml64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libmmd.dll acml64_ilp64
copy %INTEL_REDIST%\intel64\compiler\svml_dispmd.dll acml64_ilp64
copy %MSREDIST%\msvcp100.dll acml64_ilp64
copy %MSREDIST%\msvcr100.dll acml64_ilp64
SET PATH=.;acml64_ilp64
regtest_cvm_em64t_ilp64 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f %OUTDIR%cvmlib.%CVMVER%.acml.em64t.ilp64.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml.em64t.ilp64.zip %FILESETILP64% acml64_ilp64\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

devenv src_acml_mp_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_mp_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIB64DIR%
rmdir /S /Q acml_mp64
mkdir acml_mp64
copy %ACML_PATH%\ifort64_mp\lib\libacml_mp_dll.dll acml_mp64
copy %INTEL_REDIST%\intel64\compiler\libifcoremd.dll acml_mp64
copy %INTEL_REDIST%\intel64\compiler\libifportmd.dll acml_mp64
copy %INTEL_REDIST%\intel64\compiler\libmmd.dll acml_mp64
copy %INTEL_REDIST%\intel64\compiler\svml_dispmd.dll acml_mp64
copy %INTEL_REDIST%\intel64\compiler\libiomp5md.dll acml_mp64
copy %MSREDIST%\msvcp100.dll acml_mp64
copy %MSREDIST%\msvcr100.dll acml_mp64
SET PATH=.;acml_mp64
regtest_cvm_em64t -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f %OUTDIR%cvmlib.%CVMVER%.acml_mp.em64t.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml_mp.em64t.zip %FILESET% acml_mp64\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

devenv src_acml_mp_ilp64_2010.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_acml_mp_ilp64_2010.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call 2010_build_test_em64t_ilp64.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd

pushd %LIB64DIR%
rmdir /S /Q acml_mp64_ilp64
mkdir acml_mp64_ilp64
copy %ACML_PATH%\ifort64_mp_int64\lib\libacml_mp_dll.dll acml_mp64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libifcoremd.dll acml_mp64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libifportmd.dll acml_mp64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libmmd.dll acml_mp64_ilp64
copy %INTEL_REDIST%\intel64\compiler\svml_dispmd.dll acml_mp64_ilp64
copy %INTEL_REDIST%\intel64\compiler\libiomp5md.dll acml_mp64_ilp64
copy %MSREDIST%\msvcp100.dll acml_mp64_ilp64
copy %MSREDIST%\msvcr100.dll acml_mp64_ilp64
SET PATH=.;acml_mp64_ilp64
regtest_cvm_em64t_ilp64 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd

IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
	IF ERRORLEVEL 1 GOTO ErrMsg
del /f %OUTDIR%cvmlib.%CVMVER%.acml_mp.em64t.ilp64.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.acml_mp.em64t.ilp64.zip %FILESETILP64% acml_mp64_ilp64\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)


GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit
SET PATH=%PATHORIG%
