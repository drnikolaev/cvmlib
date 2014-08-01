@@echo off
SET ZIPPER=winrar
SET ZIPPERCMD=a -r -afzip
SET LIB64DIR=..\..\lib64\
SET OUTDIR=..\
SET FILESET=LICENSE_1_0.txt cvm_em64t.dll cvm_em64t.lib cvm_em64t_debug.dll cvm_em64t_debug.lib cvm_em64t_debug.pdb regtest_cvm_em64t.exe regtest_cvm_em64t_debug.exe regtest_cvm_em64t_debug.pdb
SET FILESETILP64=LICENSE_1_0.txt cvm_em64t_ilp64.dll cvm_em64t_ilp64.lib cvm_em64t_ilp64_debug.dll cvm_em64t_ilp64_debug.lib cvm_em64t_ilp64_debug.pdb regtest_cvm_em64t_ilp64.exe regtest_cvm_em64t_ilp64_debug.exe regtest_cvm_em64t_ilp64_debug.pdb

SET CVMVER=8.1
SET INTELREDIST="C:\Program Files (x86)\Intel\Composer XE 2013 SP1\redist\intel64\compiler"
SET MKLREDIST="C:\Program Files (x86)\Intel\Composer XE 2013 SP1\redist\intel64\mkl"
SET MSREDIST="C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\redist\x64\Microsoft.VC120.CRT"

SET BUILDCMD=Rebuild
IF [%1] NEQ [] SET BUILDCMD=%1
SET ZIPMODE=Pack
IF [%2] NEQ [] SET ZIPMODE=%2

SET PATHORIG=%PATH%

rem ----------------------------------------------------------------------------------------------
pushd ..\ftn
call build_ftn_em64t.cmd %BUILDCMD%
popd
rem ----------------------------------------------------------------------------------------------

pushd %LIB64DIR%
SET DEST=intel64
rmdir /S /Q %DEST%
mkdir %DEST%
copy %INTELREDIST%\libicaf.dll %DEST%
copy %INTELREDIST%\libifcoremd.dll %DEST%
copy %INTELREDIST%\libifportmd.dll %DEST%
copy %INTELREDIST%\libiomp5md.dll %DEST%
copy %INTELREDIST%\libiompstubs5md.dll %DEST%
copy %INTELREDIST%\libmmd.dll %DEST%
copy %INTELREDIST%\svml_dispmd.dll %DEST%
copy %MKLREDIST%\libimalloc.dll %DEST%
rem copy %MKLREDIST%\mkl_cdft_core.dll %DEST%
copy %MKLREDIST%\mkl_core.dll %DEST%
copy %MKLREDIST%\mkl_def.dll %DEST%
copy %MKLREDIST%\mkl_intel_thread.dll %DEST%
copy %MKLREDIST%\mkl_mc.dll %DEST%
copy %MKLREDIST%\mkl_mc3.dll %DEST%
copy %MKLREDIST%\mkl_p4n.dll %DEST%
copy %MKLREDIST%\mkl_rt.dll %DEST%
copy %MKLREDIST%\mkl_sequential.dll %DEST%
copy %MSREDIST%\msvcp120.dll %DEST%
copy %MSREDIST%\msvcr120.dll %DEST%
popd

rem ----------------------------------------------------------------------------------------------

devenv src_mkl_2013.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_2013.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call build_test_em64t.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
pushd %LIB64DIR%
SET PATH=.;%DEST%
regtest_cvm_em64t -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd
IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
del /f %OUTDIR%cvmlib.%CVMVER%.mkl.em64t.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.mkl.em64t.zip %FILESET% %DEST%\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

devenv src_mkl_ilp64_2013.vcxproj /%BUILDCMD% "Debug|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
devenv src_mkl_ilp64_2013.vcxproj /%BUILDCMD% "Release|x64"
	IF ERRORLEVEL 1 GOTO ErrMsg
pushd ..\test
call build_test_em64t_ilp64.cmd %BUILDCMD%
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
pushd %LIB64DIR%
SET PATH=.;%DEST%
regtest_cvm_em64t_ilp64 -t3 -r3
IF ERRORLEVEL 1 GOTO ErrMsg
SET PATH=%PATHORIG%
popd
IF %ZIPMODE% == Pack (
pushd %LIB64DIR%
del /f %OUTDIR%cvmlib.%CVMVER%.mkl.em64t.ilp64.zip 
%ZIPPER% %ZIPPERCMD% %OUTDIR%cvmlib.%CVMVER%.mkl.em64t.ilp64.zip %FILESETILP64% %DEST%\*.*
	IF ERRORLEVEL 1 GOTO ErrMsg
popd
)

rem ----------------------------------------------------------------------------------------------

GOTO Exit
:ErrMsg
echo ***ERROR DETECTED***
popd
:Exit

SET PATH=%PATHORIG%