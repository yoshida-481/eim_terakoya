@echo off

REM #
REM # [概要]
REM #    引数で渡されたファイルをPDFに変換する
REM # [引数]
REM #    /in：変換対象ファイルパス
REM #    /out：出力PDFファイルパス
REM #    /excelExts：PDF変換対象のExcelの拡張子（カンマ区切りのリスト）
REM #    /wordExts：PDF変換対象のWordの拡張子（カンマ区切りのリスト）
REM #    /powerPointExts：PDF変換対象のPowerPointの拡張子（カンマ区切りのリスト）
REM #
REM # --------------------------------------------------
pushd %~dp0

set CONVERT_ERROR=0

REM PDF変換実行
CScript //nologo PDFConvert.vbs %*

REM 変換エラーがあれば戻り値にエラーを設定
if %ERRORLEVEL% neq 0 (
	set CONVERT_ERROR=1
)


popd

exit /b %CONVERT_ERROR%
