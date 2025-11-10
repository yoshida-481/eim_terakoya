
# ユーザ・パス・ファイル名入力

$filename = Read-Host "ファイルパスを入力してください"
$username = Read-Host "ユーザを入力してください"
$SecureString = Read-Host "パスワードを入力してください" -asSecureString
$useExcel  = Read-Host "取り込ファイルをExcelで編集した場合は「y」、テキストエディタで編集した場合は「n」を入力してください"

$ptr = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($SecureString)
$pass = [System.Runtime.InteropServices.Marshal]::PtrToStringBSTR($ptr)


if ( $useExcel -eq 'y' ){
	# 作業用フォルダを作成する
	if((Test-Path ".\temp") -ne $true){
		New-Item .\temp -itemType Directory
	}

	# CSVファイルにダブルクォーテーションを付与する
	$cont=get-content $filename | convertfrom-csv
	$cont | convertto-csv -NoTypeInformation | out-file .\temp\import.csv -Encoding default

	# インポート処理実行
	./execUpdate.bat '.\temp\import.csv' $username $pass
}else{
	#インポート処理実行
	./execUpdate.bat $filename $username $pass
}