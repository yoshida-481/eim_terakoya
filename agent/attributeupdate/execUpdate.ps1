
#ユーザ・パス・ファイル名入力

$filename = Read-Host "ファイルパスを入力してください"
$username = Read-Host "ユーザを入力してください"
$SecureString = Read-Host "パスワードを入力してください" -asSecureString

$ptr = [System.Runtime.InteropServices.Marshal]::SecureStringToBSTR($SecureString)
$pass = [System.Runtime.InteropServices.Marshal]::PtrToStringBSTR($ptr)

./EIMAttributeUpdate.bat $filename $username $pass


