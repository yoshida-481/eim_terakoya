Option Explicit

'  #
'  # [概要]
'  #    PDFファイル変換(Word)
'  # [引数]
'  #    1：変換対象ファイルパス
'  #    2：出力PDFファイルパス
'  #
'  # --------------------------------------------------
Dim ErrorFlg: ErrorFlg = False
WScript.Quit main()

' メイン処理
function main()
	main = 0
	On Error Resume Next
		Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")

		' 引数から変換対象ファイルパス、PDFファイルパスを取得
		Dim origFile: origFile = getArgument(1)
		Dim pdfFile: pdfFile = getArgument(2)
		
		Dim app: Set app=WScript.CreateObject("Word.Application")
		app.Visible=False
		app.DisplayAlerts = False
		
		Dim book: Set book = app.Documents.Open(origFile,,True,,"-1")
		checkError()

		if (Not ErrorFlg) then
			Dim wdExportFormatPDF: wdExportFormatPDF = 17
			Call book.ExportAsFixedFormat(pdfFile, wdExportFormatPDF)
			checkError()
		end if

		Call book.Close(False)
		app.DisplayAlerts = true
		app.Quit

		if (ErrorFlg) then
			main = 1
		end if

	On Error Goto 0
end function

' ログ文字列を生成
function logMessage(errorLevel, message)
	logMessage = logMessage & dateString() & " "
	if (errorLevel <> "") then
		logMessage = logMessage & "[" & errorLevel & "]"
	end if
	logMessage = logMessage & WScript.ScriptName 
	logMessage = logMessage & "：" & message
end function

' 現在時刻の文字列を返す
function dateString()
	Dim tNow: tNow = now()
	Dim tTime: tTime = Timer()

	Dim y,m,d,h,n,s,ms

	y = Right("0000" & Year(tNow)  , 4)
	m = Right(  "00" & Month(tNow) , 2)
	d = Right(  "00" & Day(tNow)   , 2)
	h = Right(  "00" & Hour(tNow)  , 2)
	n = Right(  "00" & Minute(tNow), 2)
	s = Right(  "00" & Second(tNow), 2)
	ms = Right("000" & Fix((tTime - Fix(tTime)) * 1000), 3)

	dateString = y & "/" & m & "/" & d & " " & h & ":" & n & ":" & s & "." & ms
end function

' 環境変数を取得
function getEnv(key)
	Dim objWSH: Set objWSH = CreateObject("WScript.Shell")
	getEnv = objWSH.ExpandEnvironmentStrings("%" & key & "%")
end function

' 引数を取得：start=1
function getArgument(pos)
	if (WScript.Arguments.Count < pos) then
		WScript.StdErr.WriteLine(logMessage("ERROR", "第" & pos & "引数が指定されていません。"))
		WScript.Quit 1
	end if

	getArgument = WScript.Arguments(pos-1)
end function

' エラーチェック
function checkError()
	' エラーがあれば、エラーメッセージを出力してエラーフラグを立てる
	if (Err.Number <> 0) then
		ErrorFlg = True
		WScript.StdErr.WriteLine(logMessage("ERROR", "[ErrorCode=" & Err.Number & "]" & Err.Description))
	end if

	' errオブジェクトの内容をリセット
	On Error Goto 0
	On Error Resume Next
end function
