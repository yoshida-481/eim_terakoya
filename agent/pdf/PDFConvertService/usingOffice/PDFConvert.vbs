Option Explicit

'  #
'  # [概要]
'  #    PDFファイル変換
'  # [引数]
'  #    /in：変換対象ファイルパス
'  #    /out：出力PDFファイルパス
'  #    /excelExts：PDF変換対象のExcelの拡張子（カンマ区切りのリスト）
'  #    /wordExts：PDF変換対象のWordの拡張子（カンマ区切りのリスト）
'  #    /powerPointExts：PDF変換対象のPowerPointの拡張子（カンマ区切りのリスト）
'  #
'  # --------------------------------------------------
Dim ErrorFlg: ErrorFlg = False
WScript.Quit main()

' メイン処理
function main()
	main = 0
	On Error Resume Next
		Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")

		' 引数から変換対象ファイルパスを取得
		Set oNamedArgs = WScript.Arguments.Named
		
		Dim origFile: origFile = ""
		If oNamedArgs.Exists("in") Then
			origFile = WScript.Arguments.Named.Item("in")
		End If

		Dim pdfFile: pdfFile   = ""
		If oNamedArgs.Exists("out") Then
			pdfFile = WScript.Arguments.Named.Item("out")
		End If
		WScript.StdOut.WriteLine(logMessage("INFO", "【PDF変換】" & fso.getFileName(origFile)))

		' 出力フォルダを作成
		createFolder(fso.GetParentFolderName(pdfFile))

		' 拡張子から実行するコンバータを決定
		Dim excelExts: excelExts = ""
		If oNamedArgs.Exists("excelExts") Then
			excelExts = WScript.Arguments.Named.Item("excelExts")
		End If

		Dim wordExts: wordExts = ""
		If oNamedArgs.Exists("wordExts") Then
			wordExts = WScript.Arguments.Named.Item("wordExts")
		End If

		Dim powerPointExts: powerPointExts = ""
		If oNamedArgs.Exists("powerPointExts") Then
			powerPointExts = WScript.Arguments.Named.Item("powerPointExts")
		End If

		Dim converter
		if (contains(excelExts, fso.getExtensionName(origFile))) then
			converter = "PDFConvertExcel.vbs"

		elseIf (contains(wordExts, fso.getExtensionName(origFile))) then
			converter = "PDFConvertWord.vbs"

		elseIf (contains(powerPointExts, fso.getExtensionName(origFile))) then
			converter = "PDFConvertPowerPoint.vbs"

		else
			WScript.StdErr.WriteLine(logMessage("ERROR", fso.getFileName(origFile) & "：非対応の拡張子です。"))
			ErrorFlg = True
		end if

		' コンバータ実行
		if (Not ErrorFlg) then
		
			' Officeアプリケーションで処理するとカレントフォルダが異なるため、絶対パス表現に直してPDF変換を呼び出す。
			Dim objWSH: Set objWSH = CreateObject("WScript.Shell")
			dim objExec: set objExec = objWSH.Exec("CScript //nologo " & converter & " """ & fso.GetAbsolutePathName(origFile) & """ """ & fso.GetAbsolutePathName(pdfFile) & """")

			' 標準出力/標準エラーを横流し
			do while (NOT (objExec.StdOut.AtEndOfStream AND objExec.StdErr.AtEndOfStream))
				if (NOT objExec.StdOut.AtEndOfStream) then
					WScript.StdOut.WriteLine(objExec.StdOut.ReadLine())
				end if
				if (NOT objExec.StdErr.AtEndOfStream) then
					WScript.StdErr.WriteLine(objExec.StdErr.ReadLine())
				end if
			loop

			if (objExec.ExitCode <> 0) then
				ErrorFlg = true
			end if
		end if

		if (ErrorFlg) then
			WScript.StdErr.WriteLine(logMessage("ERROR", "PDF変換に失敗しました。：" & fso.getFileName(origFile)))
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

' カンマ区切りの拡張子リストに、check対象拡張子が含まれていればtrue
function contains(propExtensions, checkExtension)
	Dim extensions: extensions = split(propExtensions, ",")
	
	Dim extension
	for each extension in extensions
		if (UCase(extension) = UCase(checkExtension)) then
			contains = true
			exit function
		end if
	next

	contains = false
end function

' フォルダを作成します
function createFolder(folderPath)
	Dim fso : Set fso = CreateObject("Scripting.FileSystemObject")

	' 無限ループを避ける
	if (folderPath = "") then
		exit function
	end if

	' フォルダが存在しない場合、
	if (Not fso.FolderExists(folderPath)) then
		' まずは再帰で親フォルダを作成
		createFolder(fso.GetParentFolderName(folderPath))

		' 自分のフォルダを作成
		fso.CreateFolder(folderPath)
	end if
end function
