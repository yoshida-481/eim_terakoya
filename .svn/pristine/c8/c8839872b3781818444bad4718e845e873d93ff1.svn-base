<%@page contentType="text/html; charset=UTF-8"%>
<jsp:useBean id="origin" class="java.lang.String" scope="request" />

<!DOCTYPE html>
<html>
<head>
	<meta charset="UTF-8">
	<meta http-equiv="Pragma" content="no-cache">
	<meta http-equiv="cache-control" content="no-cache">
	<meta http-equiv="Expires" content="Thu,01 Dec 1994 16:00:00 GMT">
	<script>
		var userAgent = window.navigator.userAgent;
		var isIE = userAgent.indexOf('Trident/') >= 0;
		if (isIE) {
		} else {
			if (window.opener) {
				window.opener.postMessage('boxauthorized', '<%=origin%>');
				window.close();
			}
		}
	</script>
</head>

<body style="text-align: center;">
	<p>Boxへのアクセスが許可されました。画面を閉じてください。</p>
	<p>元の画面に戻った後、Boxアカウント選択画面が表示されたままの場合は「アカウント情報を再読み込み」リンクをクリックしてください。</p>
	<button onclick="window.close()">画面を閉じる</button>
</body>
</html>
