
function editDocument(Array) {
	try {
		sharePoitDocument = new ActiveXObject("SharePoint.OpenDocuments.2");
		sharePoitDocument.EditDocument(Array[0], Array[1]);
	} catch(e) {
	}
}

function getSessionId() {
	
	// Cookieから値を取得する
	var cookieString = document.cookie;

	// 要素ごとに ";" で区切られているので、";" で切り出しを行う
	var cookieKeyArray = cookieString.split(";");

	// 要素分ループを行う
	for (var i=0; i<cookieKeyArray.length; i++) {
		var targetCookie = cookieKeyArray[i];

		// 前後のスペースをカットする
		targetCookie = targetCookie.replace(/^\s+|\s+$/g, "");

		var valueIndex = targetCookie.indexOf("=");
		if (targetCookie.substring(0, valueIndex) == "JSESSIONID") {
			// キーが引数と一致した場合、値を返す
			return unescape(targetCookie.slice(valueIndex + 1));
		}
	}
	return "";
}