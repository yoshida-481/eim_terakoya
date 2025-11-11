<%@ page contentType="image/jpeg; charset=UTF-8"%><%@ page import="java.io.*"%><%
	// Parameter
	String logoImageFileName = request.getParameter("logoImageFileName");
	String slashStr = "/";
	
	// contentType - mime type
	String mimeType = null;
		
	File file = null;
	
	FileInputStream objFis = null;
	
	BufferedInputStream objBis = null;
	
	ServletOutputStream objSos = null;
	
	try {
		
		// SaaS脆弱性：ディレクトリトラバーサル対応
		// リクエストパラメータで指定されたファイル名に「/」が入っている場合はエラーとして出力
		if(logoImageFileName.indexOf(slashStr) != -1) {
			
			// エラーメッセージはmxml側でハンドリング
			return;
		}
		
		String[] imageFileNameArray = logoImageFileName.split("\\.");
						
		int lastSize = imageFileNameArray.length - 1;

		// path
		String imageFilePath = application.getRealPath("logo/" + logoImageFileName);
		
		// get file type
		String imageFileType = imageFileNameArray[lastSize];
		
		// contentTyp judge from file type
		if (imageFileType.equals("swf")) {
			mimeType = "application/x-shockwave-flash";
		} else if (imageFileType.equals("jpg")) {
			mimeType = "image/jpeg";
		} else if (imageFileType.equals("png")) {
			mimeType = "image/png";
		} else if (imageFileType.equals("gif")) {
			mimeType = "image/gif";
		} else {
			// swf、jpg、png、gif以外の拡張子の場合はエラーを出力
			// エラーメッセージはmxmlでハンドリング
			return;
		}
		
		// contentType set
		response.setContentType(mimeType);
		
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
		
		file = new File(imageFilePath);
		
		objFis = new FileInputStream(file);
	
		objBis = new BufferedInputStream(objFis);
	
		response.resetBuffer(); //Weblogicでは必須
		objSos = response.getOutputStream();
	
		int i;
	
		while((i = objBis.read()) != -1){
			objSos.write(i);
		}

		objFis.close();
		objBis.close();
		objSos.close();	
		
	} catch (IOException ie) {
		ie.getStackTrace();
	} catch (Exception e) {
		e.getStackTrace();
	}

%>