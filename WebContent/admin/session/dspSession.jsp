<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.util.CacheUtils" %>
<%@ page import = "org.infinispan.manager.EmbeddedCacheManager" %>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Message
	String message = null;

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(loginUser, (String)session.getAttribute("ADMIN_APP_ID")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Group
		String groupPath = "";
		List groupList = GroupUtils.getGroupByUser(sess, loginUser);
		for(int i = 0; i < groupList.size(); i++)
		{
			String buff = "";
			EIMGroup group = (EIMGroup)groupList.get(i);
			group = GroupUtils.getGroupById(sess, group.getId());
			while(group != null)
			{
				if(group.getParent() != null)
				{
					buff = " - " + group.getName() + buff;
				}
				else
				{
					buff = group.getName() + buff;
				}
				
				group = group.getParent();
			}
			
			if(i > 0)
			{
				groupPath += " / ";
			}
			groupPath += buff;
		}
		
		//テキスト属性入力欄の最大文字数の取得
		String textAttrMaxChars = EIMConfig.get("TEXT_ATTR_MAX_CHARS");

		//EIMANAGER文書管理バージョン
		String eimanagerDocumentVersion = EIMConfig.get("EIMANAGER_DOCUMENT_VERSION");

		// 操作履歴画面表示CSVダウンロード
		String csvDownloadHistoryFileHeader = EIMConfig.get("CSV_DOWNLOAD_HISTORY_FILEHEADER");
		String csvDownloadCharset = EIMConfig.get("CSV_DOWNLOAD_CHARSET");
		String csvDownloadNewLine = EIMConfig.get("CSV_DOWNLOAD_NEWLINE");

		// オプション機能設定の取得
		String optionArray = EIMConfig.get("OPTION_ARRAY");

		// 承認中チェックイン可否設定の取得
		String enableApproverCheckin = "false";
		if (optionArray.contains("enable_approver_checkin")) {
			enableApproverCheckin = "true";
		}

		// URL挿入オプション設定
		String insertURL = "false";
		if (optionArray.contains("insert_url")) {
			insertURL = "true";
		}

		// 属性ツリー使用可否設定の取得
		String attributeTreeView = "false";
		if (optionArray.contains("attribute_tree_view")) {
			attributeTreeView = "true";
		}

		// 電子捺印付加オプション設定
		String digitalSignatureFlg = "false";
		if (optionArray.contains("digital_signature")) {
			digitalSignatureFlg = "true";
		}

		// OCR設定の取得
		String ocrFlg = "false";
		if (optionArray.contains("ocr")) {
			ocrFlg = "true";
		}

		// 公開ファイルセキュリティ設定
		String pdfOutputConf = "false";
		if (optionArray.contains("pdf_output_conf")) {
			pdfOutputConf = "true";
		}

		// ページ分割検索オプション設定
		String searchPageFlg = "false";
		if (optionArray.contains("search_page")) {
			searchPageFlg = "true";
		}

		// 紙文書オプション利用可否設定の取得
		String convertPaperToPDFFlg = "false";
		if (optionArray.contains("convert_paper_to_pdf")) {
			convertPaperToPDFFlg = "true";
		}

		String boxIntegrationFlg = EIMConfig.get("USE_BOX_INTEGRATION_OPTION");
		
		// 電子署名仕様ツールの取得
		String useSignTool = EIMConfig.get("SIGN_USE_TOOL");

		// キャッシュ無効フラグ
		String disabledCacheFlg = "false";
		if (Objects.isNull(CacheUtils.getCacheManager())) {
			disabledCacheFlg = "true";
		}

		// 除外ネームスペース（カンマ区切り）
		String nameSpaceToExclude = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");

		//Session Information
		out.println("<user");
			out.println(" userId=\"" + loginUser.getId() + "\"");
			out.println(" userCode=\"" + StringUtils.xmlEncode(loginUser.getCode()) + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(loginUser.getName()) + "\"");
			out.println(" userKana=\"" + StringUtils.xmlEncode(loginUser.getKana()) + "\"");
			out.println(" userMail=\"" + StringUtils.xmlEncode(loginUser.getMail()) + "\"");
			out.println(" userAdmin=\"" + loginUser.getAdmin() + "\"");
			out.println(" groupPath=\"" + StringUtils.xmlEncode(groupPath) + "\"");
			out.println(" currentLID=\"" + sess.getAttribute("langId") + "\"");
			out.println(" adminAppId=\"" + session.getAttribute("ADMIN_APP_ID") + "\"");
			out.println(" textAttrMaxChars=\"" + textAttrMaxChars + "\"");
			out.println(" eimanagerDocumentVersion=\"" + eimanagerDocumentVersion + "\"");
			out.println(" csvDownloadHistoryFileHeader=\"" + csvDownloadHistoryFileHeader + "\"");
			out.println(" csvDownloadCharset=\"" + csvDownloadCharset + "\"");
			out.println(" csvDownloadNewLine=\"" + csvDownloadNewLine + "\"");
			out.println(" enableApproverCheckinFlg=\"" + enableApproverCheckin + "\"");
			out.println(" insertURL=\"" + insertURL + "\"");
			out.println(" attributeTreeView=\"" + attributeTreeView + "\"");
			out.println(" digitalSignatureFlg=\"" + digitalSignatureFlg + "\"");
			out.println(" ocrFlg=\"" + ocrFlg + "\"");
			out.println(" searchPageFlg=\"" + searchPageFlg + "\"");
			out.println(" pdfOutputConf=\"" + pdfOutputConf + "\"");
			out.println(" convertPaperToPDFFlg=\"" + convertPaperToPDFFlg + "\"");
			out.println(" useSignTool=\"" + useSignTool + "\"");
			out.println(" boxIntegrationFlg=\"" + boxIntegrationFlg + "\"");
			out.println(" disabledCacheFlg=\"" + disabledCacheFlg + "\"");
			out.println(" nameSpaceToExclude=\"" + nameSpaceToExclude + "\"");
			out.println(">");
		out.println("</user>");
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
