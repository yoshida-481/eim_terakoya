<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "app.document.approve.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.impl.TaskServiceImpl"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.TaskDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@ page import = "org.springframework.context.ApplicationContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.TaskService"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.PasswordValidateService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.FormatService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.util.ConfigUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Message
	String message = null;

	try{
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
		user = (EIMUser)sess.getAttribute("USER");

		//ユーザ別Box連携利用許可フラグの設定(0:許可しない 1:許可する)
		String boxUserIntegFlg = "false";
		EIMObjectType userObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_USER"));
		EIMObject userObj = ObjectUtils.getObjectByTypeAndName(sess, userObjectType, Long.toString(sess.getUser().getId()));

		long boxflg = AppObjectUtil.getIntAttr(sess, userObj, EIMConfig.get("ATTR_NAME_USER_BOX_INTEGRATION_FLAG"), AppConstant.FLAG_OFF);
		boxUserIntegFlg = (boxflg == AppConstant.FLAG_ON) ? "true" : "false";

		//Group
		String groupPath = "";
		List groupList = GroupUtils.getGroupByUser(sess, user);
				Boolean enableCoverCreateMenu = false;
		for(int i = 0; i < groupList.size(); i++)
		{
			String buff = "";
			EIMGroup group = (EIMGroup)groupList.get(i);
			group = GroupUtils.getGroupById(sess, group.getId());
			while(group != null)
			{
				// 紙文書電子化オプション
				if (OptionConfData.getInstance().convertPaperToPDFFlg && !enableCoverCreateMenu)
				{
					// グループがスキャン用表紙作成可能かどうか
					EIMObjectType groupObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_GROUP"));
					List<EIMObject> groupObjectList = ObjectUtils.getObjectListByTypeAndName(sess, groupObjectType, String.valueOf(group.getId()));
					if (groupObjectList != null && groupObjectList.size() == 1)
					{
						EIMAttribute coverFlagAttribute = groupObjectList.get(0).getAttribute(EIMConfig.get("ATTR_NAME_COVER_CREATION_PERMISSION_FLAG_FOR_SCANNING"));
						if (coverFlagAttribute != null)
						{
							long flag = coverFlagAttribute.getInt();
							enableCoverCreateMenu = (flag == 1) ? true : false;
						}
					}
				}

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

		//Vice Approve
		boolean viceApprove = true;

		//******************
		// 承認依頼チェック
		//******************
		boolean sessPutflg = false;
		List<EIMObject> taskObjectList = null;
		String isPopupExists = "FALSE";
		try{

			if(EIMThreadContext.getEIMSession() == null)
			{
				//Service、Dao呼出に必要。Service、Daoの使用が終わったらsessをremoveすること
				EIMThreadContext.putEIMSession(sess);
				sessPutflg = true;
			}
			// ログインユーザのタスク一覧取得
			taskObjectList  = ApproveCommonUtil.getApproveRequestedTaskList(sess, user.getId());

			// タスク一覧の内、ステータスタイプをMap化
			Map<Long, EIMStatusType> statusTypeMap = new HashMap<Long, EIMStatusType>();
			for (EIMObject obj : taskObjectList)
			{
				EIMStatusType statusType = obj.getStatus().getType();
				if (!statusTypeMap.containsKey((long)statusType.getId()))
				{
					statusTypeMap.put((long)statusType.getId(), statusType);
				}
			}

			// 処理待ちポップアップ通知の有無をチェック
			for (Map.Entry<Long, EIMStatusType> e : statusTypeMap.entrySet())
			{
				EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, e.getValue());

				// ワークフロー設定オブジェクト取得
				EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workFlow.getId());

				long flg = AppObjectUtil.getIntAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG"), AppConstant.FLAG_OFF);

				if (flg == AppConstant.FLAG_ON) {
					isPopupExists = "TRUE";
					break;
				}
			}
		}
		catch(EIMException eime)
		{
			throw eime;
		}
		catch(Exception e)
		{
			throw e;
		}
		finally
		{
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
		}

		//テキスト属性入力欄の最大文字数の取得
		String textAttrMaxChars = EIMConfig.get("TEXT_ATTR_MAX_CHARS");

		//systemセキュリティに属するかどうかチェック
		boolean systemSecurity = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.UPDATE);

		// 公開取消機能利用者設定の取得
		String approveMenuFlag = EIMConfig.getValue("USE_PUBLIC_CANCEL");

		//一般ドキュメント非表示設定
		String isGeneralDocVisible = EIMConfig.get("IS_GENERAL_DOC_VISIBLE");
		//一般ドキュメント非表示設定
		String isGeneralDocIconVisible = EIMConfig.get("IS_GENERAL_DOC_ICON_VISIBLE");
		//一般ドキュメント非表示設定
		String isGeneralFolVisible = EIMConfig.get("IS_GENERAL_FOL_VISIBLE");
		//一般ドキュメント非表示設定
		String isGeneralFolIconVisible = EIMConfig.get("IS_GENERAL_FOL_ICON_VISIBLE");
		// 自動採番
		String enableAutomaticNumbering = EIMConfig.getValue("ENABLE_AUTOMATIC_NUMBERING");
		// PDFカバー文言
		String pdfAutoRegistDocNamePrefix = EIMConfig.getValue("PDF_AUTO_REGIST_DOC_NAME_PREFIX");
		// パスワード変更メニュー非表示設定
		String isChangePassVisible = EIMConfig.get("IS_CHANG_PASSWORD_VISIBLE");
		// ログアウト変更メニュー非表示設定
		String isLogoutVisible = EIMConfig.get("IS_LOGOUT_VISIBLE");

		// 公開取消機能オプション設定の取得
		String optionArray = EIMConfig.get("OPTION_ARRAY");
		String publicCancelFlg = "false";
		if (optionArray.contains("public_cancel")) {
			publicCancelFlg = "true";
		}

		// OCR設定の取得
		String ocrFlg = "false";
		if (optionArray.contains("ocr")) {
			ocrFlg = "true";
		}


		// PDF比較の取得
		String pdfCompareFlg = "false";
		if (optionArray.contains("pdf_compare")) {
			pdfCompareFlg = "true";
		}

		// PDF結合設定の取得
		String pdfJoinFlg = "false";
		if (optionArray.contains("pdf_join")) {
			pdfJoinFlg = "true";
		}

		// ページ分割検索オプション設定
		String searchPageFlg = "false";
		if (optionArray.contains("search_page")) {
			searchPageFlg = "true";
		}

		// 承認中のチェックインオプション設定
		String enableApproverCheckinFlg = "false";
		if (optionArray.contains("enable_approver_checkin")) {
			enableApproverCheckinFlg = "true";
		}

		// 電子捺印付加オプション設定
		String digitalSignatureFlg = "false";
		if (optionArray.contains("digital_signature")) {
			digitalSignatureFlg = "true";
		}

		// 属性ツリービュー設定
		String attributeTreeView = "false";
		if (optionArray.contains("attribute_tree_view")) {
			attributeTreeView = "true";
		}

		// 署名・暗号化オプション設定
		String signatureAndEncryptionFlag = "false";
		if (optionArray.contains("signature_and_encryption")) {
			signatureAndEncryptionFlag = "true";
		}

		// 公開ファイルセキュリティ設定
		String pdfOutputConf = "false";
		if (optionArray.contains("pdf_output_conf")) {
			pdfOutputConf = "true";
		}

		//部分一致検索
		String searchDetailLikeCondition = EIMConfig.get("SEARCH_DETAIL_LIKE_CONDITION");
		// 詳細条件デフォルト表示有無
		String searchDetailsOpen = EIMConfig.getValue("SEARCH_DETAILS_OPEN");

		// JSESSIONID
		String jSessionId = session.getId();
		//EIMANAGER文書管理バージョン
		String eimanagerDocumentVersion = EIMConfig.get("EIMANAGER_DOCUMENT_VERSION");

		// CSVファイル名プレフィックスの取得
		String csvFileHeader = EIMConfig.get("CSV_DOWNLOAD_FILEHEADER");
		String csvAccessHistoryFileHeader = EIMConfig.get("CSV_DOWNLOAD_ACCESS_HISTORY_FILEHEADER");
		String csvCirculationFileHeader = EIMConfig.get("CSV_DOWNLOAD_CIRCULATION_FILEHEADER");
		String csvAccordionSearchFileHeader = EIMConfig.get("CSV_DOWNLOAD_ACCORDION_SEARCH_FILEHEADER");

		// CSVキャラクタセットの取得
		String csvDownloadCharset = EIMConfig.get("CSV_DOWNLOAD_CHARSET");

		// CSVの改行コード
		String csvDownloadNewLine = EIMConfig.get("CSV_DOWNLOAD_NEWLINE");

		// メールアドレス出力フラグの設定
		String csvMailAddressOutputFlg = "false";
		String csvAddress = EIMConfig.get("CSV_DOWNLOAD_ACCESS_HISTORY_OUTPUT_MAILADDRESS");
		if ("1".equals(csvAddress)) {
			csvMailAddressOutputFlg = "true";
		}

		// ドキュメントアクセスURLの名称出力有無
		String docAccessUrlPath = EIMConfig.get("DOC_ACCESS_URL_PATH_INCLUDE");

		// 原本ドキュメントアクセスURLの名称出力有無
		String orgDocAccessUrlPath = EIMConfig.get("ORG_DOC_ACCESS_URL_PATH_INCLUDE");

		// 公開ドキュメントアクセスURLの名称出力有無
		String publicDocAccessUrlPath = EIMConfig.get("PUBLIC_DOC_ACCESS_URL_PATH_INCLUDE");

		// Boxオプションの利用有無
		String boxIntegrationFlg = EIMConfig.get("USE_BOX_INTEGRATION_OPTION");

		//フォーマット選択ダイアログ表示の有無
		String boxDialogFlg = EIMConfig.get("BOX_FORMAT_DIALOG_OPTION");
		
		//フォーマット選択ダイアログ初期値を公開に設定
		String boxDefultSettingPublic = EIMConfig.get("BOX_FORMAT_DEFAULT_SETTING_PUBLIC_FLAG");

		// PDF変換対象拡張子
		String pdfConvertFileType = EIMConfig.get("PDF_CONVERT_FILE_TYPE");

		// ThumbnailImage対象拡張子
		String thumbnailFileType = EIMConfig.get("THUMBNAIL_FILE_TYPE");

		// アコーディオン検索時の画面初期表示設定
		String searchDispMode = EIMConfig.get("DOCUMENT_SEARCH_DISP_MODE");

		// 「全文を含む」検索時に本抜粋表示するフラグ
		String searchContentsFlg = EIMConfig.get("DOCUMENT_SEARCH_CONTENTS_FLG");
		
		// プレビュー変換対象拡張子
		String previewFileType = EIMConfig.get("PREVIEW_FILE_TYPE");

		// アップロードファイルサイズ上限
		String uploadFileSizeMax = EIMConfig.get("UPLOAD_FILE_SIZE_MAX");
		
		//公開ドキュメント
		//前処理
		EIMThreadContext.removeEIMSession();
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		// Service
		FormatService formatService = (FormatService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("formatService2");
		// 公開ドキュメントフォーマット取得
		FormatDomain publicDocumentFormat = formatService.getByDefinitionName(EIMConfig.get("FORMAT_NAME_PUBLIC"));

		// パスワードを変更する必要があるかどうか
		if(EIMThreadContext.getTransactionContext() != null){
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext transactionContext = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(transactionContext);
		transactionContext.setLangId(sess.getLangId());
		transactionContext.setDBConnection(sess.getDBConnection());
		transactionContext.setUser(ConvertUtils.toUserDomain(user));

		UserService userService2ForAuthentication = (UserService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("userService2ForAuthentication");
		UserDomain userDomain = userService2ForAuthentication.getByCode(user.getCode());
		UserDomain checkUser = ConvertUtils.toUserDomain(user);
		checkUser.setUserObject(userDomain.getUserObject());
		PasswordValidateService passwordValidateService = (PasswordValidateService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("passwordValidateService2");
		boolean isChangePasswordFlg = passwordValidateService.isChangePasswordFlg(checkUser);

		// 除外ネームスペース（カンマ区切り）
		String nameSpaceToExclude = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");

		//Session Information
		out.println("<user");
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
			out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
			out.println(" userMail=\"" + StringUtils.xmlEncode(user.getMail()) + "\"");
			out.println(" userAdmin=\"" + user.getAdmin() + "\"");
			out.println(" groupPath=\"" + StringUtils.xmlEncode(groupPath) + "\"");
			out.println(" approveDocument=\"" + (taskObjectList==null ? 0 : taskObjectList.size()) + "\"");
			out.println(" isPopupExists=\"" + isPopupExists + "\"");
			out.println(" viceApprove=\"" + viceApprove + "\"");
			out.println(" systemSecurity=\"" + systemSecurity + "\"");
			out.println(" textAttrMaxChars=\"" + textAttrMaxChars + "\"");
			out.println(" approveMenuFlag=\"" + approveMenuFlag + "\"");
			out.println(" enableCreateCoverDocumentMenu=\"" + enableCoverCreateMenu + "\"");
			out.println(" isGeneralDocVisible=\"" + isGeneralDocVisible + "\"");
			out.println(" isGeneralDocIconVisible=\"" + isGeneralDocIconVisible + "\"");
			out.println(" isGeneralFolVisible=\"" + isGeneralFolVisible + "\"");
			out.println(" isGeneralFolIconVisible=\"" + isGeneralFolIconVisible + "\"");
			out.println(" isChangePassVisible=\"" + isChangePassVisible + "\"");
			out.println(" isLogoutVisible=\"" + isLogoutVisible + "\"");
			out.println(" enableAutomaticNumbering=\"" + enableAutomaticNumbering + "\"");
			out.println(" pdfAutoRegistDocNamePrefix=\"" + pdfAutoRegistDocNamePrefix + "\"");
			out.println(" publicCancelFlg=\"" + publicCancelFlg + "\"");
			out.println(" ocrFlg=\"" + ocrFlg + "\"");
			out.println(" pdfCompareFlg=\"" + pdfCompareFlg + "\"");
			out.println(" pdfJoinFlg=\"" + pdfJoinFlg + "\"");
			out.println(" searchPageFlg=\"" + searchPageFlg + "\"");
			out.println(" enableApproverCheckinFlg=\"" + enableApproverCheckinFlg + "\"");
			out.println(" digitalSignatureFlg=\"" + digitalSignatureFlg + "\"");
			out.println(" attributeTreeView=\"" + attributeTreeView + "\"");
			out.println(" signatureAndEncryptionFlag=\"" + signatureAndEncryptionFlag + "\"");
			out.println(" searchDetailLikeCondition=\"" + searchDetailLikeCondition + "\"");
			out.println(" searchDetailsOpen=\"" + searchDetailsOpen + "\"");
			out.println(" jSessionId=\"" + jSessionId + "\"");
			out.println(" eimanagerDocumentVersion=\"" + eimanagerDocumentVersion + "\"");
			out.println(" csvFileHeader=\"" + csvFileHeader + "\"");
			out.println(" csvAccessHistoryFileHeader=\"" + csvAccessHistoryFileHeader + "\"");
			out.println(" csvCirculationFileHeader=\"" + csvCirculationFileHeader + "\"");
			out.println(" csvAccordionSearchFileHeader=\"" + csvAccordionSearchFileHeader + "\"");
			out.println(" csvDownloadCharset=\"" + csvDownloadCharset + "\"");
			out.println(" csvDownloadNewLine=\"" + csvDownloadNewLine + "\"");
			out.println(" csvMailAddressOutputFlg=\"" + csvMailAddressOutputFlg + "\"");
			out.println(" docAccessUrlPathFlg=\"" + docAccessUrlPath + "\"");
			out.println(" orgDocAccessUrlPathFlg=\"" + orgDocAccessUrlPath + "\"");
			out.println(" publicDocAccessUrlPathFlg=\"" + publicDocAccessUrlPath + "\"");
			out.println(" pdfOutputConf=\"" + pdfOutputConf + "\"");
			out.println(" boxIntegrationFlg=\"" + boxIntegrationFlg + "\"");
			out.println(" boxUserIntegFlg=\"" + boxUserIntegFlg + "\"");
			out.println(" boxDialogFlg=\"" + boxDialogFlg + "\"");
			out.println(" boxDefaultSettingPublic=\"" + boxDefultSettingPublic + "\"");			
			out.println(" pdfConvertFileType=\"" + pdfConvertFileType + "\"");
			out.println(" publicDocumentFormat=\"" + publicDocumentFormat.getId() + "\"");
			out.println(" searchDispMode=\"" + searchDispMode + "\"");
			out.println(" searchContentsFlg=\"" + searchContentsFlg + "\"");
			out.println(" previewFileType=\"" + previewFileType + "\"");
			out.println(" thumbnailFileType=\"" + thumbnailFileType + "\"");
			out.println(" uploadFileSizeMax=\"" + uploadFileSizeMax + "\"");
			out.println(" nameSpaceToExclude=\"" + nameSpaceToExclude + "\"");
			out.println(" isChangePasswordFlg=\"" + isChangePasswordFlg + "\"");
			out.println(">");
		out.println("</user>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
			if(EIMThreadContext.getEIMSession() != null){
				EIMThreadContext.removeEIMSession();
			}
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
