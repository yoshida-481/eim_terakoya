<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="java.math.BigInteger"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>
<%@ page import="app.document.search.EIMDocSearchType"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * オブジェクトソート用クラス
	 */
	class ObjListComparator implements java.util.Comparator{
		private EIMSession _sess;
		/** コンストラクタ */
		public ObjListComparator(EIMSession session) {
			_sess = session;
		}
		/** 比較関数 */
		public int compare(Object obj1, Object obj2) {
			try {
				EIMObject ent1 =(EIMObject)obj1;
				EIMObject ent2 =(EIMObject)obj2;
				String val1 = ent1.getName();
				String val2 = ent2.getName();
				long id1 = ent1.getId();
				long id2 = ent2.getId();
				if( val1.compareTo(val2) == 0 ) {
					return BigInteger.valueOf(id1 - id2).signum();
				} else {
					return val1.compareTo(val2);
				}
			} catch(Exception e) {
				throw new RuntimeException(e);
			}
		}
	}


	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	/* EIMObjID of Tag */
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = { "objId=" + prmObjId };

	try {
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request,
			"EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource
			.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// User
		user = (EIMUser) sess.getAttribute("USER");
		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// ごみ箱に入っているか
		if( AppObjectUtil.isObjectInRecycleLite(object) ) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.TAGASSIGN.RECYCLED");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.TAGASSIGN.RECYCLED");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 公開ドキュメントのフォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess,
			EIMConfig.get("FORMAT_NAME_PUBLIC"));
		// フォルダタイプ・タグタイプフラグ
		boolean isFolderType = helper.isTypeOfFolder(object.getType());
		boolean isTagType = helper.isTypeOfTag(object.getType());

		// 選択ドキュメント・フォルダのステータスが「なし」「公開済」以外の場合
		if ((object.getStatus() != null) && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
			out.clear();
			if( isFolderType ) {
				// フォルダ
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.FOL.CANTASSIGN");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.TAGASSIGN.FOL.CANTASSIGN");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			} else if( isTagType ) {
				// タグ (System Error)
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			} else {
				// ドキュメント
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			}
			return;
		}

		// XML
		out.println("<objectList");
		out.println(" objId=\"" + object.getId() + "\"");
		out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
		if( isFolderType ) {	// フォルダの場合は「フォルダ」を返却
			out.println(" objTypeName=\"" + helper.getObjTypeNameFolderXmlEscaped()+ "\"");
		}
		else if( isTagType ) {	// タグの場合は「タグ」を返却
			out.println(" objTypeName=\"" + helper.getObjTypeNameTagXmlEscaped()+ "\"");
		}
		else {	// ドキュメントの場合はタイプ名称を返却
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
		}
		// PDFアイコンの表示判定
		out.println(" isDspPdfIcon=\""
				+ AppLogicUtil.isDspPdfIcon(sess, object, formatPDF) + "\"");
		//PDF結合に失敗したか否か
		boolean isPDFJoinFailed = false;
		long joinFailFlg = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfPDFJoinFail(), AppConstant.FLAG_OFF);
		if(joinFailFlg == AppConstant.FLAG_ON) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			return;
		}
		if ((object.getStatus() != null) && !helper.isTypeOfFolderUnderFolderWithWorkflow(object)) {
			out.println(" isDspPubIconForNoWF=\"false\"");
			out.println(" statusTypeKind=\"" + object.getStatus().getType().getKind() + "\"");
		} else {
			if( isPDFJoinFailed ) {
				out.println(" isDspPubIconForNoWF=\"false\"");
			} else {
				out.println(" isDspPubIconForNoWF=\"" + !(isFolderType||isTagType) + "\"");
			}
			out.println(" statusTypeKind=\"\"");
		}
		// 自身がドキュメントリンクかどうかを出力(isDocumentLink)
		out.println(" isDocumentLink=\"false\"");
		// 自身がワークフローフォルダかどうかを出力(isWorkflowFolder)
		if (isFolderType) { // for Folder
			out.println(" isWorkflowFolder=\"" + helper.isTypeOfFolderWithWorkflow(object)
					+ "\"");
		} else {// for Document, Tag
			out.println(" isWorkflowFolder=\"false\"");
		}
		// 有効期限
		boolean expiration = false;
		Date expirationDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
		if(expirationDate != null) {
			expiration = DateUtils.judgeExpirationDate(sess, expirationDate);
		}
		out.println(" expiration=\"" + expiration + "\"");
		// readOnly
		out.println(" readOnly=\"" + helper.isReadOnlyAccess(object) + "\"");
		// Path
		String pathAttr = AppObjectUtil.getPath(object);
		out.println(" path=\"" + StringUtils.xmlEncode(pathAttr) + "\"");
		// revision
		out.println(" rev=\"" + ((isFolderType||isTagType) ? "-" : String.valueOf(object.getRev())) + "\"");
		out.println(" >");
		// Tags That Assigned to the Object
		List objList = new ArrayList();
		long[] tags = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if( tags != null ) {
			// タグ一覧をSearchUtilsで取得
			objList = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_ADDED_TAGLIST,
											EIMAccessRole.READ, null, tags);
		}
		if( objList.size() > 0 ) {
			Collections.sort(objList, new ObjListComparator(sess));
			for (Iterator i = objList.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				out.println(" <object");
				out.println(" objId=\"" + tmpObj.getId() + "\"");
				out.println(" objName=\"" + StringUtils.xmlEncode(tmpObj.getName()) + "\"");
				out.println(" objTypeId=\"" + tmpObj.getType().getId() + "\"");
				String objTypeName = "";
				if(helper.isTypeOfFolder(tmpObj.getType())) {
					objTypeName = helper.getObjTypeNameFolderXmlEscaped();
				} else if(helper.isTypeOfTag(tmpObj.getType())) {
					objTypeName = EIMConfig.get("OBJECT_TYPE_NAME_TAG");
				} else {
					objTypeName = StringUtils.xmlEncode(tmpObj.getType().getDefName());
				}
				out.println(" objTypeName=\"" + objTypeName + "\"");
				// パス
				String path = StringUtils.nullToBlank(AppObjectUtil.getPath(tmpObj));
				out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
				// 署名・暗号化状態
				long signencr = AppObjectUtil.getIntAttr(sess, tmpObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), 0);
				out.println(" signencr=\"" + signencr + "\"");
				// 有効期限
				expiration = false;
				expirationDate = AppObjectUtil.getDateAttr(sess, tmpObj, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
				if(expirationDate != null) {
					expiration = DateUtils.judgeExpirationDate(sess, expirationDate);
				}
				out.println(" expiration=\"" + expiration + "\"");
				// readOnly
				out.println(" readOnly=\"" + helper.isReadOnlyAccess(tmpObj) + "\"");
				out.println(" isWorkflowFolder=\"false\"");
				out.println(" isDocument=\"false\"");
				out.println(" isDocumentLink=\"false\"");
				out.println(" />");
			}
		}
		out.println("</objectList>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime
		.getMessage(), paramId), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
			if (sess != null) {
		sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess,
			"EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()),
			se);
		}
	}
%>
