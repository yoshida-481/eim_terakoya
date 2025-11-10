<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * オブジェクトソート用クラス
	 */
	class ObjListComparator implements java.util.Comparator{
		private EIMSession _sess;
		private AppObjectConditionHelper _helper;
		/** コンストラクタ */
		public ObjListComparator(EIMSession session) {
			_sess = session;
			_helper = new AppObjectConditionHelper(_sess);
		}
		/** 比較関数 */
		public int compare(Object obj1, Object obj2) {
			try {
				EIMObject ent1 =(EIMObject)obj1;
				EIMObject ent2 =(EIMObject)obj2;
				boolean isFolderType1 = _helper.isTypeOfFolder(ent1.getType());
				boolean isTagType1 = _helper.isTypeOfTag(ent1.getType());
				boolean isFolderType2 = _helper.isTypeOfFolder(ent2.getType());
				boolean isTagType2 = _helper.isTypeOfTag(ent2.getType());
				String val1 = ent1.getName();
				String val2 = ent2.getName();
				long id1 = ent1.getId();
				long id2 = ent2.getId();
				if( isTagType1 ) {
					if( isTagType2 ) {
						if( val1.compareTo(val2) == 0 ) {
							return (int)(id1 - id2);
						} else {
							return val1.compareTo(val2);
						}
					} else {
						return -1;
					}
				} else if( isFolderType1 ) {
					if( isTagType2 ) {
						return 1;
					} else if( isFolderType2 ) {
						if( val1.compareTo(val2) == 0 ) {
							return (int)(id1 - id2);
						} else {
							return val1.compareTo(val2);
						}
					} else {
						return -1;
					}
				} else {
					if( isTagType2 || isFolderType2 ) {
						return 1;
					} else {
						if( val1.compareTo(val2) == 0 ) {
							return (int)(id1 - id2);
						} else {
							return val1.compareTo(val2);
						}
					}
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
		if (object == null) {
			message = EIMResource.getMessage(sess,
			"EIM.ERROR.LOGIC.NOTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource
			.getMessage("EIM.ERROR.LOGIC.NOTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),
			message, paramId));
			return;
		}
		// 権限チェック
		if (object.getSecurity() != null) {
			if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// XML
		out.println("<objectList");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			String objTypeName = "";
			if(helper.isTypeOfFolder(object.getType())) {
				objTypeName = helper.getObjTypeNameFolderXmlEscaped();
			} else if(helper.isTypeOfTag(object.getType())) {
				objTypeName = EIMConfig.get("OBJECT_TYPE_NAME_TAG");
			} else {
				objTypeName = StringUtils.xmlEncode(object.getType().getDefName());
			}
			out.println(" objTypeName=\"" + objTypeName + "\"");
			// パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			// 署名・暗号化状態
			long signencr = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), 0);
			out.println(" signencr=\"" + signencr + "\"");
			// 有効期限
			boolean expiration = false;
			Date expirationDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
			if(expirationDate != null) {
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate);
			}
			out.println(" expiration=\"" + expiration + "\"");
			// readOnly
			out.println(" readOnly=\"" + helper.isReadOnlyAccess(object) + "\"");
			out.println(" isWorkflowFolder=\"false\"");
			out.println(" isDocument=\"false\"");
			out.println(" isDocumentLink=\"false\">");
		// 公開ドキュメントのフォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess,
			EIMConfig.get("FORMAT_NAME_PUBLIC"));

		// Objects That Given Tag
		ArrayList objList = (ArrayList)TagUtil.getTagGivenObj(sess,object);
		if( objList != null ) {
			Collections.sort(objList, new ObjListComparator(sess));
			for (Iterator i = objList.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				out.println(" <object");
					out.println(" objId=\"" + tmpObj.getId() + "\"");
					out.println(" objName=\"" + StringUtils.xmlEncode(tmpObj.getName()) + "\"");
					boolean isFolderType = helper.isTypeOfFolder(tmpObj.getType());
					boolean isTagType = helper.isTypeOfTag(tmpObj.getType());
					if( isFolderType ) {	// フォルダの場合は「フォルダ」を返却
						out.println(" objTypeName=\"" + helper.getObjTypeNameFolderXmlEscaped()+ "\"");
					}
					else if( isTagType ) {	// タグの場合は「タグ」を返却
						out.println(" objTypeName=\"" + helper.getObjTypeNameTagXmlEscaped()+ "\"");				
					}
					else {	// ドキュメントの場合はタイプ名称を返却
						out.println(" objTypeName=\"" + StringUtils.xmlEncode(tmpObj.getType().getDefName()) + "\"");
					}
					// PDFアイコンの表示判定
					out.println(" isDspPdfIcon=\""
							+ AppLogicUtil.isDspPdfIcon(sess, tmpObj, formatPDF) + "\"");
					//PDF結合に失敗したか否か
					boolean isPDFJoinFailed = false;
					long joinFailFlg = AppObjectUtil.getIntAttr(sess, tmpObj, helper.getAttrNameOfPDFJoinFail(), AppConstant.FLAG_OFF);
					if(joinFailFlg == AppConstant.FLAG_ON) {
						isPDFJoinFailed = true;
					}
					if ((tmpObj.getStatus() != null) && !helper.isTypeOfFolderUnderFolderWithWorkflow(tmpObj)) {
						out.println(" isDspPubIconForNoWF=\"false\"");
						out.println(" statusTypeKind=\"" + tmpObj.getStatus().getType().getKind() + "\"");
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
						out.println(" isWorkflowFolder=\"" + helper.isTypeOfFolderWithWorkflow(tmpObj)
								+ "\"");
					} else {// for Document
						out.println(" isWorkflowFolder=\"false\"");
					}
					// 有効期限
					expiration = false;
					expirationDate = AppObjectUtil.getDateAttr(sess, tmpObj, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
					if(expirationDate != null) {
						expiration = DateUtils.judgeExpirationDate(sess, expirationDate);
					}
					out.println(" expiration=\"" + expiration + "\"");
					// readOnly
					out.println(" readOnly=\"" + helper.isReadOnlyAccess(tmpObj) + "\"");
					// Path
					String pathAttr = AppObjectUtil.getPath(tmpObj);
					out.println(" path=\"" + StringUtils.xmlEncode(pathAttr) + "\"");
					// revision
					out.println(" rev=\"" + ((isFolderType||isTagType) ? "-" : String.valueOf(tmpObj.getRev())) + "\"");
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
