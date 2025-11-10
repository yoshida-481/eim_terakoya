<%@page import="java.text.DateFormat"%>
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
		EIMObject object = ObjectUtils.getObjectById(sess, Long
		.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess,
			"EIM.ERROR.LOGIC.NOTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource
			.getMessage("EIM.ERROR.LOGIC.NOTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),
			message, paramId));
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// XML
		out.println("<objectList");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			// パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
		out.println(">");
		// 公開ドキュメントのフォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess,
			EIMConfig.get("FORMAT_NAME_PUBLIC"));

		// Objects That Given Tag
		ArrayList objList = (ArrayList)TagUtil.getTagGivenObj(sess,object);
		objList = sort(objList, new AppObjectConditionHelper(sess));
		
		if( objList != null ) {
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
					boolean expiration = false;
					Date expirationDate = AppObjectUtil.getDateAttr(sess, tmpObj, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
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
					// 署名・暗号化状態
					long signencr = AppObjectUtil.getIntAttr(sess, tmpObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), 0);
					out.println(" signencr=\"" + signencr + "\"");
					
					// タグのオブジェクトIDからインデックスを取得する
					int tagIndex = 0;
					long[] tags = AppObjectUtil.getIntAttrs(sess, tmpObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
					for(;tagIndex < tags.length; tagIndex++) {
						if(new Long(prmObjId).longValue() == tags[tagIndex]) {
							break;
						}
					}
					// タグ付与者
					long[] addUsers = AppObjectUtil.getIntAttrs(sess, tmpObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"));
					EIMUser userObj = UserUtils.getUserById(sess, addUsers[tagIndex]);
					String addUser = (userObj != null? StringUtils.xmlEncode(userObj.getName()) : "");
					out.println(" addUser=\"" + addUser + "\"");
					// タグ付与日時
					Date[] addDates = AppObjectUtil.getDateAttrs(sess, tmpObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));
					out.println(" addDate=\"" + DateUtils.getDBTzToCLTzDate(sess, addDates[tagIndex], "EIM.FORMAT.DATETIME") + "\"");
					// ソート用タグ付与日時日付
					String addDateTime = String.valueOf(addDates[tagIndex].getTime() / 1000);
					out.println(" addDateTime=\"" + addDateTime + "\"");
					
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

<%!
public ArrayList sort(List list, AppObjectConditionHelper helper) throws Exception
{
	if(list == null) {
		return null;
	}
	
	//オブジェクトIDで昇順ソート
	list = AppObjectUtil.getIntSortedList(list, "getId", true);

	List tagList = new ArrayList();
	List folList = new ArrayList();
	List docList = new ArrayList();
	
	//オブジェクトタイプ毎に仕分け
	for (Iterator i = list.iterator(); i.hasNext();) 
	{
		EIMObject obj = (EIMObject) i.next();
		
		if (helper.isTypeOfTag(obj.getType())) {
			tagList.add(obj);
		} else if (helper.isTypeOfFolder(obj.getType())) {
			folList.add(obj);
		} else {
			docList.add(obj);
		}
	}
	
	//各リストをオブジェクト名で昇順ソート
	if (tagList.size() > 1) {
		tagList = AppObjectUtil.getStrSortedList(tagList, "getName", true);
	}
	if (folList.size() > 1) {
		folList = AppObjectUtil.getStrSortedList(folList, "getName", true);
	}
	if (docList.size() > 1) {
		docList = AppObjectUtil.getStrSortedList(docList, "getName", true);
	}
	//各リストを連結
	tagList.addAll(folList);
	tagList.addAll(docList);
	
	return (ArrayList)tagList;
}
%>
