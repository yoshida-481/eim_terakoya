<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>

<%@page import="java.text.SimpleDateFormat"%>
<%@page import="org.apache.commons.lang3.ArrayUtils"%>

<%
	Log log = LogFactory.getLog(this.getClass().getName());

	response.setContentType("text/html; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	Object[] paramId = null;

	//Message
	String message = null;

	try
	{
		//セッション情報の取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			response.sendError(1001);
			return;
		}
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		loginUser = (EIMUser)sess.getAttribute("USER");

		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//クライアントから受け渡されたパラメタの取得
		String prmObjId = EIMUtils.getParameter(request, "objId");
		String prmObjTypeId = EIMUtils.getParameter(request, "documentTypeId");
		String prmCreateUserId = EIMUtils.getParameter(request, "createUserId");
		String prmProperty = EIMUtils.getParameter(request, "property");
		String prmExpireDate = EIMUtils.getParameter(request, "expireDate");
		String prmObjNum = EIMUtils.getParameter(request, "objNum");
		paramId = new Object[]{
				"objId=" + prmObjId,
				"objTypeId=" + prmObjTypeId,
				"createUserId=" + prmCreateUserId,
				"property=" + prmProperty,
				"expireDate=" + prmExpireDate,
				"objNum=" + prmObjNum
				};
		
		//ドキュメントタイプの取得
		EIMObjectType documentObjType = getDocumentObjType(sess, prmObjTypeId);
		if(documentObjType == null) 
		{
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			/*
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			response.sendError(1014);
			return;
			*/
		}
		
		
		/*
		 * ドキュメント・フォルダの登録を行う
		 */
		List<EIMObject> createObjectList = new ArrayList<EIMObject>();
		int objNum = Integer.parseInt(prmObjNum);
		if(objNum > 0) {
			String[] nameArray = new String[objNum];
			String[] pathArray = new String[objNum];
			String[] typeArray = new String[objNum];
			for(int ii = 0; ii < objNum; ii++) 
			{
				nameArray[ii] = EIMUtils.getParameter(request, "objName_" + String.valueOf(ii));
				pathArray[ii] = EIMUtils.getParameter(request, "path_" + String.valueOf(ii));
				typeArray[ii] = EIMUtils.getParameter(request, "objType_" + String.valueOf(ii));
			}
			
			ConfirmLumpUploadManager manager = 
				new ConfirmLumpUploadManager(sess, documentObjType, prmObjId, prmCreateUserId, prmProperty, prmExpireDate);
			createObjectList = manager.createObject(nameArray, pathArray, typeArray);

			// SearchFramework 検索FW更新通知 対象：オブジェクトの親
			if( AppUpdateNoticeUtils.doEntry() )
			{
				EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, object, "SEARCHFW_LUMP_UPLOAD_PARENT_FOLDER", "SEARCHFW_LUMP_UPLOAD_PARENT_WORKSPACE", null);
			}
		}
		
		/*
		 * 一時格納フォルダを削除する
		 */
		EIMObject o = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(o != null) {
			File tmpStoreFolder = new File(EIMConfig.get("TEMP") + "/" + o.getId());
			if(tmpStoreFolder.exists() && FileUtil.clean(tmpStoreFolder)) {
				ObjectUtils.deleteObject(sess, o);
			}
			
			//操作履歴
			EIMAttribute attr = (EIMAttribute)o.getAttribute(EIMConfig.get("ATTR_NAME_REGIST_PATH"));
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.ZIP_UPLOAD, 
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, o,
					null, null, null, attr.getString());
		}

		//Commit
		sess.commit();
		out.println("<object>");
		for (EIMObject createObject : createObjectList) {
			out.println("<object");
				out.println("objId=\"" + createObject.getId() + "\"");
				out.println("objName=\"" +  StringUtils.xmlEncode(createObject.getName()) + "\"");
			out.println("/>");
		}
		out.println("</object>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	finally
	{
		try{
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}

			if(sess != null){
				sess.close();
			}
			
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
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

<%!
/**
 * ドキュメントのオブジェクトタイプを取得する。
 */
private EIMObjectType getDocumentObjType(EIMSession sess,  String id) throws Exception
{
	if (id != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(id)) == null) {
		return null;
	}
	
	return (id == null || id.length() == 0) ?
			ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")) :
			ObjectUtils.getObjectTypeById(sess, Long.parseLong(id));
}
%>
