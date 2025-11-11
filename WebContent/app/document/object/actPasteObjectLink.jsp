<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	// [09/02/03 notice by ik.]
	// パラメータ "isDocumentLink" は使われていない。

	//Parameter
	String prmObjId[] = request.getParameterValues("objId");
	String prmParentObjId[] = request.getParameterValues("parentObjId");
	String prmPasteType[] = request.getParameterValues("pasteType");
	String prmIsDocumentLink[] = request.getParameterValues("isDocumentLink");
	String prmLinkUpdateTiming[] = request.getParameterValues("linkUpdateTiming");
	
	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
		paramIdList.add("parentObjId[" + i + "]=" + prmParentObjId[i]);
		paramIdList.add("pasteType[" + i + "]=" + prmPasteType[i]);
		paramIdList.add("isDocumentLink[" + i + "]=" + prmIsDocumentLink[i]);
		paramIdList.add("linkUpdateTiming[" + i + "]=" + prmLinkUpdateTiming[i]);
	}
	Object[] paramId = paramIdList.toArray();
	
	try
	{
		// [Cut]
		// カット＆リンク貼り付けは、対象オブジェクト(ドキュメント・フォルダ・リンク)によらずエラーとする
		if(prmObjId.length > 0 && prmPasteType[0].equals("CUT")) {
			message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.DOCLINK.TARGET.NOTSELECTED");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCLINK.TARGET.NOTSELECTED");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// 以降の処理は[Copy][Branch Copy]時
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		//User
		user = (EIMUser)sess.getAttribute("USER");

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// リンク作成したオブジェクトのIDリスト
		List<Long> createLinkObjIdList = new ArrayList<Long>();

		// 渡されてきたオブジェクト分ループ
		for( int i = 0; i < prmObjId.length; i++ ) {
			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId[i]));
			//Parent Object
			EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId[i]));
			
			// [09/02/03 modified by ik.]
			// リンク作成処理を AppLogicUtil に移動
			AppLogicUtil.createDocLink(object, parentObj, true, true, helper, Integer.parseInt(prmLinkUpdateTiming[i]));
			createLinkObjIdList.add((long)object.getId());

			// ### SEARCH FRAMEWORK 検索FW更新通知 リンク元オブジェクト、貼り付け先オブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_LINKPASTE_DOCUMENTLINK");
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_LINKPASTE_PARENT_FOLDER", "SEARCHFW_LINKPASTE_PARENT_WORKSPACE", null);
			
		}
		
		//Commit
		sess.commit();

		// リンク作成したオブジェクトIDを返却
		out.println("<object>");
		for (Long createLinkObjId : createLinkObjIdList) {
			out.println("<object objId=\"" + createLinkObjId + "\" isDocumentLink=\"true\" />");
		}
		out.println("</object>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>