<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@page import="java.util.List"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.service.DocumentFormService"%>

<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.FormDomain"%>


<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmIsReturnObjectId = request.getParameter("isReturnObjectId");
	boolean isReturnObjectId = false;
	if (prmIsReturnObjectId != null && prmIsReturnObjectId.equals("true")) {
		isReturnObjectId = true;
	}
	
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"prmIsReturnObjectId=" + prmIsReturnObjectId
			};

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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		loginUser = (EIMUser)sess.getAttribute("USER");

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
		List<EIMObject> objListInVersion = VersionUtils.getVersion(sess, object).getList();
		long[] objIds = new long[objListInVersion.size()];
		for(int i = 0; i < objListInVersion.size(); i++)
		{
			objIds[i] = objListInVersion.get(i).getId();
		}
		AppObjectUtil.lockObjectById(sess, objIds);
		// 処理待ち中にEIMObjectの値が変更された可能性があるので再取得
		object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//Check Object
		if(AppObjectUtil.isObjectInRecycle(sess, object))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//Check Status
		EIMUser user = object.getLockUser();
		if (user == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOREVISING", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOREVISING", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		// システム管理権限を保有するかを取得
		Boolean isSystemSecurityAuth = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.STATUS_UP);

		// システム管理権限をもたず、チェックアウトを行ったユーザ以外の場合、エラー
		if (!isSystemSecurityAuth && user.getId() != loginUser.getId())
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCHECKOUTUSER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//Version
		EIMVersion version = VersionUtils.getVersion(sess, object);
		// ステータスが編集中以外の場合はチェックアウト取消は出来ない
		// ただし、WFなしドキュメント(ステータスの値が0)は無条件でチェックアウト取消可能
		if( (version.getLatest().getStatus() != null)
			&& (version.getLatest().getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCUMENT.NOTEDITING");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCUMENT.NOTEDITING");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//Latest
		EIMObject latestObj = version.getLatest();
		
		// 直接編集でロックしている場合は取消不可
		if (latestObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG")) != null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCHECKOUTROLE", new Object[]{latestObj.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTROLE", new Object[]{latestObj.getName()});
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//UnLock
		object = ObjectUtils.unLock(sess, object);
		
		// 物理削除（属性の添付ファイルオブジェクトもまとめて削除）
		FormDomain formDomain = new FormDomain();
		formDomain.setId(latestObj.getId());
		DocumentFormService documentFormService = 
				(DocumentFormService)ApplicationContextLoader.getApplicationContext().getBean("documentFormService");
		List<FormDomain> formDomainList = new ArrayList<FormDomain>();
		formDomainList.add(formDomain);
		documentFormService.deleteTargetRevision(formDomainList);
		
		//属性表示色を削除
		DisplayColorUtil.deleteDisplayColorObject(sess, latestObj);
		
		//取消後、取消元を最新リビジョンにセットする
		VersionUtils.setLatestWithNoCheck(sess, object, true);
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.LOCKCANCEL");
		
		//パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		// SearchFramework 検索FW更新通知 対象：改訂ドキュメント・改訂元ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CANCEL_CHECK_OUT_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeDelete(latestObj.getId(), "SEARCHFW_CANCEL_CHECK_OUT_NEW_DOCUMENT");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CANCEL_CHECKOUT, 
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);

		//Commit
		sess.commit();
		if (isReturnObjectId) {
			out.println("<object objId=\"" + object.getId() + "\" cancelObjId=\"" + latestObj.getId() + "\">");
			out.println("</object");
		} else {
			out.println("<ok></ok>");
		}
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
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
