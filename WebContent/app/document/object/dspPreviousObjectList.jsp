<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "java.io.File" %>
<%@ page import = "java.util.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "org.apache.commons.logging.*" %>
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
	
	//Message
	String message = null;
	Object paramId = null;
	
	//Parameter
	String prmObjId[] = request.getParameterValues("objId");
	
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
		
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		// TransactionContextの作成、設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		// 対象オブジェクトID、対象オブジェクトの1つ前のオブジェクト
		Map<Long, EIMObject> previousMap = new HashMap<Long, EIMObject>();
		
		for (int i = 0; prmObjId.length > i; i++) {
			long objId = Long.parseLong(prmObjId[i]);
			EIMObject object = ObjectUtils.getObjectById(sess, objId);
			
			if (object != null) {
				// １つ前のリビジョンを取得する
				EIMVersion version = VersionUtils.getVersion(sess, object);
				int previousRev = (object.getRev() > 0 ? object.getRev() - 1 : object.getRev());
				EIMObject previousObject = version.getObjectByRev(previousRev);
				// 返却用マップに格納
				previousMap.put(objId, previousObject);
			}
		}
		out.println("<objList>");
		for (Map.Entry<Long, EIMObject> e : previousMap.entrySet()) {
			out.println("<object objId=\"" + e.getKey() + "\" previousObjId=\"" + e.getValue().getId() + "\" />");
		}
		out.println("</objList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), new Object[]{paramId}), eime);
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
