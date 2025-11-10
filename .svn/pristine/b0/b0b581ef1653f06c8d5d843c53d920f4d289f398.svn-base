<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.AttributeTypeLayoutService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>

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

	//Parameter
	String prmAttTypeId = EIMUtils.getParameter(request, "attTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId
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

		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIBUTE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(prmAttTypeId));
		if(attType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Get Attribute Value Master
		AttributeValueMaster attValMst = null;
		if(sess.getAttribute("ADMIN_APP_ID").equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
		}

		//Delete Attribute Value Master
		if (attValMst != null) {
			AttributeMasterUtil.deleteAttributeValueMaster(sess, attValMst);
		}

		// Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_ATTRIBUTE,
				EIMConstant.TARGET_DELETE, EIMConstant.ATTRIBUTE_TYPE, attType,
				null, null, null, null);

		String appId = sess.getAttribute("ADMIN_APP_ID").toString();

		if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			if(EIMThreadContext.getTransactionContext() != null)
			{
				EIMThreadContext.removeTransactionContext();
			}

			TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(context);
			context.setLangId(sess.getLangId());
			context.setDBConnection(sess.getDBConnection());
			context.setUser(ConvertUtils.toUserDomain(loginUser));

			//ドキュメント管理用・帳票用システム管理ではレイアウト情報を合わせて削除する
			AttributeTypeLayoutService attributeTypeLayoutService =
				(AttributeTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeLayoutService");
			AttributeTypeLayoutDomain attributeTypeLayoutDomain = attributeTypeLayoutService.getById(attType.getId());
			attributeTypeLayoutService.delete(attributeTypeLayoutDomain);

		} else {

			//汎用システム管理では属性タイプのみを削除する
			AttributeUtils.deleteAttributeType(sess, attType);

		}

		//Commit
		sess.commit();
		out.println("<OK></OK>");
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
			if(EIMThreadContext.getTransactionContext() != null){
				EIMThreadContext.removeTransactionContext();
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
