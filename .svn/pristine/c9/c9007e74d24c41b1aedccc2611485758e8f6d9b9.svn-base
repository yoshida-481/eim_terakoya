<%@page contentType="text/xml; charset=UTF-8"%>
<%@page import="app.document.object.UpdateObjectLink"%>
<%@page import="eim.bo.*"%>
<%@page import="eim.net.*"%>
<%@page import="eim.util.*"%>
<%@page import="common.util.*"%>
<%@page import="java.util.*"%>
<%@page import="org.apache.commons.logging.*"%>

<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@page import="jp.co.ctc_g.eim.app.document.business.service.CirculationSearchService"%>
<%@page import="jp.co.ctc_g.eim.app.document.business.domain.CirculationSearchDomain"%>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//***********
	//          *
	//   検索   *
	//          *
	//***********

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutflg = false;

	//Message
	String message = null;
	Object[] paramId = {
			"targetDocType=" + EIMUtils.getParameter(request, "targetDocType"),
			"userId=" + EIMUtils.getParameter(request, "userId"),
			"status=" + EIMUtils.getParameter(request, "status"),
			"searchRangeStartDate=" + EIMUtils.getParameter(request, "searchRangeStartDate"),
			"searchRangeEndDate=" + EIMUtils.getParameter(request, "searchRangeEndDate"),
			"pathCondition=" + EIMUtils.getParameter(request, "pathCondition"),
			"searchPath=" + EIMUtils.getParameter(request, "searchPath"),
			"objectName=" + EIMUtils.getParameter(request, "objectName"),
			};
	try {
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		// セッション取得、設定
		if (EIMThreadContext.getEIMSession() == null) {
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}

		user = (EIMUser)sess.getAttribute("USER");

		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		CirculationSearchService circulationSearchService = (CirculationSearchService)ApplicationContextLoader.getApplicationContext().getBean("circulationSearchService");

		//Search
		List<CirculationSearchDomain> result = circulationSearchService.search(request);

		//Root Node
		out.println("<objList>");

		for (int i = 0; i < result.size(); i++) {
			//Object
			CirculationSearchDomain object = result.get(i);
			
			//XML
			out.println("<object");
			out.println(" objId=\"" + object.getObjId() + "\"");
			out.println(" objTypeId=\"" + object.getObjTypeId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getObjName()) + "\"");
			out.println(" number=\"" + StringUtils.xmlEncode(object.getNumber()) + "\"");
			out.println(" isWFFolder=\"" + object.getIsWFFolder() + "\"");
			out.println(" isFolder=\"" + object.getIsFolder() + "\"");
			out.println(" isDocument=\"" + object.getIsDocument() + "\"");
			out.println(" isFullPath=\"" + object.getIsFullPath() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getObjTypeName()) + "\"");
			out.println(" rev=\"" + object.getRev() + "\"");
			out.println(" isPDFJoinFailed=\"" + object.getIsPDFJoinFailed() + "\"");
			out.println(" statusId=\"" + object.getStatusId() + "\"");
			out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatusTypeName()) + "\"");
			out.println(" statusTypeKind=\"" + object.getStatusTypeKind() + "\"");
			out.println(" readOnly=\"" + object.getIsReadOnly() + "\"");
			out.println(" isPublished=\"" + object.getIsPublished() + "\"");
			out.println(" isProcessingPublic=\"" + String.valueOf(object.getStatusTypeKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) + "\"");
			out.println(" isDspPubIconForNoWF=\"false\""); //本検索は必ずWFつきであり、ステータス情報を得られるため
			out.println(" isNoWFPublic=\"" + object.getIsNoWFPublic() + "\""); //OCR処理で必要
			out.println(" requestUser=\"" + StringUtils.xmlEncode(object.getRequestUser()) + "\"");
			out.println(" requestDate=\"" + StringUtils.xmlEncode(object.getRequestDateStr()) + "\"");
			out.println(" nextApprover=\"" + StringUtils.xmlEncode(object.getNextApprover()) + "\"");
			out.println(" path=\"" + StringUtils.xmlEncode(object.getPath()) + "\"");
			out.println(" ocrProcessStatus=\"" + object.getOcrProcessStatus() + "\"");
			out.println(" ocrResultStatus=\"" + object.getOcrResultStatus() + "\"");
			out.println(" expiration=\"" + object.getExpiration() + "\"");
			out.println(" lockUserName=\"" + StringUtils.xmlEncode(object.getLockUserName()) + "\"");
			out.println(" isDocumentLink=\"false\"");
			out.println(" isDspPdfIcon=\"" + StringUtils.xmlEncode(object.getIsDspPdfIcon()) + "\"");
			out.println(" pdfConversionStatus=\"" + object.getPdfConversionStatus() + "\"");
			out.println(" isOldVer=\"" + StringUtils.xmlEncode(object.getIsOldVer()) + "\"");
			out.println(" isPdfPreRegistered=\"" + object.getIsPdfPreRegistered() + "\"");
			out.println(">");
			out.println("</object>");
		}
		//End Root Node
		out.println("</objList>");
	}
	catch (EIMException eime) {
		out.clear();

		//上限オーバーの場合は、メッセージを付け替え
		if (eime.getResultCode() == SearchUtils.EIM_ERROR_LOGIC_SEARCH_RESULT_LIMIT_OVER) {
			message = EIMResource.getMessage(
					sess,"EIM.WARN.LOGIC.DOCSEARCH.RESULT.LIMIT.OVER"
					,new Object[]{eime.getMessageParams()[1].toString(),eime.getMessageParams()[0].toString()}
					);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
		} else {
			message = eime.getMessage();
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
		}
	}
	catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally {
		try {
			if (sessPutflg) {
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if (sess != null) {
				sess.close();
			}
			
			if (jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null) {
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