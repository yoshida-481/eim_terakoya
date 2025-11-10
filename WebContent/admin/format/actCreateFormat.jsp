<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.FormatService" %>

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
	int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
	String prmDefName = LanguageFieldUtil.getDefName(request, prmOtherCnt);
	String prmPath = EIMUtils.getParameter(request, "path");
	String prmBoxFolderId = EIMUtils.getParameter(request, "boxFolderId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"otherCnt=" + prmOtherCnt,
			"path=" + prmPath,
			"boxFolderId=" + prmBoxFolderId
			};

	try
	{
		/*
		 * param check
		 */
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_FORMAT))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		/*
		 * Create Object Type
		 */
		//Create Format
		FormatService formatService = (FormatService)ApplicationContextLoader.getApplicationContext().getBean("formatService2");
		FormatDomain format = new FormatDomain();
		format.setDefinitionName(prmDefName);
		format.setBoxFolderId(prmBoxFolderId);

		DirectoryDomain directory = new DirectoryDomain();
		directory.setPath(prmPath);
		directory.setOnline(true);
		List<DirectoryDomain> dirList = new ArrayList();
		dirList.add(directory);
		format.setDirectoryList(dirList);

		format = formatService.create(format); // 他言語名称は後続処理で設定する
		
		/*
		 * Create Object Type Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);
			
			// Create Object Type
			FileUtils.addOtherFormatName(sess, format.getId(), prmOtherLId, prmOtherName);
		}
		
		// Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_FORMAT, 
				EIMConstant.TARGET_CREATE, EIMConstant.FORMAT, ConvertUtils.toEIMFormat(format),
				null, null, null, null);

		// Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_DIRECTORY, 
				EIMConstant.TARGET_PARENT_FORMAT, EIMConstant.FORMAT, ConvertUtils.toEIMFormat(format),
				EIMConstant.TARGET_CREATE, EIMConstant.DIRECTORY, ConvertUtils.toEIMDirectory(format.getDirectoryList().get(0)), null );
		
		//Commit
		sess.commit();
		
		//XML
		out.println("<format");
			out.println(" formatId=\"" + format.getId() + "\"");
			out.println(" formatName=\"" + StringUtils.xmlEncode(format.getName()) + "\"");
			out.println(" path=\"" + StringUtils.xmlEncode(directory.getPath()) + "\"");
			out.println(" boxFolderId=\"" + StringUtils.xmlEncode(format.getBoxFolderId()) + "\"");
			out.println(">");
		out.println("</format>");
		
	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
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
			EIMThreadContext.removeAll();
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
