<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.util.List" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.service.DocumentObjectTypeLayoutService"%>
<%@ page import = "jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain"%>

<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>

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
	String prmObjTypeId = request.getParameter("objTypeId");
	String[] prmAttTypeIds = request.getParameterValues("attTypeId");
	String[] prmInheritanceFlags = request.getParameterValues("inheritanceFlag");
	String[] prmRelationFlags = request.getParameterValues("relationFlag");
	

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId,
			"attTypeId=" + prmAttTypeIds,
			"inheritanceFlag=" + prmInheritanceFlags,
			"relationFlag=" + prmRelationFlags
			};

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		
		if (sess == null) {
			
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if (!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE)) {
			
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		// TransactionContextの作成、設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if (objType == null) {
			
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 属性タイプ一覧Service
		AttributeTypeService attributeTypeService = 
			(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		
		// 検索条件
		AttributeTypeCriteria attTypeCriteria = new AttributeTypeCriteria();
		
		// 検索対象の属性タイプIDリスト
		List<Long> attTypeIdList = new ArrayList<Long>();
		
		// 選択した属性タイプ数分処理
		for (String attTypeId : prmAttTypeIds) {
			attTypeIdList.add(Long.parseLong(attTypeId));
		}
		
		// 検索条件を設定
		attTypeCriteria.setIds(attTypeIdList);
		// 属性タイプ一覧を一括取得
		List<AttributeTypeDomain> attTypeDomainList = attributeTypeService.getList(attTypeCriteria);
		
		// 取得件数が合致しない場合
		if(attTypeIdList.size() != attTypeDomainList.size()) {
			
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		// フラグを設定
		ObjectTypeLayoutDomain objTypeLayoutDomain = new ObjectTypeLayoutDomain();
		objTypeLayoutDomain.setId(new Long(objType.getId()));
		
		// クライアントで設定した属性タイプを表示順で設定
		DocumentAttributeTypeLayoutDomain attTypeLayoutDomain = null;
		int i = 0;
		for (String attTypeId : prmAttTypeIds) {
			boolean inheritanceFlag = Boolean.valueOf(prmInheritanceFlags[i]);
			boolean relationFlag = Boolean.valueOf(prmRelationFlags[i]);
			attTypeLayoutDomain = new DocumentAttributeTypeLayoutDomain();
			attTypeLayoutDomain.setId(Long.parseLong(attTypeId));
			attTypeLayoutDomain.setInheritanceFlag(inheritanceFlag);
			attTypeLayoutDomain.setRelationFlag(relationFlag);
			objTypeLayoutDomain.getAttributeLayoutList().add(attTypeLayoutDomain);
			i++;
		}
		
		// 引継ぎ、関連付けを更新
		DocumentObjectTypeLayoutService documentObjectTypeLayoutService = 
			(DocumentObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectTypeLayoutService");
		documentObjectTypeLayoutService.inheritanceAndRelationAttributeType(objTypeLayoutDomain);
		
		// 履歴生成
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.INHERITANCE_AND_RELATION_CLASS_ATTRIBUTE,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType,
				null, null, null, null);

		//Commit
		sess.commit();

		out.println("<OK></OK>");
	}
	catch(eim.bo.EIMException eime)
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
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime2)
	{
		out.clear();
		message = eime2.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime2.getMessage(), paramId), eime2);
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
