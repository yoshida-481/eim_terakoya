<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.OperationHistoryDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.OperationHistoryService"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.util.ConfigUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum"%>

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
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmSecId = request.getParameter("secId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"secId=" + prmSecId
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
		user = (EIMUser)sess.getAttribute("USER");
		
		// ThreadLocalコンテキストにセッションを設定
		if(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession() == null)
		{
			jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		// システム管理のアプリケーション種別を取得
		String appId = (String)session.getAttribute("ADMIN_APP_ID");
		boolean isForm = AppConstant.ADMIN_APP_ID_FORM.equals(appId);

		// アプリケーション種別が帳票以外の場合、objectTypeにセキュリティを設定
		if(!isForm)
		{
			//ObjectType
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjId));
			
			if(objType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		
			//適用Security情報取得
			EIMSecurity sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
			
			SecurityUtils.setSecurityObjType(sess, objType, sec);
	
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.APPLY_SECURITY_SYSTEM, 
					EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType,
					EIMConstant.TARGET_APPLIED_SECURITY, EIMConstant.SECURITY, sec, null);
		}
		// アプリケーション種別が帳票の場合、objectにセキュリティを設定
		else
		{
			
			TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(context);
			context.setLangId(sess.getLangId());
			context.setDBConnection(sess.getDBConnection());
			context.setUser(ConvertUtils.toUserDomain(user));

 			ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
			ObjectTypeService objectTypeService = (ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeService2");
			AttributeTypeService attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
			OperationHistoryService operationHistoryService = (OperationHistoryService)ApplicationContextLoader.getApplicationContext().getBean("operationHistoryService2");


			// システム管理（帳票）の場合、帳票タイプフォルダobjectを取得する
			ObjectDomain obj = objectService.getById(Long.parseLong(prmObjId));
			EIMObject eimObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(obj == null)
			{
				// エラーメッセージ表示
				message = EIMResource.getMessage(sess, "EIM.ERROR.OBJECT.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.OBJECT.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
				return;
			}
			
			// 適用Security情報取得
			EIMSecurity sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
			// EIMSecurityをsecurityDomainにコンバート
			SecurityDomain securityDomain = ConvertUtils.toSecurityDomain(sec);
 			obj.setSecurity(securityDomain);
			
			// 帳票タイプフォルダのセキュリティ更新処理
			objectService.update(obj);
			
			// 帳票タイプフォルダ配下の帳票一覧を取得
			// 検索条件作成
			SearchSelectObject selectObject = new SearchSelectObject();
			SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
			SearchConditionGroup conditionGroup = helper.group(helper.opAnd());
			
			// オブジェクトタイプ指定(オブジェクトタイプ：帳票タイプフォルダの属性「帳票タイプフォルダID」)
			AttributeDomain attr = obj.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_FORM_TYPE_ID"));
			conditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, attr.getLong()));

			// 属性値指定(属性：帳票タイプフォルダID)
			AttributeTypeDomain attrTypeCond2 = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID"));
			ValueTypeEnum valueTypeCond2 = ValueTypeEnum.getByValue(1);
			attrTypeCond2.setValueType(valueTypeCond2);
			long formTypeFolderId = obj.getId();
			conditionGroup.addCondition(helper.eq(helper.opAnd(), attrTypeCond2, obj.getId()));

			// 検索実行
			selectObject.setCondition(conditionGroup);
			List<ObjectDomain> objList = objectService.getList(selectObject, null);
			// 帳票タイプフォルダ配下の帳票を帳票タイプフォルダと同じセキュリティに更新
			if(objList.size() > 0)
			{
				for(ObjectDomain objectDomain : objList)
				{
					// SearchFramework 検索FW更新通知 対象：帳票
					AppUpdateNoticeUtils.updateNoticeInsert(objectDomain.getId(), "SEARCHFW_UPDATE_FORM_SECURITY");

					// 帳票にセキュリティを設定
					objectDomain.setSecurity(securityDomain);
					// 帳票を更新
					objectService.update(objectDomain);
				}
			}

			// 操作履歴出力
			OperationHistoryDomain operationHistoryDomain = new OperationHistoryDomain();
			operationHistoryDomain.setApplicationTypeId(Long.parseLong(AppConstant.SYSTEM));
			operationHistoryDomain.setOperationTypeId(Long.parseLong(EIMConstant.APPLY_SECURITY_SYSTEM));
			
			ObjectDomain recordObjectA = new ObjectDomain();
			recordObjectA.setId(obj.getId());
			operationHistoryDomain.setRecordObjectA(recordObjectA);
			operationHistoryDomain.setRecordInfoIdA(Long.parseLong(EIMConstant.TARGET_UPDATE));
			
			SecurityDomain recordObjectB = new SecurityDomain();
			recordObjectB.setId(securityDomain.getId());
			operationHistoryDomain.setRecordObjectB(recordObjectB);
			operationHistoryDomain.setRecordInfoIdB(Long.parseLong(EIMConstant.TARGET_APPLIED_SECURITY));
			
			operationHistoryDomain = operationHistoryService.create(operationHistoryDomain);

		}
	
		//Commit
		sess.commit();

		out.println("<ok/>");
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
				jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.removeEIMSession();
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