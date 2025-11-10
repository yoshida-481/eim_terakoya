<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "java.util.*" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria"%>

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

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId,
			"attTypeId=" + Arrays.toString(prmAttTypeIds)
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
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
		
		// 検索条件
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		
		// 適用属性タイプID群
		List<Long> releaseAttTypeIdList = new ArrayList<Long>();
		
		// 適用属性タイプリスト
		List<EIMAttributeType> attTypeList = new ArrayList<EIMAttributeType>();

		// 検索条件設定
		for (String attTypeId : prmAttTypeIds) {
			
			releaseAttTypeIdList.add(Long.valueOf(attTypeId));
		}
		attributeTypeCriteria.setIds(releaseAttTypeIdList);
		
		// カスタム属性(レイアウト)含まない属性タイプ一覧を一括取得
		AttributeTypeService attributeTypeService = 
			(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		List<AttributeTypeDomain> attTypeDomainList = attributeTypeService.getList(attributeTypeCriteria);
		
		// 適用属性タイプのマップ(key：属性タイプID、value：属性タイプドメイン)
		Map<Long, EIMAttributeType> attTypeMap = new HashMap<Long, EIMAttributeType>();
		
		// 適用属性タイプのマップの生成 ＆ 後続の処理をV4系に合わせるためコンバート
		for (AttributeTypeDomain attTypeDomain : attTypeDomainList) {
			
			// コンバート
			EIMAttributeType eimAttType = ConvertUtils.toEIMAttributeType(attTypeDomain);
			
			// マッピング
			attTypeMap.put(new Long(eimAttType.getId()), eimAttType);
			
			// コンバート
			attTypeList.add(eimAttType);
		}
		
		// 適用属性タイプ数分処理
		for (Long attTypeId : releaseAttTypeIdList) {

			if(!attTypeMap.containsKey(attTypeId))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			//Release
			ObjectAttributeUtils.releaseAttributeType(sess, objType, attTypeMap.get(attTypeId));

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.RELEASE_CLASS_ATTRIBUTE,
					EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, objType,
					EIMConstant.TARGET_CHILD_ATTRIBUTE, EIMConstant.ATTRIBUTE_TYPE, attTypeMap.get(attTypeId), null);
		}
		
		// 表示列解除
		// オブジェクトタイプサービス取得
		ObjectTypeService objectTypeService = (ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("adminObjectTypeService2");
		// オブジェクトサービス取得
		ObjectService objectService  = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		
		// オブジェクトタイプ取得
		ObjectTypeDomain objectType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
		ObjectCriteria objectCriteria = new ObjectCriteria();
		objectCriteria.setObjectTypeId(objectType.getId());
		objectCriteria.setName(prmObjTypeId);
		objectCriteria.setSubClasses(false);
		List<ObjectDomain> objectList = objectService.getList(objectCriteria);
		
		// 表示列解除
		boolean isFormListColumnUpdate = false;
		if(objectList != null && objectList.size() > 0){
			AttributeDomain attribute = objectList.get(0).getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_FORM_LIST_COLUMN"));
			if(attribute != null){
				// 解除属性IDが表示列に含まれる場合、表示列から削除
				for(int i = 0; i < attribute.getStringList().size(); i++){
					for(int j = 0; j < prmAttTypeIds.length; j++){
						if(prmAttTypeIds[j].equals(attribute.getStringList().get(i))){
							attribute.getStringList().remove(i);
							i--;
							isFormListColumnUpdate = true;
						}
					}
				}
			}
		}
		
		// 更新
		if(isFormListColumnUpdate){
			objectService.update(objectList.get(0));
		}

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
