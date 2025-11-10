
<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>

<%@page import="jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>

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
			"attTypeId=" + prmAttTypeIds
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
		
		// システム管理アプリケーション種別を取得
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");
		
		// 検索条件
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		
		// 適用属性タイプID群
		List<Long> applyAttTypeIdList = new ArrayList<Long>();
		
		// 適用属性タイプリスト
		List<EIMAttributeType> attTypeList = new ArrayList<EIMAttributeType>();

		// 検索条件設定
		for (String attTypeId : prmAttTypeIds) {
			
			applyAttTypeIdList.add(Long.valueOf(attTypeId));
		}
		attributeTypeCriteria.setIds(applyAttTypeIdList);
		
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
		for (Long attTypeId : applyAttTypeIdList) {
			
			// 属性タイプが取得できなかった場合
			if(!attTypeMap.containsKey(attTypeId))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			//Apply
			ObjectAttributeUtils.applyAttributeType(sess, objType, attTypeMap.get(attTypeId));

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.APPLY_CLASS_ATTRIBUTE,
					EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, objType,
					EIMConstant.TARGET_CHILD_ATTRIBUTE, EIMConstant.ATTRIBUTE_TYPE, attTypeMap.get(attTypeId), null);
		}
		
		// 帳票管理の場合
		// ※以下の現象を発生させないため、条件に合致した場合は表示順を更新する
		// 条件：既に表示順が設定された属性タイプを適用した場合
		// 詳細：クラス移動して親が変更された際は継承により割り当て属性も変更されるが、表示順は何もしない方針。
		//       そうした場合、移動前の属性が割り当てられると表示順が残っているため（ごみデータ）、
		//       勝手に表示順が設定されたようにみえてしまう。
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			
			// 表示順が設定された適用属性タイプ情報を取得
			ObjectTypeLayoutService objectTypeLayoutService = 
				(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeLayoutService");
			ObjectTypeLayoutDomain objectTypeLayout = objectTypeLayoutService.getById(new  Long(objType.getId()));
			
			// 表示順更新フラグ
			boolean sortFlag = false;
			// 更新情報
			List<AttributeTypeLayoutDomain> sortAttTypeLayoutList = new ArrayList<AttributeTypeLayoutDomain>();
			// 表示順の設定された属性タイプ分繰り返す
			for (AttributeTypeLayoutDomain attTypeLayout : objectTypeLayout.getAttributeLayoutList()) {
				
				// 既に適用属性タイプが表示順が設定されている場合
				if (attTypeMap.containsKey(new Long(attTypeLayout.getId()))
						&& attTypeLayout.isOrderSetFlag()) {
					sortFlag = true;
					continue;
				}
				
				// 更新情報に追加
				sortAttTypeLayoutList.add(attTypeLayout);
			}
			
			// 表示順更新
			if (sortFlag) {
				objectTypeLayout.setAttributeLayoutList(sortAttTypeLayoutList);
				objectTypeLayoutService.sortAttributeType(objectTypeLayout);
			}
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
