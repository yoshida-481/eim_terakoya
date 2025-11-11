<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectTypeFormatCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeFormatDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
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
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria"%>

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
	String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");
	String prmParentObjTypeId = EIMUtils.getParameter(request, "parentObjTypeId");
	String prmDefName = EIMUtils.getParameter(request, "defName");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId,
			"parentObjTypeId=" + prmParentObjTypeId
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
		if(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession() == null)
		{
			jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
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
		
		//Object Type check
		EIMObjectType objType = null;
		ObjectTypeService objectTypeService = null;
		ObjectTypeDomain objectTypeDomain = null;
		
		// 汎用管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
			
		// 帳票管理 or 文書管理の場合
		} else {
			
			// オブジェクトタイプを取得
			objectTypeService = 
				(ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("adminObjectTypeService2");
			objectTypeDomain = 
				objectTypeService.getById(Long.parseLong(prmObjTypeId));
			
			// 後続の処理をV4系に合わせるためコンバートをかける
			if (objectTypeDomain != null) {
				
				objType = ConvertUtils.toEIMObjectType(objectTypeDomain);
			}
		}
		
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Parent Object Type check
		EIMObjectType parentObjType = null;
		if(prmParentObjTypeId != null && !prmParentObjTypeId.equals(""))
		{
			parentObjType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmParentObjTypeId));
			if(parentObjType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}
		
		//Get old info
		EIMObjectType oldParentObjType = objType.getParent();
		
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
		
		//ネームスペース付きの定義名称を取得
		String defName = LanguageFieldUtil.getDefName(sess, prmDefName, request, prmOtherCnt);

		//Update
		objType = ObjectUtils.updateObjectType(sess, objType, defName, parentObjType);
		
		/*
		 * Update Object Type Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			//Update
			ObjectUtils.updateOtherObjectTypeName(sess, objType.getId(), prmOtherLId, prmOtherName);
		}
		
		// 番号自動生成
		String prmNumberAutoCreate = EIMUtils.getParameter(request, "numberAutoCreate");
		boolean isNumberAutoCreate = prmNumberAutoCreate != null ? Boolean.valueOf(prmNumberAutoCreate) : false;
		
		// 帳票管理 or 文書管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)
			|| (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT) && isNumberAutoCreate)
		){
			
			// 更新前の連続データを取得
			SequenceService sequenceService = 
				(SequenceService)ApplicationContextLoader.getApplicationContext().getBean("sequenceService2");
			SequenceDomain beforeSeqDomain = sequenceService.getById(objectTypeDomain.getSequence().getId());
			
			// 付加文字列の更新判定
			String newFormatString = EIMUtils.getParameter(request, "formatString");
			
			// 付加文字列の更新フラグ
			boolean updateFormatStringFlag = false;
			
			// 更新前の付加文字列に何も設定されていない
			if (beforeSeqDomain.getFormatString() == null
					|| beforeSeqDomain.getFormatString().length() == 0) {
				
				// 付加文字列に更新値有り
				if (newFormatString.length() > 0) {
					
					// 付加文字列の更新フラグON
					updateFormatStringFlag = true;
				}
				
			// 更新前の付加文字列と違う場合
			} else if (!beforeSeqDomain.getFormatString().equals(newFormatString)) {
				// 付加文字列の更新ON
				updateFormatStringFlag = true;
			}
			
			// 連続データの付加文字列(書式文字列)を更新(再生成)
			// updateメソッドは存在しないため、create→delete
			if (updateFormatStringFlag) {
				
				// 連続データ設定値を設定
				SequenceDomain afterSeqDomain = new SequenceDomain();
				afterSeqDomain.setCurrentCycle(beforeSeqDomain.getCurrentCycle());
				afterSeqDomain.setCurrentValue(beforeSeqDomain.getCurrentValue());
				afterSeqDomain.setSequenceType(beforeSeqDomain.getSequenceType());
				afterSeqDomain.setFormatString(newFormatString);
				// 連続データを再生成
				afterSeqDomain = sequenceService.create(afterSeqDomain);
				
				
				//シーケンスを共有しているクラスのオブジェクトタイプObjectの連続データ属性を更新
				AttributeTypeService attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
				//該当クラスの更新前シーケンスIDを取得
				long boforeSequenceId = beforeSeqDomain.getId();
				//オブジェクトタイプObjectTypeを取得
				ObjectTypeDomain objTypeDomain = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
				//連続データ属性タイプを取得
				AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID"));
				//シーケンスを共有しているオブジェクトタイプObjectの検索条件を生成
				SearchSelectObject searchSelectObject = new SearchSelectObject();
				SearchSelectObject.SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
				searchSelectObject.setCondition(helper.group(helper.opAnd()));
				//objTypeを指定
				searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd()).addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, objTypeDomain.getId()) ) );
				//連続データ属性IDを指定
				searchSelectObject.getCondition().addCondition(helper.group(helper.opAnd()).addCondition(helper.eq(helper.opAnd(), attributeTypeDomain, boforeSequenceId)));
				
				//検索上限設定
				SearchLimitCountCondition limitCond = new SearchLimitCountCondition(-1, true);
				ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
				//object検索
				List<ObjectDomain> beforeObjectDomainList = objectService.getList(searchSelectObject,limitCond);
				
				//該当クラスの更新前と同じシーケンスを使用しているオブジェクトタイプObjectIdを取得
				long targetObjectTypeObjId = objectTypeDomain.getObjectTypeObject().getId();
				Iterator ite = beforeObjectDomainList.iterator();
				while(ite.hasNext())
				{
					ObjectDomain resultObjectDomain = (ObjectDomain)ite.next();
					if(targetObjectTypeObjId != resultObjectDomain.getId()){
						//更新後のシーケンスIDを連続データ属性に設定し、更新する
						objectService.setAttributeSingleLong(resultObjectDomain,attributeTypeDomain,afterSeqDomain.getId());
					}
				}
				
				// 更新対象のオブジェクトタイプ取得
				objectTypeDomain = objectTypeService.getById(new Long(objType.getId()));
				
				// 親タイプから継承された属性タイプを除いた属性タイプ一覧を取得
				AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
				attributeTypeCriteria.setObjectTypeId(objectTypeDomain.getId());
				attributeTypeCriteria.setIncludingParents(false);
				List<AttributeTypeDomain> attTypeDomainList = attributeTypeService.getList(attributeTypeCriteria);
				// 親タイプから継承された属性タイプを除いた属性タイプ一覧を設定
				// ※親タイプから引き継いでいる属性タイプも設定してしまうと既に割当済みのエラーが発生するため
				objectTypeDomain.setAttributeTypeList(attTypeDomainList);
				
				// 引き継ぎ無しでフォーマットを取得する
				// ※ObjectTypeService#update()でフォーマットの割当更新が実行されるが、
				// 新旧比較の際、旧フォーマットとして親からの引き継ぎなしを取得して新(親からの引き継ぎ有り)と比較するため、
				// 差異が発生してしまいフォーマットのエラーが発生するため。
				ObjectTypeFormatCriteria objectTypeFormatCriteria = new ObjectTypeFormatCriteria();
				objectTypeFormatCriteria.setObjectTypeId(objectTypeDomain.getId());
				objectTypeFormatCriteria.setIncludingParents(false);
				List<ObjectTypeFormatDomain> objectTypeFormatList = objectTypeService.getObjectTypeFormatList(objectTypeFormatCriteria);
				objectTypeDomain.setObjectTypeFormatList(objectTypeFormatList);
				
				// 連続データ情報を設定
				objectTypeDomain.setSequence(afterSeqDomain);
				// オブジェクトタイプ更新(再生成した連続データを設定)
				objectTypeService.update(objectTypeDomain);
				// 更新前の連続データを削除
				sequenceService.delete(beforeSeqDomain);
			}
		}
		
		// SearchFramework 検索FW更新通知 対象：オブジェクトタイプ + 配下のオブジェクトタイプ
		AppUpdateNoticeUtils.updateNoticeInsert(objType.getId(), "SEARCHFW_OBJTYPE_EDIT_OBJTYPE");
		AppUpdateNoticeUtils.updateNoticeInsertObjTypeChild(sess, objType, "SEARCHFW_OBJTYPE_EDIT_CHILD_OBJTYPE");

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_CLASS,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType,
				null, null, null, null);

		if(parentObjType != null)
		{
			if(oldParentObjType != null)
			{
				//Change parent class
				if(oldParentObjType.getId() != parentObjType.getId())
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_CLASS,
							EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, parentObjType,
							EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType, null);
				}
			}
			//Add parent class
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_CLASS,
						EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, parentObjType,
						EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType, null);
			}
		}
		else
		{
			//Delete parent class
			if(oldParentObjType != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_CLASS,
						EIMConstant.TARGET_PARENT_CLASS, null, null,
						EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT_TYPE, objType, null);
			}
		}

		//XML
		out.println("<objType");
			out.println(" objTypeId=\"" + objType.getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
			if(objType.getParent() != null)
			{
				out.println(" parentObjTypeId=\"" + objType.getParent().getId() + "\"");
				out.println(" parentObjTypeName=\"" + StringUtils.xmlEncode(objType.getParent().getName()) + "\"");
			}
		out.println(">");
		out.println("</objType>");

		//Commit
		sess.commit();

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
			if(sessPutFlag
				&& jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession() != null) {
				jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.removeEIMSession();
			}
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
