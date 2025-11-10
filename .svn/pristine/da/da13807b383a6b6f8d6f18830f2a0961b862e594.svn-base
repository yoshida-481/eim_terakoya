<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="java.util.*"%>

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
<%@ page import ="jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria"%>

<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain" %>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.FormListColumnService" %>

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
	String prmParentObjTypeId = request.getParameter("parentObjTypeId");
	String prmSrcObjTypeId = request.getParameter("srcObjTypeId");
	String prmNamespace = request.getParameter("namespace");
	String prmDefName = EIMUtils.getParameter(request, "defName");
	String[] prmFormListColumnIds = request.getParameterValues("formListColumnIds");

	//Message
	String message = null;
	Object[] paramId = {
			"parentObjTypeId=" + prmParentObjTypeId,
			"srcObjTypeId=" + prmSrcObjTypeId
			};

	try
	{
		// Session
		sess = EIMUtils.getSession(request);
		
		if(sess == null) {
			
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE)) {
			
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
		
		// 親オブジェクトタイプ
		EIMObjectType parentObjType = null;
		
		if(prmParentObjTypeId != null && !prmParentObjTypeId.equals("")) {
			
			parentObjType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmParentObjTypeId));
			
			if(parentObjType == null) {
				
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}

		// オブジェクトタイプ(レイアウト情報含む)Service
		ObjectTypeLayoutService objectTypeLayoutService =
			(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeLayoutService");
		
		// 複写元オブジェクトタイプ
		ObjectTypeLayoutDomain srcObjTypeLayoutDomain = null;
		
		if(prmSrcObjTypeId != null && !prmSrcObjTypeId.equals("")) {
			
			// 複写元オブジェクトタイプを取得
			srcObjTypeLayoutDomain = objectTypeLayoutService.getById(Long.parseLong(prmSrcObjTypeId));
			
			if(srcObjTypeLayoutDomain == null) {
				
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}

		// 他言語名称数を取得
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);

		// ネームスペース付きの定義名称を取得
		String defName = NamespaceUtil.concatenate(prmNamespace, prmDefName);
		
		// 複写するオブジェクトタイプ情報を生成
		ObjectTypeLayoutDomain destObjeTypeLayoutDomain = new ObjectTypeLayoutDomain();
		// 定義名称を設定
		destObjeTypeLayoutDomain.setDefinitionName(defName);
		// 他言語名称を設定
		List<OtherNameDomain> nameDomainList = new ArrayList<OtherNameDomain>();
		for(int i = 0; i < prmOtherCnt; i++) {
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			OtherNameDomain nameDomain = new OtherNameDomain();
			nameDomain.setLangId(prmOtherLId);
			nameDomain.setName(prmOtherName);
			destObjeTypeLayoutDomain.getNameList().add(nameDomain);
		}
		
		// オブジェクトタイプをレイアウト情報も含めて複写
		ObjectTypeLayoutDomain copyObjectTypeLayoutDomain =
			objectTypeLayoutService.copy(srcObjTypeLayoutDomain, destObjeTypeLayoutDomain);
		
		// 連続データ生成
		// 連続データ定義(デフォルトシーケンス)を取得
		SequenceTypeService sequenceTypeService =
			(SequenceTypeService)ApplicationContextLoader.getApplicationContext().getBean("sequenceTypeService2");
		SequenceTypeDomain sequenceTypeDomain =
			sequenceTypeService.getByDefinitionName(EIMConfig.get("SEQUENCE_TYPE_DEFINITION_NAME"));
		
		// 連続データ定義が存在しない場合
		if (sequenceTypeDomain == null) {
			
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEQTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEQTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		String prmParentSequence = EIMUtils.getParameter(request, "parentSequence");
		SequenceDomain seqDomain = new SequenceDomain();
		if(Boolean.valueOf(prmParentSequence)){
			//親クラスのシーケンス番号を参照する場合
			List<Long> parentObjectTypeIds = new ArrayList<Long>();
			long parentObjectTypeId = parentObjType.getId();
			ObjectTypeService objTypeService = (ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("adminObjectTypeService2");
			
			//「オブジェクトタイプ」ObjTypeを取得
			ObjectTypeDomain objTypeDomain = objTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
			long objectTypeObjTypeId = objTypeDomain.getId();
				
			if(objTypeDomain == null){
				//対象のオブジェクトタイプが存在しません。「オブジェクトタイプ」ObjTypeが取得できません。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECTTYPELAYOUT.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			//親クラスの「オブジェクトタイプ」objectを取得
			ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
			ObjectDomain objectDomain = objectService.getByTypeAndName(objTypeDomain, String.valueOf(parentObjType.getId()));
			
			if(objectDomain == null){
				//オブジェクトが取得できません。親クラスの「オブジェクトタイプ」Obectが取得できません。
				message = EIMResource.getMessage(sess, "EIM.ERROR.OBJECT.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.OBJECT.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
				
			//親クラスの連続ID属性を取得
			long sequenceId = objectDomain.getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID")).getLong();
			//親クラスのシーケンスドメインを取得
			SequenceService sequenceService = (SequenceService)ApplicationContextLoader.getApplicationContext().getBean("sequenceService2");
			seqDomain = sequenceService.getById(sequenceId);
			
			if(seqDomain == null){
				//連続データ情報が取得できません。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEQUENCE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEQUENCE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			
		}else{
			
			// 連続データを生成
			seqDomain.setSequenceType(sequenceTypeDomain);
			// 書式文字列を設定
			String formatString = EIMUtils.getParameter(request, "formatString");
			seqDomain.setFormatString(formatString);
			SequenceService sequenceService =
				(SequenceService)ApplicationContextLoader.getApplicationContext().getBean("sequenceService2");
			seqDomain = sequenceService.create(seqDomain);
		}
		
		// 親タイプから継承された属性タイプを除いた属性タイプ一覧を取得
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId(copyObjectTypeLayoutDomain.getId());
		attributeTypeCriteria.setIncludingParents(false);
		AttributeTypeService attributeTypeService =
			(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		List<AttributeTypeDomain> attTypeDomainList =
			attributeTypeService.getList(attributeTypeCriteria);
		// 親タイプから継承された属性タイプを除いた属性タイプ一覧を設定
		// ※親タイプから引き継いでいる属性タイプも設定してしまうと既に割当済みのエラーが発生するため
		copyObjectTypeLayoutDomain.setAttributeTypeList(attTypeDomainList);
		
		// 複写されたオブジェクトタイプを更新
		// ※objectTypeLayoutService.copyの仕様で連続データも強制的に複写されてしまうため
		//   生成した連続データIDを設定
		copyObjectTypeLayoutDomain.setSequence(seqDomain);
		ObjectTypeService objectTypeService =
			(ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("adminObjectTypeService2");
		objectTypeService.update(copyObjectTypeLayoutDomain);

		// 表示列情報取得
		FormListColumnService formListColumnService = (FormListColumnService)ApplicationContextLoader.getApplicationContext().getBean("formListColumnService");
		FormTypeDomain formType = new FormTypeDomain();
		formType.setId(Long.valueOf(prmSrcObjTypeId));
		formType = formListColumnService.getByFormType(formType);
		List<AttributeTypeLayoutDomain> formListColumn = formType.getFormListColumn().getSystemSettingFormListColumns();
		List<String> formListColumnIdList = new ArrayList<String>();
		for(AttributeTypeLayoutDomain formColumn : formListColumn){
			formListColumnIdList.add(formColumn.getFormListColumnId());
		}
		
		// 表示順を更新
		objectTypeLayoutService.sortAttributeType(copyObjectTypeLayoutDomain);
		
		// 複製フラグを更新
		objectTypeLayoutService.newCopyAttributeType(copyObjectTypeLayoutDomain);
		
		// 表示列更新
		if(formType.getAttributeTypeList() != null){
			long objTypeId = copyObjectTypeLayoutDomain.getId();
			formListColumnService.update(objTypeId, formListColumnIdList);
		}
		
		// 後続の処理(履歴生成、ワークフロー更新)をV4系に合わせるためコンバートをかける
		EIMObjectType copyObjType = ConvertUtils.toEIMObjectType(copyObjectTypeLayoutDomain);
		
		// ワークフロー更新
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(sess, srcObjTypeLayoutDomain.getWorkflow().getId());
		if(workFlow == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		WorkFlowUtils.applyWorkFlow(sess, copyObjType, workFlow);
		
		// 履歴生成
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.COPY_CLASS,
				EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, parentObjType,
				EIMConstant.TARGET_COPY, EIMConstant.OBJECT_TYPE, copyObjType, null);
		
		//Commit
		sess.commit();

		//XML
		out.println("<objType");
			out.println(" objTypeId=\"" + copyObjType.getId() + "\"");
		out.println(">");
		out.println("</objType>");
		
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
