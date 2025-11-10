<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.ObjectDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.ObjectTypeDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.dao.ObjectTypeDao"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "java.util.*"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SequenceService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SequenceDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>

<%@ page import=" jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@ page import= "jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@ page import=" jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
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

	//Message
	String message = null;
	Object[] paramId = {
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

		// システム管理アプリケーション種別を取得
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");
		
		/*
		 * Create Object Type(Default)
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
		
		String prmDefName = EIMUtils.getParameter(request, "defName");
		
		String prmParentSequence = EIMUtils.getParameter(request, "parentSequence");
		
		// 番号自動生成
		String prmNumberAutoCreate = EIMUtils.getParameter(request, "numberAutoCreate");
		boolean isNumberAutoCreate = prmNumberAutoCreate != null ? Boolean.valueOf(prmNumberAutoCreate) : false;
		
		//ネームスペース付きの定義名称を取得
		String defName = LanguageFieldUtil.getDefName(sess, prmDefName, request, prmOtherCnt);

		EIMObjectType objType = null;

		// 帳票管理用システム管理、または文書管理用システム管理の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)
			|| (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT) && (Boolean.valueOf(prmParentSequence) || isNumberAutoCreate))
		){
		
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
				AttributeDomain seqIdAttr = objectDomain.getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID"));
				if(seqIdAttr == null){
					//連続データ情報が取得できません。
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEQUENCE.NOTFOUND");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEQUENCE.NOTFOUND");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					return;
				}
				long sequenceId = objectDomain.getAttribute(EIMConfig.get("ATTRIBUTE_TYPE_NAME_SEQUENCE_ID")).getLong();
				//親クラスのシーケンスドメインを取得
				SequenceService sequenceService = 
					(SequenceService)ApplicationContextLoader.getApplicationContext().getBean("sequenceService2");
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
				
				// 連続データを生成
				seqDomain.setSequenceType(sequenceTypeDomain);
				// 付加文字列を設定
				String formatString = EIMUtils.getParameter(request, "formatString");
				seqDomain.setFormatString(formatString);
				
				SequenceService sequenceService = 
					(SequenceService)ApplicationContextLoader.getApplicationContext().getBean("sequenceService2");
				seqDomain = sequenceService.create(seqDomain);
			}
			
			// 直上の親クラスの属性タイプの表示順を取得
			ObjectTypeLayoutService objectTypeLayoutService = 
				(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeLayoutService");
			ObjectTypeLayoutDomain parentLayoutDomain = 
				objectTypeLayoutService.getById(new Long(parentObjType.getId()));
			
			// 生成するオブジェクトタイプ情報を設定
			ObjectTypeLayoutDomain newObjectLayoutDomain = new ObjectTypeLayoutDomain();
			
			// 親オブジェクトタイプ設定
			newObjectLayoutDomain.setParent(parentLayoutDomain);
			// 連続データを設定
			newObjectLayoutDomain.setSequence(seqDomain);
			// 定義名称を設定
			newObjectLayoutDomain.setDefinitionName(defName);
			// 他言語名称を設定
			for(int i = 0; i < prmOtherCnt; i++) {
				String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);
				String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
				OtherNameDomain nameDomain = new OtherNameDomain();
				nameDomain.setLangId(prmOtherLId);
				nameDomain.setName(prmOtherName);
				newObjectLayoutDomain.getNameList().add(nameDomain);
			}
			
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
				// 直上の親クラスで表示順もしくは複製設定が設定されている属性タイプを設定
				for (AttributeTypeLayoutDomain attTypeLayout : parentLayoutDomain.getAttributeLayoutList()) {
					
					if( (attTypeLayout.isOrderSetFlag() == true) || (attTypeLayout.getNewCopyFlag() == true) ){
						// 表示順もしくは複製設定が設定されている属性タイプを設定
						newObjectLayoutDomain.getAttributeLayoutList().add(attTypeLayout);
					}
				}
			}
			
			// オブジェクトタイプ生成
			newObjectLayoutDomain = objectTypeLayoutService.create(newObjectLayoutDomain);
			
			// 後続の処理をV4系に合わせるためコンバートをかける
			objType = ConvertUtils.toEIMObjectType(newObjectLayoutDomain);
			
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
				// 表示列情報引継
				FormListColumnService formListColumnService = (FormListColumnService)ApplicationContextLoader.getApplicationContext().getBean("formListColumnService");
				if(newObjectLayoutDomain.getParent() != null){
					formListColumnService.inherit(newObjectLayoutDomain.getParent().getId() , newObjectLayoutDomain.getId());
				}
			}

		} else {
			
			objType = ObjectUtils.createObjectType(sess, defName, parentObjType);
			
			/*
			 * Create Object Type Other
			 */
			for(int i = 0; i < prmOtherCnt; i++)
			{
				String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
				String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

				// Create Object Type
				ObjectUtils.addOtherObjectTypeName(sess, objType.getId(), prmOtherLId, prmOtherName);
			}
		}
		
		//Create Operation History
		if(parentObjType != null)
		{
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_CLASS,
					EIMConstant.TARGET_PARENT_CLASS, EIMConstant.OBJECT_TYPE, parentObjType,
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, objType, null);
		}
		else
		{
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_CLASS,
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, objType,
					null, null, null, null);
		}

		//Commit
		sess.commit();

		//XML
		out.println("<objType");
			out.println(" objTypeId=\"" + objType.getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
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
