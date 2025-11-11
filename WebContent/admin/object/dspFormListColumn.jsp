<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.impl.FormListColumnServiceImpl"%>
<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import	= "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.FormListColumnService" %>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain" %>

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
	String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
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
		
		// 表示列情報取得
		FormListColumnService formListColumnService = (FormListColumnService)ApplicationContextLoader.getApplicationContext().getBean("formListColumnService");
		FormTypeDomain formType = new FormTypeDomain();
		formType.setId(Long.valueOf(prmObjTypeId));
		formType = formListColumnService.getByFormType(formType);
		
		// 表示列情報XML生成
		if(formType != null){
			// デフォルト表示列要素生成
			String nameColumnXml		= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_ID + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.ID") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.ID") + "\" />";
			String titleColumnXml		= "<column id=\""				+ AppConstant.FORM_LIST_COLUMN_TITLE + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.TITLE") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.TITLE") + "\" />";
			String statusColumnXml		= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_STATUS + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.STATUS") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.STATUS") + "\" />";
			String creationUserXml		= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_CREATION_USER + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.CREATION.USER") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.CREATION.USER") + "\" />";
			String creationDateXml		= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_CREATION_DATE + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.CREATION.DATE") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.CREATION.DATE") + "\" />";
			String modificationUserXml	= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_MODIFICATION_USER + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.MODIFICATION.USER") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.MODIFICATION.USER") + "\" />";
			String modificationDateXml	= "<column id=\"" 				+ AppConstant.FORM_LIST_COLUMN_MODIFICATION_DATE + "\""
												+ " name = \"" 			+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.MODIFICATION.DATE") + "\" "
												+ " definitionName=\""	+ EIMResource.getMessageValue(sess,"EIM.LABEL.FORM.LIST.COLUMN.MODIFICATION.DATE") + "\" />";
			
			// Root Node
			out.println("<formListColumnInfo>");
			
			// デフォルト表示列
			out.println("<defaultFormListColumns>");
			{
				out.println(nameColumnXml);
				out.println(titleColumnXml);
				out.println(statusColumnXml);
				out.println(creationUserXml);
				out.println(creationDateXml);
				out.println(modificationUserXml);
				out.println(modificationDateXml);
			}
			out.println("</defaultFormListColumns>");
			
			// 属性表示列
			out.println("<attributeFormListColumns>");
			{
				// 属性表示列※タイトル属性はデフォルト表示列に含むため除外する。
				if(formType.getAttributeTypeList() != null){
					for(int i = 0; i < formType.getAttributeTypeList().size(); i++){
						AttributeTypeDomain attributeType = formType.getAttributeTypeList().get(i);
						String titleDefinitionName = EIMConfig.get("ATTRIBUTE_TYPE_NAME_TITLE").replaceAll(EIMConfig.get("APP_FORM_NAMESPACE") + ":", "");
						if(!titleDefinitionName.equals(attributeType.getDefinitionName())){
							out.println("<column id=\""+String.valueOf(attributeType.getId())+"\" name = \""+StringUtils.xmlEncode(attributeType.getName())+"\" definitionName=\""+StringUtils.xmlEncode(attributeType.getDefinitionName())+"\" />");
						}
					}
				}
			}
			out.println("</attributeFormListColumns>");
			
			// システム設定表示列
			out.println("<systemSettingFormListColumns>");
			{
				if(formType.getFormListColumn().getSystemSettingFormListColumns() != null){
					for(int i = 0; i < formType.getFormListColumn().getSystemSettingFormListColumns().size(); i++){
						AttributeTypeLayoutDomain attributeTypeLayout = formType.getFormListColumn().getSystemSettingFormListColumns().get(i);
						if(AppConstant.FORM_LIST_COLUMN_ID.equals(attributeTypeLayout.getFormListColumnId())){
							// ID
							out.println(nameColumnXml);
						}else if(AppConstant.FORM_LIST_COLUMN_TITLE.equals(attributeTypeLayout.getFormListColumnId())){
							// タイトル
							out.println(titleColumnXml);
						}else if(AppConstant.FORM_LIST_COLUMN_STATUS.equals(attributeTypeLayout.getFormListColumnId())){
							// ステータス
							out.println(statusColumnXml);
						}else if(AppConstant.FORM_LIST_COLUMN_CREATION_USER.equals(attributeTypeLayout.getFormListColumnId())){
							// 作成者
							out.println(creationUserXml);
						}else if(AppConstant.FORM_LIST_COLUMN_CREATION_DATE.equals(attributeTypeLayout.getFormListColumnId())){
							// 作成日時
							out.println(creationDateXml);
						}else if(AppConstant.FORM_LIST_COLUMN_MODIFICATION_USER.equals(attributeTypeLayout.getFormListColumnId())){
							// 更新者
							out.println(modificationUserXml);
						}else if(AppConstant.FORM_LIST_COLUMN_MODIFICATION_DATE.equals(attributeTypeLayout.getFormListColumnId())){
							// 更新日時
							out.println(modificationDateXml);
						}else{
							// 属性表示列
							for(int j = 0; j < formType.getAttributeTypeList().size(); j++){
								AttributeTypeDomain attributeType = formType.getAttributeTypeList().get(j);
								if(attributeType != null && String.valueOf(attributeType.getId()).equals(attributeTypeLayout.getFormListColumnId())){
									out.println("<column id=\""+String.valueOf(attributeType.getId())+"\" name = \""+StringUtils.xmlEncode(attributeType.getName())+"\" definitionName=\""+StringUtils.xmlEncode(attributeType.getDefinitionName())+"\" />");
									break;
								}
							}
						}
					}
				}
			}
			out.println("</systemSettingFormListColumns>");
			
			// 表示列最大数
			out.println("<formListColumnsMaxCount>");
			out.println(String.valueOf(formType.getFormListColumn().getFormListColumnMaxCount()));
			out.println("</formListColumnsMaxCount>");
			
			// End Root Node
			out.println("</formListColumnInfo>");
			
		}else{
			// Root Node
			out.println("<formListColumnInfo>");
			{
				// デフォルト表示列
				out.println("<defaultFormListColumns/>");
				// 属性表示列
				out.println("<attributeFormListColumns/>");
				// システム設定表示列
				out.println("<systemSettingFormListColumns/>");
				// 表示列最大数
				out.println("<formListColumnsMaxCount/>");
			}
			// End Root Node
			out.println("</formListColumnInfo>");
		}
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
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
