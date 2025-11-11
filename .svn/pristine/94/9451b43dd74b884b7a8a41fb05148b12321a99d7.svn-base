
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain"%>
<%@page import="java.util.HashMap"%>
<%@page import="java.util.Map"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.AttributeTypeLayoutService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.SearchMasterDisplayConfigDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>
<%@ page import = "java.util.List" %>
<%@ page import = "java.util.ArrayList"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>

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
	String prmAttTypeId = EIMUtils.getParameter(request, "attTypeId");
	String prmInputRuleCheck = EIMUtils.getParameter(request, "inputRuleCheck");
	String prmMultipleCheck = request.getParameter("multipleCheck");

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId,
			"inputRuleCheck=" + prmInputRuleCheck
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIBUTE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Namespace
		NamespaceDomain namespaceDomain = (NamespaceDomain)sess.getAttribute("namespace");

		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(prmAttTypeId));
		if(attType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		/*
		 * Update Attribute Type
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);

		boolean isMultiple = false;

		if (prmMultipleCheck != null && prmMultipleCheck.equals("true")) {
			isMultiple = true;
		}

		AttributeTypeLayoutDomain attributeTypeLayoutDomain = new AttributeTypeLayoutDomain();
		List<String> initValueList = new ArrayList<String>();
		String refmasterTypeName = EIMUtils.getParameter(request, "refmasterTypeName");
		String codeTypeId = EIMUtils.getParameter(request, "codeTypeId");
		String codeTypeName = EIMUtils.getParameter(request, "codeTypeName");
		String definitionName = EIMUtils.getParameter(request, "definitionName");
		Boolean required = Boolean.valueOf(EIMUtils.getParameter(request, "required")).booleanValue();
		String uicontrolId = EIMUtils.getParameter(request, "uicontrolId");
		String uicontrolName = EIMUtils.getParameter(request, "uicontrolName");
		String uicontrolType = EIMUtils.getParameter(request, "uicontrolType");

		String appId = sess.getAttribute("ADMIN_APP_ID").toString();

		/* 各システム管理共通 */
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));


		// Value Type Enum
		ValueTypeEnum valType = null;
		String prmValTypeName = EIMUtils.getParameter(request, "valTypeName");
		if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.INTEGER")) ) {
			valType = ValueTypeEnum.LONG;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.STRING")) ) {
			valType = ValueTypeEnum.STRING;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.DATE")) ) {
			valType = ValueTypeEnum.DATE;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.TEXT")) ) {
			valType = ValueTypeEnum.TEXT;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.DOUBLE")) ) {
			valType = ValueTypeEnum.DOUBLE;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.OBJECT")) ) {
			valType = ValueTypeEnum.OBJECT;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.USER")) ) {
			valType = ValueTypeEnum.USER;
		} else if( prmValTypeName.equals(EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.CODE")) ) {
			valType = ValueTypeEnum.CODE;
		}

		String namespace;

		if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL))
		{
			//汎用システム管理はネームスペースをリクエストから取得
			namespace = EIMUtils.getParameter(request, "namespace");
		} else {
			//汎用システム管理以外はネームスペースをドメインから取得
			namespace = namespaceDomain.getName();
		}

		String prmDefName;
		
		if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			// 文書管理の場合、ネームスペース付きの定義名称を取得(言語名称から取得)
			prmDefName = LanguageFieldUtil.getDefName(sess, null, request, prmOtherCnt);
		}
		else {
			// 汎用・帳票管理用の場合、ネームスペース付きの定義名称を取得(ネームスペースと言語名称から生成)
			prmDefName = NamespaceUtil.concatenate(namespace, definitionName);
		}
		
		attributeTypeLayoutDomain.setId(Long.parseLong(prmAttTypeId));
		attributeTypeLayoutDomain.setDefinitionName(prmDefName);
		attributeTypeLayoutDomain.setValueType(valType);
		attributeTypeLayoutDomain.setMultiple(isMultiple);
		attributeTypeLayoutDomain.setVisible(true); // 可視性は常にtrue

		attributeTypeLayoutDomain.setRequired(required);
		attributeTypeLayoutDomain.setUiControlId(uicontrolId);
		attributeTypeLayoutDomain.setUiControlType(uicontrolType);

		if(codeTypeName != null && codeTypeName.length() > 0){
			CodeTypeDomain codeTypeDomain = new CodeTypeDomain();
			codeTypeDomain.setId(Long.parseLong(codeTypeId));
			codeTypeDomain.setDefinitionName(codeTypeName);
			attributeTypeLayoutDomain.setCodeType(codeTypeDomain);
		}

		if(refmasterTypeName != null && refmasterTypeName.length() > 0){
			ObjectTypeDomain refmasterType = new ObjectTypeDomain();
			refmasterType.setDefinitionName(refmasterTypeName);
			SearchMasterDisplayConfigDomain searchMasterDisplayConfigDomain = new SearchMasterDisplayConfigDomain();
			searchMasterDisplayConfigDomain.setObjectType(refmasterType);
			attributeTypeLayoutDomain.setSearchMasterDisplayConfig(searchMasterDisplayConfigDomain);
		}

		// ドキュメント管理用・帳票管理用システム管理の場合、初期値を設定
		if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			int pos = 0;
			while(true) {
				String param = EIMUtils.getParameter(request, "initValue_" + pos);
				if ( param == null || param.length() <= 0) {
					break;
				}
				initValueList.add(param);
				pos++;
			}

			switch(valType.getValue()){
				// 数値型
				case EIMValueType.INTEGER:

					List<Long> longValueList = new ArrayList<Long>();
					for(String initValueStr : initValueList)
					{
						longValueList.add(Long.parseLong(initValueStr));
					}
					attributeTypeLayoutDomain.setInitialLongValueList(longValueList);
					break;

				// 文字列型
				case EIMValueType.STRING:

					attributeTypeLayoutDomain.setInitialStringValueList(initValueList);
					break;

				// ダブル型
				case EIMValueType.DOUBLE:

					List<Double> doubleValueList = new ArrayList<Double>();
					for(String initValueStr : initValueList)
					{
						doubleValueList.add(Double.parseDouble(initValueStr));
					}
					attributeTypeLayoutDomain.setInitialDoubleValueList(doubleValueList);
					break;
				// コード型
				case EIMValueType.CODE:

					List<Long>codeValueList = new ArrayList<Long>();
					for(String initValueStr : initValueList)
					{
						codeValueList.add(Long.parseLong(initValueStr));
					}
					attributeTypeLayoutDomain.setInitialCodeValueList(codeValueList);
					break;
				// ユーザ型
				case EIMValueType.USER:

					List<String>userValueList = new ArrayList<String>();
					for(String initValueStr : initValueList)
					{
						userValueList.add(initValueStr);
					}
					attributeTypeLayoutDomain.setInitialUserValueList(userValueList);
					break;
			}
		}

		if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			//ドキュメント管理用・帳票用システム管理の場合はレイアウト情報を更新する
			AttributeTypeLayoutService attributeTypeLayoutService =
				(AttributeTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeLayoutService");
			attributeTypeLayoutService.update(attributeTypeLayoutDomain);

			// レイアウト情報再取得
			attributeTypeLayoutDomain = attributeTypeLayoutService.getById(attType.getId());

		}else{

			//汎用システム管理の場合はレイアウト情報を作成しない
			AttributeTypeService attributeTypeService =
				(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
			attributeTypeService.update(attributeTypeLayoutDomain);
			AttributeTypeDomain attributeType = attributeTypeService.getById(attributeTypeLayoutDomain.getId());
			attributeTypeLayoutDomain = new AttributeTypeLayoutDomain(attributeType);

		}

		//後続の処理をV4系に合わせるためコンバートをかける
		attType = ConvertUtils.toEIMAttributeType(attributeTypeLayoutDomain);

		/*
		 * Update Attribute Type Other
		 */
		String nowLId = (String)sess.getAttribute(EIMSession.LANG);
		String nowLangName = attType.getName();
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			// Get NowLangName
			if(nowLId.equals(prmOtherLId)){
				nowLangName = prmOtherName;
			}

			//Update
			AttributeUtils.updateOtherAttributeTypeName(sess, attType.getId(), prmOtherLId, prmOtherName);
		}

		//Get Attribute Value Master (ドキュメント管理用システム管理の場合)
		AttributeValueMaster attValMst = null;
		if(appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
		}

		//Create Attribute Value Master
		if (prmInputRuleCheck != null && prmInputRuleCheck.equals("true") && attValMst == null) {
			AttributeMasterUtil.createAttributeValueMaster(sess, attType);
		}

		//Delete Attribute Value Master
		if (prmInputRuleCheck != null && prmInputRuleCheck.equals("false") && attValMst != null) {
			AttributeMasterUtil.deleteAttributeValueMaster(sess, attValMst);
		}

		// Default Value Update
		List<String> defValueList = new ArrayList<String>();
		int pos = 0;
		while(true) {
			String param = EIMUtils.getParameter(request, "defaultValue_" + pos);
			if ( param == null || param.length() <= 0) {
				break;
			}
			defValueList.add(param);
			pos++;
		}

		if(defValueList.size() > 0){
			AttributeUtil.setDefaultValueApp(sess, attType, defValueList);
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_ATTRIBUTE,
				EIMConstant.TARGET_UPDATE, EIMConstant.ATTRIBUTE_TYPE, attType,
				null, null, null, null);

		//Commit
		sess.commit();

		//Get Attribute Value Master (ドキュメント管理用システム管理の場合)
		if(appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
		}

		String inputRuleValue = null;
		String inputRuleFlag;
		if (attValMst != null) {
			inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.LIST");	// リスト定義
			inputRuleFlag = "true";
		} else {
			inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.NONE");	// なし
			inputRuleFlag = "false";
		}

		// 複数値属性
		String isMultipleValue;
		String isMultipleFlag;
		if (attType.isMultiple()) {
			isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.MAINTENANCE");	// 保持する
			isMultipleFlag = "true";
		} else {
			isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.NONE");		// なし
			isMultipleFlag = "false";
		}

		//XML
		out.println("<attType");
			out.println(" attTypeId=\"" + attType.getId() + "\"");
			out.println(" attTypeName=\"" + StringUtils.xmlEncode(nowLangName) + "\"");
			if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL))
			{
				//汎用システム管理は定義名をネームスペース付で表示
				out.println(" attTypeDefName=\"" + StringUtils.xmlEncode(attType.getDefName()) + "\"");
			}
			out.println(" valType=\"" + attType.getValueType().getId() + "\"");
			out.println(" valTypeName=\"" + StringUtils.xmlEncode(attributeTypeLayoutDomain.getValueType().toString()) + "\"");
			out.println(" inputRuleValue=\"" + inputRuleValue + "\"");		// 入力規則
			out.println(" inputRuleFlag=\"" + inputRuleFlag + "\"");		// 入力規則フラグ
			out.println(" isMultipleValue=\"" + isMultipleValue + "\"");	// 複数値属性
			out.println(" isMultipleFlag=\"" + isMultipleFlag + "\"");		// 複数値属性フラグ

			if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
				// ドキュメント管理用・帳票用システム管理の場合
				out.println(" attTypeEssential=\"" + required + "\"");
				
				if(uicontrolName == null){
					uicontrolName = "";
				}
				out.println(" uiControlName=\"" + StringUtils.xmlEncode(uicontrolName) + "\"");

				out.println(" refmasterTypeName=\"" + inputRuleValue + "\"");
			}

			if(appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
				// 汎用システム管理の場合のみ
				// 定義名をネームスペース付で表示
				definitionName = NamespaceUtil.getDefNameWithNamespaceParentheses(
							attType.getName(), attType.getDefName());
				out.println(" definitionName=\"" + StringUtils.xmlEncode(definitionName) + "\"");
				out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");

			} else {
				// ドキュメント管理用・帳票管理用システム管理の場合
				out.println(" definitionName=\"" + StringUtils.xmlEncode(definitionName) + "\"");
				out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");

			}


			out.println(">");

			// 属性タイプのデフォルト値
			out.println("<defValueList>");
			for( int j=0; j<defValueList.size(); j++ ){
				out.print("<defValue value=\"" + StringUtils.xmlEncode(defValueList.get(j).toString()) + "\"/>" );
			}
			out.println("\n</defValueList>");

			if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
				// ドキュメント管理用・帳票管理用システム管理、属性タイプの初期値

				out.println("<initValueList>");

				// コード型の場合
				if(attType.getValueType().getId() == EIMValueType.CODE){
					// コードマップ（key=id, value=name）
					Map<Long, String> codeMap = new HashMap<Long, String>();

					// コードマップ生成
					CodeTypeDomain codeTypeDomain = attributeTypeLayoutDomain.getCodeType();
					List<CodeDomain> codeDomainList = codeTypeDomain.getCodeList();
					for(CodeDomain codeDomain : codeDomainList)
					{
						codeMap.put(codeDomain.getId(), codeDomain.getName());
					}

					// IDを名称に変換して出力
					for( int k=0; k<initValueList.size(); k++ ){
						String initValue = codeMap.get(Long.parseLong(initValueList.get(k).toString()));
						out.print("<initValue value=\"" + StringUtils.xmlEncode(initValue) + "\"/>" );
					}


				} else {
					for( int k=0; k<initValueList.size(); k++ ){
						out.print("<initValue value=\"" + StringUtils.xmlEncode(initValueList.get(k).toString()) + "\"/>" );
					}
				}
				if(initValueList.size() == 0){
					out.print("<initValue value=\"\"/>");
				}
				out.println("\n</initValueList>");
			}

		out.println("</attType>");

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
