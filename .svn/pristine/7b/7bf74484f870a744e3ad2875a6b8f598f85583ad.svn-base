<%@ page contentType="text/xml; charset=UTF-8" %>

<%@ page import="common.util.*" %>
<%@ page import="common.bo.*" %>
<%@ page import="eim.net.*" %>
<%@ page import="eim.bo.*" %>
<%@ page import="eim.util.*" %>
<%@ page import="org.apache.commons.logging.*" %>
<%@ page import="java.util.*" %>

<%@page import="java.sql.Timestamp"%>

<%@ page import="jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.UIControlConfService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.AttributeTypeLayoutService"%>

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

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId
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

		/*
		 * Get Attribute Type
		 */
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

		// APPID
		String appId = sess.getAttribute("ADMIN_APP_ID").toString();

		//Get Attribute Value Master (ドキュメント管理用システム管理の場合)
		AttributeValueMaster attValMst = null;
		if(appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
		}

		//Set Attribute Value Master CheckBox
		boolean isValMaster = false;

		if (attValMst != null) {
			isValMaster = true;
		}

		//Other Attribute Type
		List otherList = AttributeUtils.getOtherAttTypeNameList(sess, attType.getId());

		// 属性タイプのデフォルト値(デフォルト値が取り込まれていないので再取得)
		EIMAttributeType attTypeNew = AttributeUtils.getAttributeTypeById(sess, attType.getId());

		StringBuffer attDefValueListStr = new StringBuffer();
		if( attTypeNew != null )
		{
			List defaultValueList = attTypeNew.getDefaultValueList();

			if (defaultValueList != null || defaultValueList.size() > 0 ) {
				switch (attTypeNew.getValueType().getId()) {
					// 数値型
					case EIMValueType.INTEGER:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							long valueInt = Long.parseLong(defaultValueList.get(j).toString());
							attDefValueListStr.append(" <defValue value=\"" + valueInt + "\"/>");
						}

						break;
						// 文字列型とテキスト型
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							String valueStrt = defaultValueList.get(j).toString();
							attDefValueListStr.append(" <defValue value=\"" + StringUtils.xmlEncode(valueStrt) + "\"/>");
						}
						break;
					// 日付型
					case EIMValueType.DATE:
						// タイムゾーン対応した値を取得
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							Date a = new Date(((Timestamp)defaultValueList.get(j)).getTime());
							attDefValueListStr.append(" <defValue value=\"" + DateUtils.getDBTzToCLTzDate(sess, a) + "\"/>");
						}
						break;
					// ダブル型
					case EIMValueType.DOUBLE:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							Double b = Double.parseDouble(defaultValueList.get(j).toString());
							attDefValueListStr.append(" <defValue value=\"" + FormatUtil.getDoubleFormatedString(b) + "\"/>");
						}
						break;
				}
			}
		}
		//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
		if (attDefValueListStr.length() == 0) {
			attDefValueListStr.append(" <defValue value=\"\"/>");
		}

		// 各システム管理共通 レイアウトドメインの取得または生成
		AttributeTypeLayoutDomain attTypeLayoutDomain = null;

		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			//ドキュメント管理用・帳票用システム管理の場合は、レイアウトドメインを取得
			AttributeTypeLayoutService attributeTypeLayoutService =
				(AttributeTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeLayoutService");
			attTypeLayoutDomain = attributeTypeLayoutService.getById(Long.parseLong(prmAttTypeId));
		}else{
			//汎用システム管理の場合は、
			AttributeTypeService attributeTypeService =
				(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
			AttributeTypeDomain attributeType = attributeTypeService.getById(Long.parseLong(prmAttTypeId));
			attTypeLayoutDomain = new AttributeTypeLayoutDomain(attributeType);
		}

		// 属性タイプの初期値
		StringBuffer attInitValueListStr = new StringBuffer();
		if( attTypeLayoutDomain != null )
		{
			List<String> initialStringValueList = attTypeLayoutDomain.getInitialStringValueList();
			for(String stringValue : initialStringValueList)
			{
				attInitValueListStr.append(" <initValue value=\"" + StringUtils.xmlEncode(stringValue) + "\"/>");
			}

			List<Double> initialDoubleValueList = attTypeLayoutDomain.getInitialDoubleValueList();
			for(Double doubleValue : initialDoubleValueList)
			{
				attInitValueListStr.append(" <initValue value=\"" + FormatUtil.getDoubleFormatedString(doubleValue) + "\"/>");
			}

			List<Long> initialLongValueList = attTypeLayoutDomain.getInitialLongValueList();
			for(Long longValue : initialLongValueList)
			{
				attInitValueListStr.append(" <initValue value=\"" + longValue + "\"/>");
			}

			List<Long> initialCodeValueList = attTypeLayoutDomain.getInitialCodeValueList();
			for(Long codeValue : initialCodeValueList)
			{
				attInitValueListStr.append(" <initValue value=\"" + codeValue.toString() + "\"/>");
			}
			List<String> initialUserValueList = attTypeLayoutDomain.getInitialUserValueList();
			for(String userValue : initialUserValueList)
			{
				attInitValueListStr.append(" <initValue value=\"" + userValue + "\"/>");
			}
		}

		//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
		if (attInitValueListStr.length() == 0) {
			attInitValueListStr.append(" <initValue value=\"\"/>");
		}

		//XML Out
		// 属性
		out.println("<attType");
			out.println(" attTypeId=\"" + attType.getId() + "\"");
			//定義名称からネームスペースを取得
			out.println(" namespace=\"" + StringUtils.xmlEncode(NamespaceUtil.getNamespaceByDefName(attType.getDefName())) + "\"");

			out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
			if(otherList != null)
			{
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_CNT + "=\"" + otherList.size() + "\"");
			}
			out.println(" isValMaster=\"" + isValMaster + "\"");
			out.println(" isMultiple=\"" + attType.isMultiple() + "\"");
			out.println(" definitionName=\"" + StringUtils.xmlEncode(attType.getDefName()) + "\"");

			// 各システム管理共通
			String codeTypeId = "";
			String codeTypeName = "";
			CodeTypeDomain codeTypeDomain = attTypeLayoutDomain.getCodeType();
			if(codeTypeDomain != null){
				codeTypeId = String.valueOf(codeTypeDomain.getId());
				codeTypeName =  codeTypeDomain.getDefinitionName();
			}
			out.println(" codeTypeId=\"" + codeTypeId + "\"");
			out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");

			// ドキュメント管理用・帳票用の場合
			if(!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
				out.println(" attTypeEssential=\"" + attTypeLayoutDomain.isRequired() + "\"");
				out.println(" uiControlType=\"" + attTypeLayoutDomain.getUiControlType() + "\"");
				out.println(" uiControlId=\"" + attTypeLayoutDomain.getUiControlId() + "\"");
			}
			out.println(">");

			for(int i=0;i<otherList.size();i++){
				EIMOtherName eimOtherName = (EIMOtherName)otherList.get(i);
				out.println("<lang");
					out.println(" " + LanguageFieldUtil.PARAM_OTHRE_LID + "=\"" + eimOtherName.getLangId() + "\"");
					out.println(" " + LanguageFieldUtil.PARAM_OTHRE_NAME + "=\"" + StringUtils.xmlEncode(eimOtherName.getName()) + "\"");
				out.println("/>");
			}

			// デフォルト値のリスト
			out.println("<defValueList>");
				out.println(attDefValueListStr.toString());
			out.println("</defValueList>");

			// 初期値のリスト
			out.println("<initValueList>");
				out.println(attInitValueListStr.toString());
			out.println("</initValueList>");

			// コードのリスト(コード型の場合のみ)
			if(attType.getValueType().getId() == EIMValueType.CODE){
				out.println("<codeList>");
					codeTypeDomain = attTypeLayoutDomain.getCodeType();
					List<CodeDomain> codeDomainList = codeTypeDomain.getCodeList();

					//表示順にソート
					List<CodeDomain> sortedCodeDomainList = AppObjectUtil.getIntSortedList(codeDomainList, "getSequence", true);

					for(CodeDomain codeDomain : sortedCodeDomainList){

						if(!codeDomain.isDisable()){
							// 有効なコードのみ返却
							out.println("<code");
							out.println(" id=\"" + codeDomain.getId() + "\"");
							out.println(" code=\"" + StringUtils.xmlEncode(codeDomain.getCode()) + "\"");
							out.println(" name=\"" + StringUtils.xmlEncode(codeDomain.getName()) + "\"");

							out.println("/>");
						}
					}

				out.println("</codeList>");
			}

		out.println("</attType>");




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
