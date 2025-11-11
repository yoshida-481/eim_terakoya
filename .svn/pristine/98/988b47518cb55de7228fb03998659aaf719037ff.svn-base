<%@ page contentType="text/xml; charset=UTF-8" %>

<%@ page import="eim.net.*" %>
<%@ page import="eim.bo.*" %>
<%@ page import="eim.util.*" %>
<%@ page import="java.util.*" %>
<%@ page import="common.util.*" %>

<%@ page import="org.apache.commons.logging.*" %>

<%@page import="java.sql.Timestamp"%>

<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.CodeTypeCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>

<%@page import="jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.app.document.business.service.DocumentObjectTypeLayoutService"%>

<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.UIControlConfService"%>

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
		// EIMThreadContextにEIMSessionを登録
		EIMThreadContext.putEIMSession(sess);

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
		
		// ドキュメント管理 必須属性リストを取得
		String[] essentialAttributes = AppConstant.ESSENTIAL_ATTRIBUTE_DEFNAME;
		
		// 属性タイプ一覧(EIMAttributeType)
		List<EIMAttributeType> attTypeList = new ArrayList<EIMAttributeType>();
		
		// 属性タイプ一覧(カスタム属性(レイアウト)含む)
		List<AttributeTypeLayoutDomain> attTypeLayoutDomainList = null;
		
		// ネームスペースを取得
		String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");
		
		// 汎用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			
			// オブジェクトタイプに紐つくカスタム属性(レイアウト)含まない全属性タイプ一覧を取得
			attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
		
		// ドキュメント管理用・帳票管理用の場合
		} else {
			
			// カスタム属性(レイアウト)含むオブジェクトタイプを取得
			ObjectTypeLayoutService objectTypeLayoutService =
				(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectTypeLayoutService");
			ObjectTypeLayoutDomain objTypeLayoutdomain = objectTypeLayoutService.getById(new Long(objType.getId()).longValue());

			attTypeLayoutDomainList = objTypeLayoutdomain.getAttributeLayoutList();
		}
		
		// カスタム属性(レイアウト)含まない属性タイプのマップ
		Map<Long, AttributeTypeDomain> attTypeDomainMap = new HashMap<Long, AttributeTypeDomain>();
		
		// カスタム属性(レイアウト)含む属性タイプのマップ
		Map<Long, AttributeTypeLayoutDomain> attTypeLayoutDomainMap = new HashMap<Long, AttributeTypeLayoutDomain>();
		
		// UIコントロール名称のマップ
		Map<String, String> uiContorolNameMap = new HashMap<String, String>();
		
		// ドキュメント管理用・帳票管理用の場合
		if (!adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			
			// カスタム属性(レイアウト)含む属性タイプが取得できている場合
			if(attTypeLayoutDomainList != null && attTypeLayoutDomainList.size() > 0) {
				
				// カスタム属性(レイアウト)含む属性タイプをマッピング
				for(AttributeTypeLayoutDomain attTypeLayout : attTypeLayoutDomainList) {
					
					// 可視性が無い場合
					if (!attTypeLayout.isVisible()) {
						// 画面表示対象外
						continue;
					}
					
					// マッピング
					attTypeLayoutDomainMap.put(attTypeLayout.getId(), attTypeLayout);
					// コンバート
					attTypeList.add(ConvertUtils.toEIMAttributeType(attTypeLayout));
				}
				
				// UIコントロール定義Service
				UIControlConfService uiControlConnfService = 
					(UIControlConfService)ApplicationContextLoader.getApplicationContext().getBean("uIControlConfService");
				
				// UIコントロール定義一覧を取得
				List<UIControlConfDomain> uiControlConfList = uiControlConnfService.getList();
				
				// UIコントロール名称のマップを生成
				for (UIControlConfDomain uiContorlConf : uiControlConfList) {
					
					for (OtherNameDomain otherNameDomain : uiContorlConf.getNameList()) {
						
						// セッション言語のUIコントロール名称をマッピング
						if (otherNameDomain.getLangId().equals(sess.getLangId())) {
							uiContorolNameMap.put(uiContorlConf.getBeanId(), otherNameDomain.getName());
							break;
						}
					}
				}
			}
		}
		
		// 入力規則の存在判定用HashSet
		HashSet masterExistSet = new HashSet();
		// ドキュメント管理用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT))
		{
			// 属性タイプ値マスターが存在する属性の属性IDを取得
			masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(sess, attTypeList);
		}
		
		//Root Node
		out.println("<attTypes>");
		
		// コードマップ（key=id, value=name）
		Map<Long, String> codeMap = new HashMap<Long, String>();
		
		// コードマップ作成済みコードタイプIDセット
		Set<Long> codeTypeIdSet = new HashSet<Long>();
		
		int dispOrderCnt = 0;
		
		// 文書IDで検索可否フラグ
		boolean docSeachIndexFlag = Boolean.parseBoolean(EIMConfig.get("DOCUMENT_SEARCHINDEX_FLAG"));
		
		
		for(int i = 0; i < attTypeList.size(); i++)
		{
			//Attribute Type
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
			
			// 文書IDでの検索不可の場合にはスキップする
			if(docSeachIndexFlag == false){
				if( attType.getDefaultName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX")) == true ){
					continue;
				}
			}

			// 複数値属性
			String isMultipleValue;
			
			if (attType.isMultiple()) {
				
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.MAINTENANCE");	// 保持する
				
			} else {
				
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.NONE");		// なし
			}
			
			// 属性タイプのデフォルト値(デフォルト値が取り込まれていないので再取得)
			EIMAttributeType attTypeNew = AttributeUtils.getAttributeTypeById(sess, attType.getId());
			
			// カスタム属性(レイアウト)含む属性タイプ
			DocumentAttributeTypeLayoutDomain attTypeLayout = null;
			
			// デフォルト値XML情報
			StringBuffer attValueListStr = new StringBuffer();
			
			// 初期値XML情報
			StringBuffer attInitValueListStr = new StringBuffer();
			
			// 属性タイプID
			Long id = new Long(attType.getId());
			
			if(attTypeNew != null)
			{
				List defaultValueList = attTypeNew.getDefaultValueList();
				
				if (defaultValueList != null || defaultValueList.size() > 0 ) {
					switch (attTypeNew.getValueType().getId()) {
						// 数値型
						case EIMValueType.INTEGER:
							for( int j=0; j<defaultValueList.size(); j++ )
							{
								long valueInt = Long.parseLong(defaultValueList.get(j).toString());
								attValueListStr.append(" <defValue value=\"" + valueInt + "\"/>");
							}
							
							break;
							// 文字列型とテキスト型
						case EIMValueType.STRING:
						case EIMValueType.TEXT:
							for( int j=0; j<defaultValueList.size(); j++ )
							{
								String valueStrt = defaultValueList.get(j).toString();
								attValueListStr.append(" <defValue value=\"" + StringUtils.xmlEncode(valueStrt) + "\"/>");
							}
							break;
						// 日付型
						case EIMValueType.DATE:
							// タイムゾーン対応した値を取得
							for( int j=0; j<defaultValueList.size(); j++ )
							{
								Date a = new Date(((Timestamp)defaultValueList.get(j)).getTime());
								attValueListStr.append(" <defValue value=\"" + DateUtils.getDBTzToCLTzDate(sess, a) + "\"/>");
							}
							break;
						// ダブル型
						case EIMValueType.DOUBLE:
							for( int j=0; j<defaultValueList.size(); j++ )
							{
								Double b = Double.parseDouble(defaultValueList.get(j).toString());
								attValueListStr.append(" <defValue value=\"" + FormatUtil.getDoubleFormatedString(b) + "\"/>");
							}
							break;
					}
				}
				
				// ドキュメント管理用・帳票管理用の場合 and カスタム属性(レイアウト)含む属性タイプが存在する場合
				// 初期値を返却
				if (!adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL) && attTypeLayoutDomainMap.containsKey(id)) {
					
					// カスタム属性(レイアウト)含む属性タイプを取得
					attTypeLayout = (DocumentAttributeTypeLayoutDomain)attTypeLayoutDomainMap.get(id);
					
					switch (attTypeLayout.getValueType()) {
						// 数値型
						case LONG:
							for(Long initLongValue : attTypeLayout.getInitialLongValueList()) {
								
								attInitValueListStr.append(" <initValue value=\"" + initLongValue + "\"/>");
							}
							break;
						// 文字列型
						case STRING:
							for(String initStringValue : attTypeLayout.getInitialStringValueList()) {
								
								attInitValueListStr.append(" <initValue value=\"" + StringUtils.xmlEncode(initStringValue) + "\"/>");
							}
							break;
						// 実数型
						case DOUBLE:
							for(Double initDoubleValue : attTypeLayout.getInitialDoubleValueList()) {
								
								attInitValueListStr.append(" <initValue value=\"" + FormatUtil.getDoubleFormatedString(initDoubleValue) + "\"/>");
							}
							break;
						// コード型
						case CODE:
							CodeTypeDomain codeTypeDomain = attTypeLayout.getCodeType();
							
							// コードマップ未作成のコードタイプの場合
							if(!codeTypeIdSet.contains(codeTypeDomain.getId()))
							{
								/* マッピング追加 */
								List<CodeDomain> codeDomainList = codeTypeDomain.getCodeList();
								for(CodeDomain codeDomain : codeDomainList)
								{
									codeMap.put(codeDomain.getId(), codeDomain.getName());
								}
							}
							
							for(Long initCodeValue : attTypeLayout.getInitialCodeValueList()) {
								
								// IDをキーに名称を取得、出力
								String codeName = codeMap.get(initCodeValue);
								if(codeName != null){
									attInitValueListStr.append(" <initValue value=\"" + StringUtils.xmlEncode(codeName) + "\"/>");
								}
							}
							break;
						// ユーザ型
						case USER:
							for(String initUserValue : attTypeLayout.getInitialUserValueList()) {
								
								attInitValueListStr.append(" <initValue value=\"" + initUserValue + "\"/>");
							}
							break;
					}
				}
			}
			
			//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
			if (attValueListStr.length() == 0) {
				attValueListStr.append(" <defValue value=\"\"/>");
			}
			
			// 必須属性
			String isEssential = "false";
			
			//XML Out
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" objTypeId=\"" + objType.getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
				out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(AppValueTypeUtil.getValueTypeName(sess, attType.getValueType().getId())) + "\"");
				out.println(" isMultipleValue=\"" + isMultipleValue + "\"");	// 複数値属性
				
				// 定義名称生成
				String definitionName = "";
				// コードタイプ名称を取得
				String codeTypeName = "";
				
				// 汎用の場合
				if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
					
					// 定義名をネームスペース付で表示
					definitionName = NamespaceUtil.getDefNameWithNamespaceParentheses(
							attType.getName(), attType.getDefName());
					
					// コードタイプ名称を取得
					if (attTypeDomainMap.containsKey(id)) {
						
						if (attTypeDomainMap.get(id).getCodeType() != null) {
							
							codeTypeName = attTypeDomainMap.get(id).getCodeType().getDefinitionName();
						}
					}
					
					out.println(" definitionName=\"" + StringUtils.xmlEncode(definitionName) + "\""); // 定義名称
					out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");     // コードタイプ名称
					
				// ドキュメント管理用・帳票管理用の場合 and カスタム属性(レイアウト)含む属性タイプが取得できた場合
				} else if (attTypeLayout != null) {
				
					// 定義名をネームスペースなしで表示
					definitionName = NamespaceUtil.getDefNamenWhichExceptedNamespace(attTypeLayout.getDefinitionName());
					
					// UIコントロール名称
					String uiControlName = "";
						
					String tmpUiControlId = attTypeLayout.getUiControlId();
					if( tmpUiControlId != null ){
						if (uiContorolNameMap.containsKey(tmpUiControlId)) {
							
							uiControlName = uiContorolNameMap.get(tmpUiControlId);
						}
					}
					
					String dispOrderStr = "";
					// 表示順が設定されていた場合
					if (attTypeLayout != null && attTypeLayout.isOrderSetFlag()) {
						dispOrderCnt++;
						dispOrderStr = String.valueOf(dispOrderCnt);
					}
					out.println(" definitionName=\"" + StringUtils.xmlEncode(definitionName) + "\"");	// 定義名称
					out.println(" uiControlName=\"" + StringUtils.xmlEncode(uiControlName) + "\""); 	// UIコントロール名称
					out.println(" uiControlId=\"" + StringUtils.xmlEncode(tmpUiControlId) + "\"");		// UIコントロールBeanId
					out.println(" dispOrder=\"" + dispOrderStr + "\""); // 表示順
					out.println(" inheritanceFlag=\"" + attTypeLayout.isInheritanceFlag() + "\""); 		// リビジョンアップ引継ぎフラグ
					out.println(" relationFlag=\"" + attTypeLayout.isRelationFlag() + "\""); 			// 最新リビジョン関連付けフラグ

					if (adminAppId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {		// ドキュメント管理用の場合
						
						// 必須属性かどうか判定
						String defName = attType.getDefName();
						
						for (int j = 0; j < essentialAttributes.length; j++)
						{
							if (defName.equals(essentialAttributes[j]))
								isEssential = "true";
						}
						
						// 入力規則
						String inputRuleValue;
						
						// マスター定義かどうか判定
						if (masterExistSet.contains(new Long(attType.getId()))) {
							// リスト定義
							inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.LIST");
							
						} else {
							// なし
							inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.NONE");
						}
						
						out.println(" attTypeEssential=\"" + isEssential + "\"");		// 必須
						out.println(" inputRuleValue=\"" + inputRuleValue + "\"");		// 入力規則
					} 
					
					else {															// 帳票管理用の場合

						// コードタイプ名称を取得
						if (attTypeLayout.getCodeType() != null) {
							
							codeTypeName = attTypeLayout.getCodeType().getDefinitionName();
						}
						
						out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");		// コードタイプ名称
						out.println(" attTypeEssential=\"" + attTypeLayout.isRequired() + "\"");			// 必須
						out.println(" newCopyFlag=\"" + attTypeLayout.getNewCopyFlag() + "\"");				// 複製フラグ
					}
					
				}
				
				out.println(">");
				
				// デフォルト値のリスト
				out.println("<defValueList>");
					out.println(attValueListStr.toString());
				out.println("</defValueList>");
				
				// ドキュメント管理用・帳票管理用の場合
				if (!adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
					
					if (attInitValueListStr.length() == 0) {
						attInitValueListStr.append(" <initValue value=\"\"/>");
					}
					
					// 初期値のリスト
					out.println("<initValueList>");
						out.println(attInitValueListStr.toString());
					out.println("</initValueList>");
				}
				
			out.println("</attType>");
		}
		
		//End Root Node
		out.println("</attTypes>");
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
	}
	finally
	{
		try{
			if(EIMThreadContext.getEIMSession() != null){
				EIMThreadContext.removeEIMSession();
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
