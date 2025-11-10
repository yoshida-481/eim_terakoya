<%@page contentType="text/xml; charset=UTF-8" %>

<%@page import="eim.net.*" %>
<%@page import="eim.bo.*" %>
<%@page import="eim.util.*" %>
<%@page import="java.util.*" %>
<%@page import="common.util.*" %>

<%@page import="java.sql.Timestamp"%>

<%@page import="org.apache.commons.logging.*" %>

<%@page import="jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.UIControlConfService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.AttributeTypeLayoutService"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain" %>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.CodeTypeCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.util.ConfigUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
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
	String prmAttTypeName = EIMUtils.getParameter(request, "attTypeName");
	String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");
	String prmStTypeId = EIMUtils.getParameter(request, "stTypeId");

	// リレーション管理の属性タイプ一覧かどうかのフラグを取得
	String prmRelationViewFlag = EIMUtils.getParameter(request, "relationViewFlag");

	// ルートオブジェクトタイプが一般ドキュメントかどうかのフラグを取得
	String prmDocumentTypeFlag = EIMUtils.getParameter(request, "documentTypeFlag");

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeName=" + prmAttTypeName
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
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
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

		// カスタム属性(レイアウト)Service
		AttributeTypeLayoutService attributeTypeLayoutService = null;

		//Attribute Type List
		List<EIMAttributeType> attTypeList = new ArrayList<EIMAttributeType>();

		// 属性タイプ一覧(カスタム属性(レイアウト)含まない)
		List<AttributeTypeDomain> attTypeDomainList =
			new ArrayList<AttributeTypeDomain>();

		// 属性タイプ一覧(カスタム属性(レイアウト)含む)
		List<AttributeTypeLayoutDomain> attTypeLayoutDomainList =
			new ArrayList<AttributeTypeLayoutDomain>();

		// 検索条件(定義名称)を設定
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();

		// 検索条件とする定義名称
		String defSearchName = "";

		// APPID
		String appId = (String)sess.getAttribute("ADMIN_APP_ID");

		// 汎用の場合
		if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			// 部分一致の為、前後に*追加
			defSearchName = "*";
			if(StringUtils.isBlank(prmAttTypeName) != true )
			{
				defSearchName += prmAttTypeName + "*";
			}

		// 帳票管理用の場合
		} else if (appId.equals(AppConstant.ADMIN_APP_ID_FORM)) {

			// 定義名称(ネームスペース＋検索文字列)を検索条件とする（定義名称のあいまい検索を行うためV5APIを使用）
			String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");
			if(prmAttTypeName == null || prmAttTypeName.equals(""))
			{
				prmAttTypeName = "*";
			}
			defSearchName = NamespaceUtil.concatenate(namespaceStr, prmAttTypeName) + "*"; // 前方一致の為、末尾に*追加
			//クラス管理の属性の場合
			if(prmObjTypeId != null){
				Long prmLongObjTypeId = Long.parseLong(prmObjTypeId);
				attributeTypeCriteria.setObjectTypeId(prmLongObjTypeId);
			}
			//ワークフロー管理の場合
			if(prmStTypeId != null){
				Long prmLongStTypeId = Long.parseLong(prmStTypeId);
				attributeTypeCriteria.setStatusTypeId(prmLongStTypeId);
			}

		// タスク管理用の場合
		} else if (appId.equals(AppConstant.ADMIN_APP_ID_TASK)) {

			// 定義名称(ネームスペース＋検索文字列)を検索条件とする（定義名称のあいまい検索を行うためV5APIを使用）
			String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");
			if(prmAttTypeName == null || prmAttTypeName.equals(""))
			{
				prmAttTypeName = "*";
			}
			defSearchName = NamespaceUtil.concatenate(namespaceStr, prmAttTypeName) + "*"; // 前方一致の為、末尾に*追加
			//クラス管理の属性の場合
			if(prmObjTypeId != null){
				Long prmLongObjTypeId = Long.parseLong(prmObjTypeId);
				attributeTypeCriteria.setObjectTypeId(prmLongObjTypeId);
			}
			//ワークフロー管理の場合
			if(prmStTypeId != null){
				Long prmLongStTypeId = Long.parseLong(prmStTypeId);
				attributeTypeCriteria.setStatusTypeId(prmLongStTypeId);
			}

		// ドキュメント管理用の場合
		} else {

			// 定義名称(ネームスペース＋検索文字列)を検索条件とする（定義名称のあいまい検索を行うためV5APIを使用）
			String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");
			if(prmAttTypeName == null || prmAttTypeName.equals(""))
			{
				prmAttTypeName = "*";
			}
			defSearchName = NamespaceUtil.concatenate(namespaceStr, prmAttTypeName) + "*"; // 前方一致の為、末尾に*追加
			//クラス管理の属性の場合
			if(prmObjTypeId != null){
				Long prmLongObjTypeId = Long.parseLong(prmObjTypeId);
				attributeTypeCriteria.setObjectTypeId(prmLongObjTypeId);
			}
		}

		attributeTypeCriteria.setDefinitionName(defSearchName);

		// 汎用の場合
		if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			// カスタム属性(レイアウト)含まない属性タイプ一覧を取得
			AttributeTypeService attributeTypeService =
				(AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
			attTypeDomainList = attributeTypeService.getList(attributeTypeCriteria);
			// 他言語名称でソートを実装
			attTypeDomainList = AppObjectUtil.getStrSortedList(attTypeDomainList, "getName", true);

			// 検索結果が0件の時
			if(attTypeDomainList.size() == 0)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

		// ドキュメント管理用・帳票管理用の場合
		} else {

			// カスタム属性(レイアウト)含む属性タイプ一覧を取得
			attributeTypeLayoutService =
				(AttributeTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeLayoutService");
			attTypeLayoutDomainList = attributeTypeLayoutService.getList(attributeTypeCriteria);
			// 他言語名称でソートを実装
			attTypeLayoutDomainList = AppObjectUtil.getStrSortedList(attTypeLayoutDomainList, "getName", true);

			// 検索結果が0件の時
			if(attTypeLayoutDomainList.size() == 0)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
		}

		// カスタム属性(レイアウト)含まない属性タイプのマップ
		Map<Long, AttributeTypeDomain> attTypeDomainMap = new HashMap<Long, AttributeTypeDomain>();

		// カスタム属性(レイアウト)含む属性タイプのマップ
		Map<Long, AttributeTypeLayoutDomain> attTypeLayoutDomainMap = new HashMap<Long, AttributeTypeLayoutDomain>();

		// UIコントロール名称のマップ
		Map<String, String> uiContorolNameMap = new HashMap<String, String>();

		// 後続の処理をV4系に合わせるためコンバートをかける
		// 汎用の場合
		if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

			if (attTypeDomainList != null) {

				for (AttributeTypeDomain attributeTypeDomain : attTypeDomainList) {

					// マッピング
					attTypeDomainMap.put(attributeTypeDomain.getId(), attributeTypeDomain);
					// コンバート
					attTypeList.add(ConvertUtils.toEIMAttributeType(attributeTypeDomain));
				}
			}

		// ドキュメント管理用・帳票管理用の場合
		} else {

			// カスタム属性(レイアウト)含む属性タイプが取得できている場合
			if(attTypeLayoutDomainList != null && attTypeLayoutDomainList.size() > 0) {

				// ドキュメント管理の表示対象外ネームスペースを取得
				String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
				String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

				// カスタム属性(レイアウト)含む属性タイプをマッピング
				outerLoop:
				for(AttributeTypeLayoutDomain attTypeLayout : attTypeLayoutDomainList) {

					// 可視性が無い場合
					if (!attTypeLayout.isVisible()) {
						// 画面表示対象外
						continue;
					}

					// ドキュメント管理の場合
					if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
						// ドキュメント管理の場合表示対象外ネームスペースのワークフローを除去する
						for (int i = 0; i < excludeNameSpaceArray.length; i++) {

							String excludeNameSpace = excludeNameSpaceArray[i].trim();
							if (attTypeLayout.getDefinitionName().indexOf(excludeNameSpace) == 0) {
								continue outerLoop;
							}
						}
					}

					// 帳票管理用の場合
					if (appId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
						// UIコントロール：ドキュメント検索 の場合
						String uiControlId = attTypeLayout.getUiControlId();
						if(uiControlId != null && uiControlId.equals(ConfigUtils.getByKey("UI_CONTROL_ID_RELATED_DOCUMEMT"))) {
							// 画面表示対象外
							continue;
						}
					}

					// タスク管理用の場合
					if (appId.equals(AppConstant.ADMIN_APP_ID_TASK)) {
						// UIコントロール：ドキュメント検索 の場合
						String uiControlId = attTypeLayout.getUiControlId();
						if(uiControlId != null && uiControlId.equals(ConfigUtils.getByKey("UI_CONTROL_ID_RELATED_DOCUMEMT"))) {
							// 画面表示対象外
							continue;
						}
					}

					// マッピング
					attTypeLayoutDomainMap.put(attTypeLayout.getId(), attTypeLayout);
					// コンバート
					attTypeList.add(ConvertUtils.toEIMAttributeType(attTypeLayout));
				}

				// 表示対象を除いた結果が0件の時
				if (attTypeList.size() == 0) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}

				// UIコントロール定義Service
				UIControlConfService uiControlConfService =
					(UIControlConfService)ApplicationContextLoader.getApplicationContext().getBean("uIControlConfService");

				// UIコントロール定義一覧を取得
				List<UIControlConfDomain> uiControlConfList = uiControlConfService.getList();

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

		// 入力規則の存在判定用HashSet (ドキュメント管理用の場合)
		HashSet masterExistSet = new HashSet();
		if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(sess, attTypeList);
		}

		//Root Node
		out.println("<attTypes>");

		// コードマップ（key=id, value=name）
		Map<Long, String> codeMap = new HashMap<Long, String>();

		// コードマップ作成済みコードタイプIDセット
		Set<Long> codeTypeIdSet = new HashSet<Long>();

		//ドキュメント管理の必須属性リスト
		String[] essentialAttributes = AppConstant.ESSENTIAL_ATTRIBUTE_DEFNAME;
		//フレームワークの初期登録属性リスト
		String[] frameworkAttributes = AppConstant.FRAMEWORK_ATTRIBUTE_DEFNAME;

		ATTR_LIST_OUTPUT_LOOP:
		for (int i = 0; i < attTypeList.size(); i++)
		{
			//Attribute Type
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);

			//ドキュメント管理用の場合、必須属性は非表示
			if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT))
			{
				String defName = attType.getDefName();

				//必須属性は非表示
				for (int j=0; j<essentialAttributes.length; j++)
				{
					if (defName.equals(essentialAttributes[j]))
						continue ATTR_LIST_OUTPUT_LOOP;
				}

				//フレームワーク初期登録属性は非表示
				for (int k=0; k<frameworkAttributes.length; k++)
				{
					if (defName.equals(frameworkAttributes[k]))
						continue ATTR_LIST_OUTPUT_LOOP;
				}

				//帳票管理で作成された属性は非表示
				if(defName.startsWith("app.form.user")){
					continue ATTR_LIST_OUTPUT_LOOP;
				}

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

			// 属性タイプID
			Long id = new Long(attType.getId());

			// カスタム属性(レイアウト)含む属性タイプ
			AttributeTypeLayoutDomain attTypeLayout = attTypeLayoutDomainMap.get(id);

			// リレーション管理の属性タイプ一覧からのリクエストの場合 or ドキュメント管理用でルートオブジェクトタイプが一般ドキュメントではない場合
			if (prmRelationViewFlag != null && prmRelationViewFlag.equals("true") ||
				appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT) && prmDocumentTypeFlag != null && prmDocumentTypeFlag.equals("false")) {
				// オブジェクト型、ユーザ型、コード型の属性タイプは非表示とする(非対応のため)
				switch (attType.getValueType().getId()) {
					// オブジェクト型
					case EIMValueType.OBJECT:
					// ユーザ型
					case EIMValueType.USER:
					// コード型
					case EIMValueType.CODE:
						continue;
				}
			}
			// ドキュメント管理用でルートオブジェクトタイプが一般ドキュメントの場合
			else if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT) && attTypeLayout != null && prmDocumentTypeFlag != null && prmDocumentTypeFlag.equals("true")) {
				// コード型の属性タイプは非表示とする(非対応のため)
				switch (attType.getValueType().getId()) {
					// コード型
					case EIMValueType.CODE:
						continue;
				}
			}

			// 属性タイプのデフォルト値(デフォルト値が取り込まれていないので再取得)
			EIMAttributeType attTypeNew = AttributeUtils.getAttributeTypeById(sess, attType.getId());

			// デフォルト値XML情報
			StringBuffer attValueListStr = new StringBuffer();

			// 初期値XML情報
			StringBuffer attInitValueListStr = new StringBuffer();

			if( attTypeNew != null ) {

				// デフォルト値を返却
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
						// 実数型
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
				if (!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL) && attTypeLayout != null) {

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
								attInitValueListStr.append(" <initValue value=\"" + StringUtils.xmlEncode(initUserValue) + "\"/>" );
							}
							break;
					}
				}
			}

			//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
			if (attValueListStr.length() == 0) {
				attValueListStr.append(" <defValue value=\"\"/>");
			}

			//XML Out
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" valType=\"" + attType.getValueType().getId() + "\"");
			out.println(" valTypeName=\"" + StringUtils.xmlEncode(
						AppValueTypeUtil.getValueTypeName(sess, attType.getValueType().getId())) + "\"");
					out.println(" isMultipleValue=\"" + isMultipleValue + "\"");	// 複数値属性
					out.println(" isMultipleFlag=\"" + isMultipleFlag + "\"");

				// 定義名称生成
				String definitionName = "";
				// コードタイプ名称を取得
				String codeTypeName = "";

				// 汎用の場合
				if (appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

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

					// コードタイプ名称を取得
					if (attTypeLayout.getCodeType() != null) {

						codeTypeName = attTypeLayout.getCodeType().getDefinitionName();
					}

					// UIコントロール名称
					String uiControlName = "";

					String tmpUiControlId = attTypeLayout.getUiControlId();
					if( tmpUiControlId != null ){
						if (uiContorolNameMap.containsKey(tmpUiControlId)) {

							uiControlName = uiContorolNameMap.get(tmpUiControlId);
						}
					}

					out.println(" definitionName=\"" + StringUtils.xmlEncode(definitionName) + "\"");  // 定義名称
					out.println(" codeTypeName=\"" + StringUtils.xmlEncode(codeTypeName) + "\"");      // コードタイプ名称
					out.println(" attTypeEssential=\"" + attTypeLayout.isRequired() + "\"");           // 必須
					out.println(" uiControlName=\"" + StringUtils.xmlEncode(uiControlName) + "\""); // UIコントロール名称
				}

				// ドキュメント管理用の場合
				if (appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {

					// 入力規則
					String inputRuleValue = "";
					String inputRuleFlag = "";

					if (masterExistSet.contains(new Long(attType.getId()))) {

						inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.LIST");	// リスト定義
						inputRuleFlag = "true";
					} else {

						inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.NONE");	// なし
						inputRuleFlag = "false";
					}

					out.println(" inputRuleValue=\"" + inputRuleValue + "\"");		// 入力規則
					out.println(" inputRuleFlag=\"" + inputRuleFlag + "\"");		// リスト定義フラグ
				}

				out.println(">");

			// デフォルト値のリスト
			out.println("<defValueList>");
				out.println(attValueListStr.toString());
			out.println("</defValueList>");

			// ドキュメント管理用・帳票管理用の場合
			if (!appId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {

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
