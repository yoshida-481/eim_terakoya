<%@page contentType="text/xml; charset=UTF-8"%>
<%@page import="app.document.search.EIMDocSearchType"%>
<%@page import="common.bo.TagTreeItem"%>
<%@page import="common.bo.AttributeTree"%>
<%@page import="eim.bo.*"%>
<%@page import="eim.net.*"%>
<%@page import="eim.util.*"%>
<%@page import="java.util.*"%>
<%@page import="eim.util.internal.search.sql.tune.Strategy"%>
<%@page import="eim.util.internal.search.sql.tune.StrategiesStd"%>
<%@page import="eim.util.internal.search.sql.tune.UNUSE__Strategy3ForMergeGroupANDToSetExpCombine"%>
<%@page import="org.apache.commons.logging.*"%>
<%@page import="org.springframework.context.ApplicationContext"%>
<%@page import="common.util.*"%>
<%@page import="eim.bo.EIMSearchSelectEIMObject.*"%>
<%@page import="eim.bo.EIMSearchLimitCountCondition"%>

<%@page import="jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService"%>
<%@page import="jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.util.ResourceUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>

<%@page import="app.document.object.UpdateObjectLink"%>


<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// Session
	EIMSession sess = null;
	EIMUser user = null;

	// Parameter
	String prmObjId = request.getParameter("objId");
	String prmTagObjectIds =  EIMUtils.getParameter(request, "tagObjectIds");
	String prmReturnObjectIds =  EIMUtils.getParameter(request, "returnObjectIds");
	String prmCheckAuth = EIMUtils.getParameter(request, "checkAuth");

	// 権限チェックをするかしないか。権限チェックで権限が無い場合、エラーを返却する
	boolean checkAuth = (prmCheckAuth != null && prmCheckAuth.equals("true") ? true : false);

	//属性ツリー情報の取得
	AttributeTreeUtil.HttpParamParsed attrTreeParam = AttributeTreeUtil.parseHttpParam(request);
	String prmAttrTreePath = EIMUtils.getParameter(request, "attrTreePath");//折り返し用

	// Message
	String message = null;
	Object[] paramId = new Object[3 + attrTreeParam.getParamIds().length];
	paramId[0] = "objId=" + prmObjId;
	paramId[1] = "attrTreePath=" + prmAttrTreePath;
	paramId[2] = "tagObjectIds=" + prmTagObjectIds;
	System.arraycopy(attrTreeParam.getParamIds(), 0, paramId, 3, attrTreeParam.getParamIds().length);

	// Path Attribute
	String pathAttr = "";
	SearchConditionBuildHelper h = new SearchConditionBuildHelper();

	boolean sessPutflg = false;

	try {
		// ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

		// オブジェクト検索Strategyのカスタマイズ
		// StrategiesStd.addTuneStrategies(UNUSE__Strategy3ForMergeGroupANDToSetExpCombine.class);

		// Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		user = (EIMUser) sess.getAttribute("USER");

		//前処理
		EIMThreadContext.removeEIMSession();
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}

		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		// 設定確認
		boolean isDisplayLatestLinkConfig  = AppLogicUtil.isDisplayLatestLink(sess);

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		int limit = -1; // -1の場合nullと同じでSEARCH.RESULT.MAXを使う

		List<EIMObject> objList = null;
		HashMap isDocumentLinkMap = null;
		// リンク更新タイミング取得のためのHashMap。KeyはオブジェクトID、Valueはリンク更新タイミングの値
		HashMap<Long, Long> linkUpdateTimingMap = null;

		// 引数オブジェクトID/属性ツリーIDのチェック
		if (StringUtils.isBlank(prmObjId) && StringUtils.isBlank(attrTreeParam.getAttrTreeId())) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		Map noSTPublicObjMap = new HashMap();	// 公開アイコン表示判定用Map
		Map noWFPublicObjMap = new HashMap();	// 公開アイコン表示判定用Map(OCRのために追加)
		EIMObject parentObj = null;		// ドキュメントリンクで使用する。ワークスペースモード用の場合のみ値が入るので注意

		// リクエストパラメータで返却するオブジェクトが指定されている場合、返却するオブジェクトを以外は削除する。
		Set<String> returnObjIdSet = new HashSet<String>();
		if (prmReturnObjectIds != null) {
			String[] strReturnObjIds = prmReturnObjectIds.split(",");
			for (int i = 0 ; i < strReturnObjIds.length ; i++) {
				returnObjIdSet.add(strReturnObjIds[i]);
			}
		}
		if (!StringUtils.isBlank(prmObjId)) {
			//ワークスペースモード

			// Parent Object
			parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if (parentObj == null) {
				// 選択したワークスペース、フォルダ、またはタグは存在しません。\n「最新の情報に更新」を押してください。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			// アクセス権限チェック
			if (checkAuth) {
				if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(),EIMAccessRole.READ)){
					out.clear();
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			// 自身フォルダのアクセス権限チェック
			helper.checkAccessibleStatusSelf(parentObj, true);

			// Security
			long secId = 0;
			EIMSecurity sec = parentObj.getSecurity();
			if (sec != null) {
				secId = sec.getId();
			}

			if (!StringUtils.isBlank(prmTagObjectIds)) {
				// 【タグ付与対象一覧の場合】

				// Path
				// タグ付与対象一覧の場合はFLEX側で「パス」を生成する

				// オブジェクトIDリストの作成 (直近上位タグ直下から選択対象まで)
				String[] strTagObjIds = prmTagObjectIds.split(",");
				List tagObjIdList = new ArrayList();
				for (int i = 1 ; i < strTagObjIds.length ; i++) {
					tagObjIdList.add(strTagObjIds[i]);
				}

				// 直近上位のタグオブジェクトの取得
				EIMObject tagObj = ObjectUtils.getObjectById(sess, Long.parseLong(strTagObjIds[0]));

				if (tagObj == null) {
					out.clear();
					// 選択したフォルダの直近上位のタグは存在しません。\n「最新の情報に更新」を押してください。
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPARENTTAG.RELOAD");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPARENTTAG.RELOAD");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}

				// タグツリーの取得
				TagTreeItem rootItem = TagUtil.getTagTree(sess, tagObj);

				// タグのソート
				TagUtil.sortTagTree(sess, rootItem, false, helper);

				// 指定階層の取得
				objList = TagUtil.getTagTreeForTarget(sess, rootItem, tagObjIdList);

				// タグ内のドキュメントのうち、公開アイコンの判定のため、
				// ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
				// OCR用に追加
				List documentObjList = new ArrayList();
				for (Iterator i = objList.iterator(); i.hasNext();) {
					EIMObject childObj = (EIMObject) i.next();
					if (helper.isTypeOfDocument(childObj.getType())) {
						documentObjList.add(childObj);
					}
				}

				//ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
				//これに非該当のドキュメントは公開アイコンを表示しない
				if(documentObjList.size() > 0){
					List<Long> objIdList = new ArrayList<>();
					for (int i = 0; i < documentObjList.size(); i ++) {
						EIMObject document = (EIMObject) documentObjList.get(i);
						objIdList.add(new Long(document.getId()));
					}
					String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objIdList);

					EIMSearchSelectEIMObject noSTEditObjSelect = new EIMSearchSelectEIMObject();
					noSTEditObjSelect.setCondition(
							h.group(h.opAnd()).addCondition(
									new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
								);

					noSTEditObjSelect.setResultAttrs(new ArrayList());
					List noSTPublicObjList = SearchUtils.searchObjects(sess, noSTEditObjSelect, new EIMSearchLimitCountCondition(-1, false));// タグ配下のため-1(SEARCH.RESULT.MAXを使う)
					for(int i = 0; i< noSTPublicObjList.size() ;i ++){
						noWFPublicObjMap.put(((EIMObject)noSTPublicObjList.get(i)).getId(),"");
					}
				}

			} else {
				// 【タグ付与対象一覧以外の場合】

				// Path
				pathAttr = AppObjectUtil.getPath(parentObj);
				if (pathAttr == null) {
					// ワークスペースの場合、パス属性の値を保持していない
					pathAttr = "/";
				}
				pathAttr += parentObj.getName() + "/";

				// アクセス可能な子のリストを取得し、タグ、フォルダ、ドキュメント(ドキュメントリンク含む)、ワークスペース固有ごみ箱の順に並び替え
				isDocumentLinkMap = new HashMap();
				linkUpdateTimingMap = new HashMap<Long, Long>();

				// 指定したオブジェクトがごみ箱またはワークスペース固有ごみ箱の場合、検索条件追加プラグインは使用しない
				if(parentObj.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_RECYCLE")) ||
						parentObj.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
					// ごみ箱配下は上限を「TRASH_BOX_DISP_LIMIT」で指定する（SEARCH.RESULT.MAXは効かなくする）
					if (EIMConfig.get("TRASH_BOX_DISP_LIMIT") != null ) {
						limit = Integer.parseInt(EIMConfig.get("TRASH_BOX_DISP_LIMIT"));
					}
					EIMSearchLimitCountCondition limitForTrash = new EIMSearchLimitCountCondition(limit, true);
					objList = helper.getChildObjectsWithDocLinkInAccessibleStatus(parentObj, isDocumentLinkMap, limitForTrash);
				}else{
					objList = helper.getChildObjectsWithDocLinkInAccessibleStatusByCondMaker(parentObj, isDocumentLinkMap,
																							EIMDocSearchType.DISPLAY_LISTVIEW, linkUpdateTimingMap);
				}

				List tagObjList = new ArrayList(objList.size());
				List folderObjList = new ArrayList(objList.size());
				List documentObjList = new ArrayList(objList.size());
				EIMObject wsRecycleObj = null;

				for (Iterator i = objList.iterator(); i.hasNext();) {
					EIMObject childObj = (EIMObject) i.next();

					if (returnObjIdSet.isEmpty() || returnObjIdSet.contains(String.valueOf(childObj.getId()))) {
						if (helper.isTypeOfTag(childObj.getType())) {
							tagObjList.add(childObj);
						}
						else if (helper.isTypeOfFolder(childObj.getType())) {
							folderObjList.add(childObj);
						}
						else if (helper.isTypeOfDocument(childObj.getType())) {
							documentObjList.add(childObj);
						}
						else if (helper.isTypeOfWorkspaceRecycle(childObj.getType())){
							wsRecycleObj = childObj;
						}
					}

					i.remove();
				}
				if (prmAttrTreePath == null) {
					// 属性ツリーの場合、タグは表示しない
					objList.addAll(tagObjList);
				}
				objList.addAll(folderObjList);
				objList.addAll(documentObjList);
				if (wsRecycleObj != null) {
					objList.add(wsRecycleObj);
				}

				//ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
				//これに非該当のドキュメントは公開アイコンを表示しない
				if (documentObjList.size() > 0) {
					List<Long> objIdList = new ArrayList<>();
					for (int i = 0; i < documentObjList.size(); i ++) {
						EIMObject document = (EIMObject) documentObjList.get(i);
						objIdList.add(new Long(document.getId()));
					}
					String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objIdList);

					EIMSearchSelectEIMObject noSTEditObjSelect = new EIMSearchSelectEIMObject();
					noSTEditObjSelect.setCondition(
							h.group(h.opAnd()).addCondition(
									new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
								);

					noSTEditObjSelect.setResultAttrs(new ArrayList());

					List noSTPublicObjList = SearchUtils.searchObjects(sess, noSTEditObjSelect, new EIMSearchLimitCountCondition(limit, false));// ごみ箱配下以外は-1(SEARCH.RESULT.MAXを使う)

					for(int i = 0; i< noSTPublicObjList.size() ;i ++){
						noSTPublicObjMap.put(((EIMObject)noSTPublicObjList.get(i)).getId(),"");
						noWFPublicObjMap.put(((EIMObject)noSTPublicObjList.get(i)).getId(),"");
					}
				}
			}

			// Root Node
			out.print("<objList ");
			out.print("path=\"" + StringUtils.xmlEncode(pathAttr) + "\" secId=\"" + secId + "\"" + " tagObjIds=\"" + prmTagObjectIds + "\"" + " objId=\"" + prmObjId + "\""
				+ " objTypeName=\"" + (helper.isTypeOfFolder(parentObj.getType())?helper.getObjTypeNameFolderXmlEscaped():StringUtils.xmlEncode(parentObj.getType().getDefName())) + "\"");
		} else {
			//属性ツリーモード
			if (StringUtils.isBlank(attrTreeParam.getAttrTreeId())) {
				message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.NOATTRTREE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOATTRTREE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			AttributeTree attrTree = null;
			try {
				//パラメータのチェックと属性ツリーの取得
				attrTree = AttributeTreeUtil.checkAndGetAttrTree(sess, attrTreeParam);
			} catch (eim.bo.EIMException e) {
				if (!AttributeTreeUtil.isErrorOfCheckAndGetAttrTree(e))
					throw e;
				message = e.getMessage();
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage(e.getMessageKey());
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			if (attrTreeParam.getAttrTreeValues().size() != attrTree.getTreeItemList().size()) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			//属性値リストを型変換
			List attrTreeValues = new ArrayList(attrTreeParam.getAttrTreeValues());
			AttributeTreeUtil.convertAttrTreeValueToDataType(attrTree, attrTreeValues);
			//属性値に一致するオブジェクトを取得
			objList = AttributeTreeUtil.getClassifiedObjects(sess, attrTree, attrTreeValues);

			// Root Node
			out.print("<objList objTypeName=\"属性\"");

			int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM"));	// 最大取得件数

			// <最大取得件数>を超過した場合、余分な最後の1件は除去
			if (objList.size() > maxnum) {
				objList.remove(maxnum);
				// 最大取得件数超過ラベルの設定
				String[] maxNumArray = {String.valueOf(maxnum)};
				out.println(" overflowLabel=\"" + EIMResource.getMessage(sess, "EIM.LABEL.ATTRTREE.OVERFLOW", maxNumArray) + "\"");
			}

			// 属性ツリービュー内のドキュメントのうち、公開アイコンの判定のため、
			// ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
			// OCR用に追加
			List documentObjList = new ArrayList();
			for (Iterator i = objList.iterator(); i.hasNext();) {
				EIMObject childObj = (EIMObject) i.next();
				if (returnObjIdSet.isEmpty() || returnObjIdSet.contains(String.valueOf(childObj.getId()))) {
					if (helper.isTypeOfDocument(childObj.getType())) {
						documentObjList.add(childObj);
					}
				}
			}

			//ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
			//これに非該当のドキュメントは公開アイコンを表示しない
			if (documentObjList.size() > 0) {
				List<Long> objIdList = new ArrayList<>();
				for (int i = 0; i < documentObjList.size(); i ++) {
					EIMObject document = (EIMObject) documentObjList.get(i);
					objIdList.add(new Long(document.getId()));
				}
				String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objIdList);

				EIMSearchSelectEIMObject noSTEditObjSelect = new EIMSearchSelectEIMObject();
				noSTEditObjSelect.setCondition(
						h.group(h.opAnd()).addCondition(
								new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
							);

				noSTEditObjSelect.setResultAttrs(new ArrayList());
				List noSTPublicObjList = SearchUtils.searchObjects(sess, noSTEditObjSelect, new EIMSearchLimitCountCondition(-1, false));// 属性ツリーのためlimit=-1(SEARCH.RESULT.MAXを使う)
				for(int i = 0; i< noSTPublicObjList.size() ;i ++){
					noWFPublicObjMap.put(((EIMObject)noSTPublicObjList.get(i)).getId(),"");
				}
			}

		}

		// Root Nodeの続き。属性情報を折り返す
		if (attrTreeParam.getAttrTreeId() != null)
			out.print(" attrTreeId=\"" + attrTreeParam.getAttrTreeId() + "\"");
		for (int i = 0;i < attrTreeParam.getAttrTreeValues().size(); i++) {
			out.print(" attrTreeValue" + (i + 1) + "=\"" + StringUtils.xmlEncode((String)attrTreeParam.getAttrTreeValues().get(i)) + "\"");
		}
		if (prmAttrTreePath != null)
			out.print(" attrTreePath=\"" + StringUtils.xmlEncode(prmAttrTreePath) + "\"");

		out.println(">");

		// essential attribute
		EIMAttributeType attTypeOfCreaterId = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));

		// Document essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));
		EIMAttributeType attTypeOfSearchIndex = AttributeUtils.getAttributeTypeByName(sess,
			EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX"));

		// 予め属性表示色オブジェクトを一括取得
		HashMap displayColorSet = new HashMap();
		if (objList != null && objList.size() > 0) {
			displayColorSet = DisplayColorUtil .getDisplayColorSet(sess, objList);
		}

		long[] idArray = new long[objList.size()];
		long wsRecycleObjId = 0;
		int i = 0;
		for (Iterator it = objList.iterator(); it.hasNext();) {
			EIMObject object = (EIMObject) it.next();
			if(object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
				// ワークスペース固有ごみ箱のオブジェクトIDを格納
				wsRecycleObjId = object.getId();
			} else {
				// ワークスペース固有ごみ箱以外のオブジェクトIDを格納
				idArray[i] = object.getId();
				i++;
			}
		}
		

		//読み取りのみ値が入ったMap
		Map readMap = new HashMap();
		for (Iterator it = objList.iterator(); it.hasNext();) {
			EIMObject object = (EIMObject) it.next();
			readMap.put(object.getId(),0);
		}
		// ワークスペース固有ごみ箱以外の検索条件
		EIMSearchSelectEIMObject selectTargetAlways = new EIMSearchSelectEIMObject();
		selectTargetAlways.setCondition(
				h.group(h.opAnd())
				.addCondition(h.in(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, h.opIn(), TypeConvertUtils.convertToBuildTypeArray(idArray)))
			);
		selectTargetAlways.setRole(EIMAccessRole.READ);
		selectTargetAlways.setResultAttrs(new ArrayList());
		selectTargetAlways.setRole(AppConstant.ACCESS_ROLE_ALWAYS_READ);
		
		// ワークスペース固有ごみ箱の検索条件
		EIMSearchSelectEIMObject wsRecycleSelectTargetAlways = new EIMSearchSelectEIMObject();
		
		wsRecycleSelectTargetAlways.setCondition(
				h.group(h.opAnd())
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,wsRecycleObjId ))
			);
		wsRecycleSelectTargetAlways.setRole(EIMAccessRole.READ);
		wsRecycleSelectTargetAlways.setResultAttrs(new ArrayList());
		wsRecycleSelectTargetAlways.setRole(AppConstant.ACCESS_ROLE_ALWAYS_READ);
		List alwaysList;
		List wsRecyclealwaysList;
		if(objList.size() > 0){
			alwaysList = SearchUtils.searchObjects(sess, selectTargetAlways, new EIMSearchLimitCountCondition(limit, true)); // ごみ箱配下以外はlimit=-1(SEARCH.RESULT.MAXを使う)
			wsRecyclealwaysList = SearchUtils.searchObjects(sess, wsRecycleSelectTargetAlways, new EIMSearchLimitCountCondition(-1, true));
			alwaysList.addAll(wsRecyclealwaysList);
		}
		else{
			alwaysList = new ArrayList();
		}
		for (Iterator it = alwaysList.iterator(); it.hasNext();) {
			EIMObject object = (EIMObject) it.next();
			if(readMap.containsKey(object.getId())){
				readMap.remove(object.getId());
			}
		}

		ApplicationContext applicationContext = ApplicationContextLoader.getApplicationContext();
		PublicDocumentService publicDocumentService = (PublicDocumentService)applicationContext.getBean("publicDocumentService");

		// PDF変換中オブジェクトIDのSetを用意する
		Set<Long> pdfConversionObjectIdSet = publicDocumentService.getPDFConversionProcessingObjectIdSet(
			AppObjectUtil.getIdList(objList));

		//作成者用のマップを用意する
		HashMap<Long, EIMUser> createUserMap = new HashMap<Long, EIMUser>();
		//上位WFフォルダ用のマップを用意する
		HashMap<Long, EIMObject> higherWFFolderMap = new HashMap<Long, EIMObject>();

		// ファイル情報のマップを用意する
		List<EIMObject> docList = new ArrayList<EIMObject>();
		for (Iterator it = objList.iterator(); it.hasNext();) {
			// Object
			EIMObject object = (EIMObject) it.next();

			if (helper.isTypeOfDocument(object.getType())) {
				docList.add(object);
			}
		}
		Map<Long, FileDomain> originalFileMap = AppFileUtils.getObjectIdAndFileMap(AppFileUtils.getOriginalFileList(sess, docList));
		Map<Long, FileDomain> publicFileMap = AppFileUtils.getObjectIdAndFileMap(AppFileUtils.getPublicFileList(sess, docList));

		// バージョンを一括取得
		List<EIMObject> documentList = new ArrayList<>();
		for (Iterator<EIMObject> it = objList.iterator(); it.hasNext();) {
			// Object
			EIMObject object = (EIMObject) it.next();
			if (helper.isTypeOfDocument(object.getType()) && object.getStatus() == null) {
				documentList.add(object);
			}
		}
		Map<Long, EIMVersion> versionMap = AppObjectUtil.getVersionMapByObjects(sess, documentList);

		// Folder, Document, Tag, WorkspaceRecycle
		for (Iterator<EIMObject> it = objList.iterator(); it.hasNext();) {
			// Object
			EIMObject object = (EIMObject) it.next();

			boolean isFolderType = helper.isTypeOfFolder(object.getType());
			boolean isTagType = helper.isTypeOfTag(object.getType());
			boolean isDocument = false;
			boolean isWSRecycle = helper.isTypeOfWorkspaceRecycle(object.getType());

			// XML
			out.println("<object");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");


			if( isFolderType ) {	// フォルダの場合は「フォルダ」を返却
				out.println(" objTypeName=\"" + helper.getObjTypeNameFolderXmlEscaped()+ "\"");
			}
			else if( isTagType ) {	// タグの場合は「タグ」を返却
				out.println(" objTypeName=\"" + helper.getObjTypeNameTagXmlEscaped()+ "\"");
			}
			else if ( isWSRecycle ) { //ワークスペース固有ごみ箱の場合はタイプ名称を返却
				out.println(" objTypeName=\"" + helper.getObjTypeNameWorkspaceRecycleXmlEscaped()+ "\"");
			}
			else {	// ドキュメントの場合はタイプ名称を返却
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
				isDocument = true;

			}
			out.println(" isDocument=\"" + isDocument + "\"");
			if ( isWSRecycle ) {
				// 英語表記への対応のため「ごみ箱」オブジェクトタイプからオブジェクト名称を取得
				EIMObjectType objTypeRecycle = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));
				out.println(" objName=\"" + StringUtils.xmlEncode(objTypeRecycle.getName()) + "\"");
			} else  {
				out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			}
			out.println(" rev=\"" + ((isFolderType||isTagType) ? "-" : String.valueOf(object.getRev()))
					+ "\"");
			out.println(" isListViewItem=\"true\"");	// ファイル一覧の選択アイテムか否か

			//PDF結合に失敗したか否か
			boolean isPDFJoinFailed = false;
			long joinFailFlg = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfPDFJoinFail(), AppConstant.FLAG_OFF);
			if(joinFailFlg == AppConstant.FLAG_ON) {
				isPDFJoinFailed = true;
			}

			// Status
			String statusTypeName = "";
			Date pdfConvExecDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
			if ((object.getStatus() != null) && !helper.isTypeOfFolderUnderFolderWithWorkflow(object)) {
				//ワークフローを持つもの
				//ただしワークフロー付きフォルダの下のフォルダを除く

				boolean isConvertFailed = false;
				statusTypeName = object.getStatus().getType().getName();
				long pdfFailFlg = AppObjectUtil.getIntAttr(sess, object,
					helper.getAttrNameOfPubProcFail(), AppConstant.FLAG_OFF);

				if (pdfConversionObjectIdSet.contains((long)object.getId())) {
					if (object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
						// 公開処理中以外の場合は(PDF変換中)を出力
	 					statusTypeName += ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.PROCESSING"); // (PDF変換中)
					}
				} else if (pdfFailFlg == AppConstant.FLAG_ON) {
					if (pdfConvExecDate == null || pdfConvExecDate.getTime() > object.getModifyDate().getTime()) {
						statusTypeName += ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.FAILED"); // (PDF変換失敗)
						isConvertFailed = true;
					}
				}

				if( isPDFJoinFailed ) {
					statusTypeName = EIMResource.getMessage(sess, "EIM.LABEL.PDF.JOIN.FAILED"); // (PDF結合失敗)
				}

				out.println(" statusId=\"" + object.getStatus().getId() + "\"");
				out.println(" statusTypeId=\"" + object.getStatus().getType().getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusTypeName) + "\"");
				out.println(" statusTypeKind=\"" + object.getStatus().getType().getKind() + "\"");
				out.println(" isConvertFailed=\"" + isConvertFailed + "\"");
				out.println(" isPDFJoinFailed=\"" + isPDFJoinFailed + "\"");
				out.println(" isDspPubIconForNoWF=\"false\""); // WFなしドキュメントの公開アイコン表示フラグ

			} else {
				statusTypeName = "-";

				if( isPDFJoinFailed ) {
					statusTypeName = EIMResource.getMessage(sess, "EIM.LABEL.PDF.JOIN.FAILED"); // (PDF結合失敗)
				}

				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusTypeName) + "\"");
				out.println(" statusTypeKind=\"\"");
				out.println(" isPDFJoinFailed=\"" + isPDFJoinFailed + "\"");
				// WFなしドキュメントの公開アイコン表示フラグ
				if( isPDFJoinFailed ){
					out.println(" isDspPubIconForNoWF=\"false\""); // PDF結合失敗のときは表示しない
				}
				else{

					out.println(" isDspPubIconForNoWF=\"" + (!(isFolderType||isTagType) && noSTPublicObjMap.containsKey(object.getId())) + "\"");// ドキュメントでのみtrue
				}

			}

			// Create User
			String createUserName = "";
			if (isFolderType) {
				createUserName = object.getCreateUser().getName();
			} else {
				if (object.getAttribute("作成者") != null) {
					long createUserId = object.getAttribute("作成者").getInt();
					EIMUser createUser = createUserMap.get((long)createUserId);
					if(createUser == null) {
						createUser = UserUtils.getUserById(sess, createUserId);
						createUserMap.put(createUserId, createUser);
					}
					createUserName = createUser.getName();
				}
			}
			out.println(" createUserName=\"" + StringUtils.xmlEncode(createUserName) + "\"");

			// Modify User & date
			out.println(" modifyUserName=\""
					+ StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
			out.println(" modifyDate=\"" + modifyDate + "\"");
			// ソート用更新日付
			String modifyDateTime = String.valueOf(object.getModifyDate().getTime() / 1000);
			out.println(" modifyDateTime=\"" + modifyDateTime + "\"");
			// 作成日時
			String createDateTime = String.valueOf(object.getCreateDate().getTime() / 1000);
			out.println(" createDateTime=\"" + createDateTime + "\"");

			// 上位WFフォルダ(higherWFFolder)出力
			long higherWFFolderId = helper.getUpperFolderWithWorkflowObjId(object);
			out.println(" higherWFFolder=\""
					+ ((higherWFFolderId != -1)
							? String.valueOf(higherWFFolderId) : "") + "\"");
			// 上位WFフォルダステータス
			if( higherWFFolderId != -1 ) {
				EIMObject higherWFFolderObj = higherWFFolderMap.get((long)higherWFFolderId);
				if(higherWFFolderObj == null) {
					higherWFFolderObj = ObjectUtils.getObjectById(sess, higherWFFolderId);
					higherWFFolderMap.put(higherWFFolderId, higherWFFolderObj);
				}
				if (higherWFFolderObj != null) {
					out.println(" higherWFFolderStatusTypeKind=\"" + higherWFFolderObj.getStatus().getType().getKind() + "\"");
				}
			}
			// 自身がワークフローフォルダかどうかを出力(isWorkflowFolder)
			if (isFolderType) { // for Folder
				out.println(" isWorkflowFolder=\"" + helper.isTypeOfFolderWithWorkflow(object)
						+ "\"");
			} else {// for Document and Tag
				out.println(" isWorkflowFolder=\"false\"");
			}

			// 自身がドキュメントリンクかどうかを出力(isDocumentLink)
			boolean isDocumentLink = false;
			if( isDocumentLinkMap != null ) {
				String str = (String) isDocumentLinkMap.get(object);
				if( str != null ) {
					out.println(" isDocumentLink=\"" + str + "\"");
					isDocumentLink = true;
				}
				// パス属性: PDF結合ダイアログ、タグ付与画面で使用 (ドキュメントリンクの場合は親からパスを取得)
				out.println(" path=\"" + StringUtils.xmlEncode(pathAttr) + "\"");


				// ドキュメント本体のパスを取得する
				String paths[] = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStrings();
				// 1件目がドキュメント本体のパス
				out.println(" realDocumentPath=\"" + StringUtils.xmlEncode(paths[0]) + "\"");
			} else {
				// ドキュメントリンクではない場合
				out.println(" isDocumentLink=\"false\"");
				// パス属性: PDF結合ダイアログ、タグ付与画面で使用 (ドキュメントリンク以外の場合は自分自身のパスでOK)
				out.println(" path=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(object)) + "\"");
			}

			// リンクの親オブジェクトID (linkParentObjId)
			String linkParentObjId = "-";
			String linkDspLatest = "false";
			String linkUpdateTiming = "0";	//0:手動 1:自動
			String linkObjVersionId = "-";
			if(isDocumentLink) {
				linkParentObjId = prmObjId;

				// ドキュメントリンクの場合
				if( isDocumentLinkMap != null ) {
					String str = (String) isDocumentLinkMap.get(object);
					if( str != null && str.equals("true") ) {

						// リンクの過去リビジョンを通知するかどうか
						if(isDisplayLatestLinkConfig){
							if( !object.getLatest() ) {
								linkDspLatest = "true";
							}
						}

						// リンクのリンク更新タイミングをアイコンで表示する
						// prmObjIdがある場合のみここに入る。そのためparentObjは取得済み。
						linkUpdateTiming = linkUpdateTimingMap.get((long)object.getId()).toString();

						// versionId取得
						EIMVersion version = versionMap.get(new Long(object.getId()));
						if (version != null) {
							linkObjVersionId = String.valueOf(version.getId());
						}
					}
				}
			}
			out.println(" linkParentObjId=\"" + linkParentObjId + "\"");
			out.println(" isDspLatestLink=\"" + linkDspLatest + "\"");
			out.println(" documentLinkUpdateTiming=\"" + linkUpdateTiming + "\"");
			out.println(" linkObjVersionId=\"" + linkObjVersionId + "\"");

			//Document only
			if (!(isFolderType||isTagType||isWSRecycle)) {
				FileDomain originalFile = originalFileMap.get((long)object.getId());
				FileDomain publicFile = publicFileMap.get((long)object.getId());

				// Lock User
				if (object.getLockUser() != null) {
					out.println(" lockUserName=\""
							+ StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
					out.println(" lockDate=\"" + object.getLockDate() + "\"");
					// ログインユーザがロックしている場合
					boolean isLockedMyself = object.getLockUser().getId() == sess.getUser().getId() ? true : false;
					out.println(" isLockedMyself=\"" + isLockedMyself + "\"");
				}else{
					out.println(" lockUserName=\""+   "\"");
				}

				// PDFアイコンの表示判定
				boolean isDspPdfIcon = AppLogicUtil.isDspPdfIcon(publicFile);
				out.println(" isDspPdfIcon=\"" + isDspPdfIcon + "\"");

				// PDF変換ステータス出力
				int pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_NONE;
				long pdfFailFlg = AppObjectUtil.getIntAttr(sess, object,
						helper.getAttrNameOfPubProcFail(), AppConstant.FLAG_OFF);
				Date pdfPreRegistDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));

				if (pdfConversionObjectIdSet.contains((long)object.getId())) {
					// PDF変換中
					pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESSING;

				} else if (pdfFailFlg == AppConstant.FLAG_ON) {
					if (pdfConvExecDate == null || pdfConvExecDate.getTime() > object.getModifyDate().getTime()) {
						// PDF失敗
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_FAILURE;
					}
				} else if (isDspPdfIcon) {
					// PDF変換完了
					if (pdfConvExecDate != null && pdfConvExecDate.getTime() <= object.getModifyDate().getTime()) {
						// PDF変換処理実行日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else {
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					}
					// 公開PDF事前登録日時が存在する場合はその判定を優先する
					if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() <= object.getModifyDate().getTime()) {
						// 公開PDF事前登録日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() > object.getModifyDate().getTime()) {
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					}
				}
				// nullでなければ公開PDF事前登録済み
				boolean isPdfPreRegistered = pdfPreRegistDate != null ? true : false;
				out.println(" isPdfPreRegistered=\"" + isPdfPreRegistered + "\"");

				out.println(" pdfConversionStatus=\"" + pdfConversionStatus + "\"");

				// Fileのサイズ
				long fSize = 0;
				if( originalFile != null )	//結合失敗ドキュメントの場合はFileDomainを取得できないことによる処置
					fSize = originalFile.getSize();

				//「サイズ」
				out.println(" fileSize=\"" + fSize + "\"");

				out.println(" readOnly=\"" + readMap.containsKey(object.getId()) + "\"");
				// 公開済みフラグ: PDF結合ダイアログのために追加
				boolean isPublished = AppLogicUtil.isPublished(sess, object);
				out.println(" isPublished=\"" + isPublished + "\"");

				/**
				 * 右クリックメニューのURLコピー(原本・公開)/ショートカット出力(原本・公開)が実行可の場合true
				 * 属性画面の原本/公開ファイルURLテキストボックスの表示条件と同じ
				 */
				boolean isDispDirectURL = true;

				if(object.getStatus() != null){
					// WFありの場合

					if(object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
						isDispDirectURL = false;
					}
				}else{
					// WFなしの場合

					// バージョンを取得
					EIMVersion version = versionMap.get(new Long(object.getId()));

					if(version != null && version.getList().size() > 1){
						// 1より多い場合

						// 最新履歴番号オブジェクトを取得
						EIMObject latestObject = version.getLatest();
						// 最新履歴番号の１つ前のオブジェクトを取得
						EIMObject latestPrevObject = version.getObjectByRev(latestObject.getRev() -1);

						// １つ前のオブジェクトにロックユーザーが設定されている場合
						if(latestPrevObject.getLockUser() != null && latestPrevObject.getLockDate() != null){

							// 引数と最新履歴番号のオブジェクトが同じ場合
							if(object.getId() == latestObject.getId()){
								isDispDirectURL = false;;
							}
						}
					}
				}

				if (isDspPdfIcon) {
					// 右クリックメニューで原本・公開のURL/ショートカット出力可のため、公開ファイル名を設定する
					String publicFileName = "";
					if (publicFile != null) {
						publicFileName = publicFile.getName();
					}

					// 公開ファイル名の有無で、右クリックメニュー側の処理で公開・原本の出力実施可否判定を行う
					out.println(" publicFileName=\"" + StringUtils.xmlEncode(publicFileName) + "\"");
				}

				// OCR処理ステータス
				EIMAttribute attributeOcrProcessStatus = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"));
				if(attributeOcrProcessStatus != null){
					out.println(" ocrProcessStatus=\"" + attributeOcrProcessStatus.getInt() + "\"");
				}else{
					out.println(" ocrProcessStatus=\"\"");
				}

				// OCR結果ステータス
				EIMAttribute attributeOcrResultStatus = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"));
				if(attributeOcrResultStatus != null){
					out.println(" ocrResultStatus=\"" + attributeOcrResultStatus.getInt() + "\"");
				}else{
					out.println(" ocrResultStatus=\"\"");
				}

				// WFなしドキュメントの公開フラグ：OCRのために追加
				boolean isNoWFPublic = false;
				if(noWFPublicObjMap.containsKey(object.getId())){
					isNoWFPublic = true;
				}
				out.println(" isNoWFPublic=\"" + isNoWFPublic + "\"");

				// 番号
				String number = "";
				if (object.getAttribute("番号") != null) {
					number = object.getAttribute("番号").getString();
				}
				out.println(" number=\"" + StringUtils.xmlEncode(number) + "\"");

				// WebDAVロックフラグ
				boolean isWebDAVLock = false;
				if (object.getAttribute("WebDAVロックフラグ") != null) {
					isWebDAVLock = object.getAttribute("WebDAVロックフラグ").getInt() == AppConstant.FLAG_ON ? true : false;
				}
				out.println(" isWebDAVLock=\"" + isWebDAVLock + "\"");

				// スキャン用表紙フラグ（紙文書電子化オプションのスキャン用表紙かどうか）
				boolean isCoverForScanning = false;
				EIMAttribute attributeCoverForScan = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_COVER_FOR_SCAN"));
				if (attributeCoverForScan != null) {
					isCoverForScanning = attributeCoverForScan.getInt() == AppConstant.FLAG_ON ? true : false;
				}
				out.println(" isCoverForScanning=\"" + isCoverForScanning + "\"");

			}

			// Property
			String property = "";
			if (object.getAttribute("プロパティ") != null) {
				property = object.getAttribute("プロパティ").getString();
			}
			out.println(" property=\"" + StringUtils.xmlEncode(property) + "\"");

			/*
			 * 有効期限切れ判定
			 */
			boolean expiration = false;
			EIMAttribute expirationDate = object.getAttribute(helper.getAttrNameOfEffectDate());
			if (expirationDate != null) {
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
				out.println(" effectiveTerm=\"" + expirationDate.getDate() + "\"");
			}
			out.println(" expiration=\"" + expiration + "\"");

			// 署名・暗号化状態
			long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			out.println(" signencr=\"" + signencr + "\"");

			// 以下、属性値(単一値)の出力
			List attList = object.getAttributeList();
			if (attList == null)
				attList = new ArrayList();

			// 作成者IDは差し替えの為、捨てる
			for (Iterator j = attList.iterator(); j.hasNext();) {
				if (((EIMAttribute) j.next()).getType().getId() == attTypeOfCreaterId.getId())
					j.remove();
			}

			StringBuffer attTypeIdCSV = new StringBuffer();
			for (Iterator j = attList.iterator(); j.hasNext();) {
				if (attTypeIdCSV.length() > 0)
					attTypeIdCSV.append(",");
				attTypeIdCSV.append(((EIMAttribute) j.next()).getType().getId());
			}
			if (attTypeIdCSV.length() > 0)
				out.println(" attTypeIdCSV=\"" + attTypeIdCSV.toString() + "\"");
			// ダミーの属性値
			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\""
					+ StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate
					+ "\""//
					+ " attType_" + attTypeOfModifyDate.getId() + "_datetime=\""
					+ modifyDateTime + "\"");
			out.println(" attType_" + attTypeOfCreaterId.getId() + "=\""
					+ StringUtils.xmlEncode(createUserName) + "\"");// for

			if(isDocument == true ){
				out.println(" attType_" + attTypeOfSearchIndex.getId() + "=\"" + object.getId() + "\"");//文書ID
			}
			// ユーザーテーブルの作成日列
			String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate
					+ "\""//
					+ " attType_" + attTypeOfCreateDate.getId() + "_datetime=\""
					+ (object.getCreateDate().getTime() / 1000) + "\" ");

			// 属性(単一値)一覧
			for (Iterator j = attList.iterator(); j.hasNext();) {
				EIMAttribute att = (EIMAttribute) j.next();
				EIMAttributeType attType = att.getType();
				if (attType.isMultiple())
					continue;// 複数値は後で出力

				String xmlValue = null;
				String datetimeValue = null;
				switch (attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER:
					xmlValue = String.valueOf(att.getInt());
					break;
				// 文字列型
				case EIMValueType.STRING:
					xmlValue = StringUtils.xmlEncode(att.getString());
					break;
				// 日付型
				case EIMValueType.DATE:
					/* Chenge Date Format */
					xmlValue = DateUtils.getDBTzToCLTzDate(sess, att.getDate());
					datetimeValue = String.valueOf(att.getDate().getTime() / 1000);
					break;
				// テキスト型
				case EIMValueType.TEXT:
					xmlValue = StringUtils.xmlEncode(att.getText());
					break;
					// ダブル型
				case EIMValueType.DOUBLE:
					xmlValue = FormatUtil.getDoubleFormatedString(att.getDouble());
					break;
				}

				// XML出力
				out.println(" attType_" + attType.getId() + "=\"" + xmlValue + "\"");

				// 属性表示色があるかどうか
				String objName = Long.toString(object.getId()) + "_" + attType.getId();
		    	EIMObject tmpObj = (EIMObject)displayColorSet.get(objName);
				if (tmpObj != null) {
					EIMAttribute tmpAttr = tmpObj.getAttribute(EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"));
					if(tmpAttr != null) {
						String dispColor = tmpAttr.getString();
						out.println(" attType_" + attType.getId() + "_color=\"" + dispColor + "\"");
					}
				}

				if (datetimeValue != null)
					out.println(" attType_" + attType.getId() + "_datetime=\"" + datetimeValue
							+ "\"");
			}
			out.println(">");

			// 属性(複数値)一覧
			for (Iterator j = attList.iterator(); j.hasNext();) {
				EIMAttribute att = (EIMAttribute) j.next();
				EIMAttributeType attType = att.getType();
				if (!attType.isMultiple())
					continue;// 単一値は出力済み

				List valList = new ArrayList();
				List dateTimeList = null;
				switch (attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER: {
					long[] values = TypeConvertUtils.convertToLongArray(att.getInts());
					for (int k = 0; k < values.length; k++) {
						valList.add(String.valueOf(values[k]));
					}
				}
					break;
				// 文字列型
				// テキスト型
				case EIMValueType.STRING:
				case EIMValueType.TEXT: {
					String[] values = (attType.getValueType().getId() == EIMValueType.STRING)
							? att.getStrings() : att.getTexts();
					for (int k = 0; k < values.length; k++) {
						valList.add(StringUtils.xmlEncode(values[k]));
					}
				}
					break;

					// ダブル型
				case EIMValueType.DOUBLE: {
					double[] values = att.getDoubles();
					for (int k = 0; k < values.length; k++) {
						valList.add(FormatUtil.getDoubleFormatedString(values[k]));
					}
					}
					break;

				// 日付型
				case EIMValueType.DATE:
					if (dateTimeList == null)
						dateTimeList = new ArrayList();
					Date[] values = att.getDates();
					for (int k = 0; k < values.length; k++) {
						/* Chenge Date Format */
						valList.add(DateUtils.getDBTzToCLTzDate(sess, values[k]));
						dateTimeList.add(String.valueOf(values[k].getTime() / 1000));
					}
					break;


				}

				// XML書き出し
				out.println("  <attType_" + attType.getId() + "_multivalue>");
				for (int k = 0; k < valList.size(); k++) {
					out.println("    <attValue value=\"" + valList.get(k)
							+ "\""//
							+ ((dateTimeList != null) ? (" value_datetime=\""
									+ dateTimeList.get(k) + "\"") : "")//
							+ ">"//
							+ "</attValue>");
				}
				out.println("  </attType_" + attType.getId() + "_multivalue>");
			}
			out.println("</object>");
		}

		// End Root Node
		out.println("</objList>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch (Exception e) {
		try {out.clear();} catch (Exception eee){}
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		// オブジェクト検索Strategyの初期化
		// StrategiesStd.resetTuneStrategies();
		try {
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if (sess != null) {
				sess.close();
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
