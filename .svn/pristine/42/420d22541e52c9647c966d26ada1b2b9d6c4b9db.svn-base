<%@page import="eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper"%>
<%@page import="common.util.AppLogicUtil.ProcessFolderTreeWalker"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.service.DocumentFormService"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.FormDomain"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;

	//Parameter
	String prmObjId[] = request.getParameterValues("objId");
	String prmIsFolder[] = request.getParameterValues("isFolder");
	String prmParentObjId[] = request.getParameterValues("parentObjId");
	String prmIsDocumentLink[] = request.getParameterValues("isDocumentLink");

	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
		paramIdList.add("isFolder[" + i + "]=" + prmIsFolder[i]);
		paramIdList.add("parentObjId=[" + i + "]=" + prmParentObjId[i]);
		paramIdList.add("isDocumentLink[" + i + "]=" + prmIsDocumentLink[i]);
	}
	Object[] paramId = paramIdList.toArray();


	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		user = (EIMUser)sess.getAttribute("USER");

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();

		// 渡されてきたオブジェクト分ループ
		for( int ii = 0; ii < prmObjId.length; ii++ ) {

			boolean isFolder = prmIsFolder[ii].equals("true") ? true : false;	//フォルダ判定
			boolean isDocumentLink = prmIsDocumentLink[ii].equals("true") ? true : false;	//ドキュメントリンクか否か

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId[ii]));
			if (object == null) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			if (ii == 0) {
				// 対象オブジェクトに紐付くワークスペースの手動削除禁止フラグ
				if (AppObjectUtil.isObjectInWsRecycle(sess, object)) {
					// オブジェクトタイプがワークスペース
					EIMObjectType objTypeWS = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));

					// 削除対象オブジェクトからパスを取得
					String objPath = AppObjectUtil.getPath(object);
					String wsName = objPath.split("/")[1];

					EIMSearchSelectEIMObject selectTargetws = new EIMSearchSelectEIMObject();
					EIMSearchConditionGroup searchConditions = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

					// オブジェクトタイプを条件に設定
					searchConditions.addCondition(new EIMSearchConditionCompare(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, h.opEq(), objTypeWS.getId()));

					// ワークスぺース名称を条件に設定
					searchConditions.addCondition(new EIMSearchConditionCompare(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, h.opEq(), wsName));

					selectTargetws.setCondition(searchConditions);

					// 返却値設定 返却項目：手動削除禁止フラグ
					List<EIMAttributeType> resultAttrs = new ArrayList<EIMAttributeType>();

					EIMAttributeType manualDeleteFlagAttributeType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_WORKSPACE_MANUAL_DELETE_PROHIBITED_FLAG"));
					resultAttrs.add(manualDeleteFlagAttributeType);
					selectTargetws.setResultAttrs(resultAttrs);

					// ワークスペースオブジェクト
					List wsList = SearchUtils.searchObjects(sess, selectTargetws, new EIMSearchLimitCountCondition(-1, true));
					EIMObject wsObj  = (EIMObject)wsList.get(0);

					long isManualDeleteFlag = AppObjectUtil.getIntAttr(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_MANUAL_DELETE_PROHIBITED_FLAG"), Integer.MIN_VALUE);
					if (isManualDeleteFlag == 1) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.MANUAL.DELETE.PROHIBITED");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.MANUAL.DELETE.PROHIBITED", new Object[]{object.getName()});
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
						return;
					}
				}
			}
			
			// 性能向上のためフォルダ配下のオブジェクトを一括ロードする
			List<Integer> roleIdList = Arrays.asList(EIMAccessRole.READ, EIMAccessRole.UPDATE, AppConstant.ACCESS_ROLE_ALWAYS_READ);
			helper.loadChildObjectsRecursive(object, roleIdList, AppObjectConditionHelper.LoadingModeEnum.VERSION_AND_BRANCH);

			if(isDocumentLink != true) {
				//選択対象がタグが付与されたドキュメント・フォルダ・タグの場合は削除できない。
				//ドキュメントリンクは削除できる。
				if( AppLogicUtil.isTagAssignedObject(sess, object, helper, true) ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}

			// ドキュメント or フォルダ
			if( isDocumentLink != true &&
					(helper.isTypeOfDocument(object.getType()) || helper.isTypeOfFolder(object.getType())) ) {

				//Check Delete Role
				if (object.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.DELETE)) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODELETEROLE");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
				}

				//対象オブジェクトの親が画面で選択された親と一致するかチェック
				if (!AppObjectUtil.isObjectInParent(sess, Long.parseLong(prmParentObjId[ii]), object.getId())) {
					if (isFolder) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
					} else {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
					}
					out.println(AppMessageUtils.makeErrorTagByMessage(message));

					if (isFolder) {
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
					} else {
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
					}
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}

				//Relation Type
				EIMRelationType relType = helper.getRelationTypeOfDocument();

				// 親オブジェクトの取得
				EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId[ii]));

				// 対象オブジェクトのパス(履歴出力用)
				String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

				/* 物理削除 */
				if (object.getSecurity() != null
							&& object.getSecurity().getDefName().equals(EIMConfig.get("SECURITY_NAME_SYSTEM"))
							&& AppObjectUtil.isObjectInRecycle(sess, object)) {
					// 削除対象が「systemのセキュリティを保持、かつ、ごみ箱配下」の場合のみ物理削除
					// フォルダの場合は再帰的に全内容を物理削除する

					//まず、ごみ箱とのリレーションを切って、同時削除を排他もどきする(select for updateは出来ないので完璧ではない・・・)
					EIMRelation relRecycleToTarget = RelationUtils.getRelationByParentAndChild(sess, helper.getRelationTypeOfDocument(), parentObj, object);
					if (relRecycleToTarget == null) {
						String messageKey = isFolder?"EIM.ERROR.LOGIC.NOFOLDER":"EIM.ERROR.LOGIC.NODOCUMENT";
						out.println(AppMessageUtils.makeErrorTagByMessage(messageKey));
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), messageKey, paramId));
						return;
					}
					RelationUtils.deleteRelation(sess, relRecycleToTarget);//遅く来た方はここで止まる

					// SearchFramework 更新通知 対象：対象または配下ドキュメントのリンク先
					AppUpdateNoticeUtils.updateNoticePhysicalDelLinkParent(sess, object, "SEARCHFW_PHYSICALDEL_LINK_FOLDER", "SEARCHFW_PHYSICALDEL_LINK_WORKSPACE", helper);

					// 削除処理を行うツリーウォーカーを作成
					ProcessFolderTreeWalker treeWalker = new ProcessFolderTreeWalker() {
						public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
								AppObjectConditionHelper helper) throws Exception {
							//何もしない
						}

						/**
					 	 * 下位フォルダから上位フォルダに再帰復帰するタイミングでフォルダの物理削除を行います
					 	 */
						public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
								AppObjectConditionHelper helper) throws Exception {

							//フォルダに紐づくドキュメントリンクを削除します (削除または更新されている場合があるためDBから再取得)
							List childRelList = RelationUtils.getChildRelationListByRelType(helper.getSession(), lowerFolderObjFrom, helper.getRelationTypeOfDocLink(),EIMAccessRole.READ);
							for (int i = 0; i < childRelList.size(); i++) {
								EIMRelation rel = (EIMRelation) childRelList.get(i);
								AppLogicUtil.deleteDocLinkPath(helper.getSession(), rel.getChild(), lowerFolderObjFrom.getId() );
								RelationUtils.deleteRelation(helper.getSession(), rel);

								// SearchFramework 更新通知 対象：配下ドキュメントリンク
								AppUpdateNoticeUtils.updateNoticeInsert(rel.getChild().getId(), "SEARCHFW_PHYSICALDEL_DOCUMENTLINK");

								//操作履歴(ドキュメントリンク)
								OperationHistoryUtils.create(helper.getSession(), AppConstant.DOCUMENT, AppConstant.DOCLINK_DELETE,
											AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, rel.getChild(),
											EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, lowerFolderObjFrom, null);
							}

							deleteObject(lowerFolderObjFrom, helper);//フォルダ削除

							// SearchFramework 削除通知 対象：削除対象フォルダ・削除対象配下のフォルダ
							AppUpdateNoticeUtils.updateNoticeDelete(lowerFolderObjFrom.getId(), "SEARCHFW_PHYSICALDEL_FOLDER");

							//操作履歴(フォルダ)
							OperationHistoryUtils.create(helper.getSession(), AppConstant.DOCUMENT, EIMConstant.DELETE_FROM_RECYCLEBOX,
									EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, lowerFolderObjFrom,
									null, null, null, null);
						}

						/**
					 	 * 引数ドキュメントオブジェクトに対して物理削除処理を行います
					 	 */
						public void processDocument(EIMObject object, AppObjectConditionHelper helper)
								throws Exception {

							// SearchFramework 削除通知 対象：削除対象ドキュメント・削除対象配下のドキュメント
							AppUpdateNoticeUtils.updateNoticeDelete(object.getId(), "SEARCHFW_PHYSICALDEL_DOCUMENT");
							AppUpdateNoticeUtils.updateNoticeInsertBranch(helper.getSession(), object, "SEARCHFW_PHYSICALDEL_ORG_BRANCH_DOCUMENT", helper);

							deleteObject(object, helper);//ドキュメント削除

							//操作履歴(ドキュメント)
							OperationHistoryUtils.create(helper.getSession(), AppConstant.DOCUMENT, EIMConstant.DELETE_FROM_RECYCLEBOX,
									EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, object,
									null, null, null, null);
						}

						void deleteObject(EIMObject object, AppObjectConditionHelper helper) throws Exception {
							// 補足：上方・下方リレーションの削除はFW PL/SQLで行われるため不要
							// アクセスログを作成してもFW PL/SQLで全アクセスログを削除する為、無駄

							// ステータスを持つものは、ワークフロー関連のオブジェクトを削除
							// ただしワークフロー付きフォルダ下を除く
							if (object.getStatus() != null && !helper.isUnderFolderWithWorkflow(object)) {
								AppObjectUtil.deleteWFRelatedObject(helper.getSession(), object);
							}

							// OCR処理オブジェクトを削除
							AppOcrUtil.deleteOcrProcessingObject(helper.getSession(), object);

							// 物理削除（属性の添付ファイルオブジェクトもまとめて削除）
							FormDomain formDomain = new FormDomain();
							formDomain.setId(object.getId());
							DocumentFormService documentFormService =
									(DocumentFormService)ApplicationContextLoader.getApplicationContext().getBean("documentFormService");
							List<FormDomain> formDomainList = new ArrayList<FormDomain>();
							formDomainList.add(formDomain);
							documentFormService.deleteDocument(formDomainList);

							// 属性表示色オブジェクト削除
							DisplayColorUtil.deleteDisplayColorObject(helper.getSession(), object);

						}

					};

					if (helper.isTypeOfDocument(object.getType())) {
						//バージョンを取得
						EIMVersion version = VersionUtils.getVersion(sess, object);
						//バージョン分繰り返す
						for (Iterator j = version.getList().iterator(); j.hasNext();) {
							EIMObject delTargetObj = (EIMObject) j.next();
							//ドキュメント削除
							//操作履歴は内部で登録される
							treeWalker.processDocument(delTargetObj, helper);
						}

					} else {
						//フォルダ削除。フォルダツリーを再帰して削除実行
						//操作履歴は内部で登録される
						AppLogicUtil.processFolderTree(sess, object, true, false, treeWalker, helper);
					}

				} else {
					// ごみ箱オブジェクト
					EIMObject recycleObj = null;

					/* 論理削除 */
					// ワークスペース内からワークスペース固有ごみ箱への移動
					if (!parentObj.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))) {
						// オブジェクトタイプがワークスペース固有ごみ箱
						EIMObjectType objTypeWSRecycle = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

						// オブジェクトタイプがパス属性
						EIMAttributeType pathAttrType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));

						// ワークスペース固有ごみ箱自体のパス
						// 削除対象オブジェクトからパスを取得
						String objPath = AppObjectUtil.getPath(object);
						String [] strs = objPath.split("/");
						String wsRecyclePath = "/" + strs[1] + "/";

						EIMSearchSelectEIMObject selectTargetwsRecycle = new EIMSearchSelectEIMObject();
						EIMSearchConditionGroup searchConditions = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

						// オブジェクトタイプを条件に設定
						searchConditions.addCondition(new EIMSearchConditionCompare(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, h.opEq(), objTypeWSRecycle.getId()));

						// パスを条件に設定
						searchConditions.addCondition(new EIMSearchConditionCompare(h.opAnd(), pathAttrType, EIMSearchOperatorEnum.EQ, wsRecyclePath));

						selectTargetwsRecycle.setCondition(searchConditions);

						// ワークスペース固有ごみ箱オブジェクト
						List wsRecycleList = SearchUtils.searchObjects(sess, selectTargetwsRecycle, new EIMSearchLimitCountCondition(-1, true));
						recycleObj  = (EIMObject)wsRecycleList.get(0);

						//Access
						AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DELETETOWSRECBOX");

						//Create Operation History
						OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE_TO_WORKSPACERECYCLEBOX,
								EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, object,
								null, null, null, path);

					} else {
						// ワークスペース固有ごみ箱からシステムのごみ箱への移動
						// システムのごみ箱オブジェクト
						recycleObj = AppObjectUtil.getObject(sess,
								EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));

						//Access
						AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DELETETORECBOX");

						//Create Operation History
						OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE_TO_RECYCLEBOX,
								EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, object,
								null, null, null, path);
					}

					/* ブランチに関連する処理==> */
					EIMRelationType relTypeBranch = helper.getRelationTypeOfBranch();

					// フォルダの場合
					if(isFolder){
						// SearchFramework 更新通知 対象：フォルダ配下のドキュメントのブランチ元ドキュメント
						AppLogicUtil.logicalDeleteFolderBranch(sess, object, relTypeBranch, relType, helper);
					}
					// ドキュメントの場合
					else {
						// SearchFramework 更新通知 対象：対象ドキュメントのブランチ元ドキュメント
						AppLogicUtil.logicalDeleteDocumentBranch(sess, object, relTypeBranch, helper);
					}
					/* <==ブランチに関連する処理 */

					/* タグに関連する処理==> */
					// フォルダの場合
					if(isFolder){
						// フォルダ配下のタグを再帰的に取得して、タグの物理削除を行う
						// SearchFramework 削除通知 対象：フォルダ配下のタグ
						AppLogicUtil.physicalDeleteTagUnderFolder(sess, object, relType, helper);
					}
					/* <==タグに関連する処理 */

					if (recycleObj == null) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// SearchFramework 検索FW更新通知
					if(isFolder){
						// フォルダ 対象フォルダ + 配下のフォルダ・ドキュメント・タグ + 親フォルダ・親ワークスペース
						// 配下のタグを付与されたドキュメント・フォルダ・タグについてはTagUtilで通知
						AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_LOGICDEL_FOLDER");
						AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_LOGICDEL_CHILD_FOLDER", "SEARCHFW_LOGICDEL_CHILD_DOCUMENT", null, "SEARCHFW_LOGICDEL_CHILD_DOCUMENTLINK", helper);
						AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_LOGICDEL_PARENT_FOLDER", "SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null, helper);
					}
					else if(helper.isTypeOfDocument(object.getType())) {
						// ドキュメント 対象ドキュメント + 親フォルダ・親ワークスペース
						AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, "SEARCHFW_LOGICDEL_DOCUMENT", true, helper);
						AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_LOGICDEL_PARENT_FOLDER", "SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null, helper);
					}

					/* ごみ箱への関連付け */
					EIMRelation recycleboxRelation = RelationUtils.createRelation(sess, relType, recycleObj, object, EIMConstant.DEPU_CHECK_NONE);

					// 属性情報更新処理
					AttributeUtil.updateAttributeForDelete(sess, object, parentObj, recycleObj, helper);

					/* リレーションの削除 */
					List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType, EIMAccessRole.READ);
					for (int i = 0; i < parentRelList.size(); i++) {
						EIMRelation parentRelation = (EIMRelation)parentRelList.get(i);
						if (parentRelation.getId() != recycleboxRelation.getId())
						{
							RelationUtils.deleteRelation(sess, parentRelation);
						}
					}

				}
			}

			// タグ
			else if( isDocumentLink != true &&
					helper.isTypeOfTag(object.getType()) ) {

				//Check Delete Role
				if (object.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, object,sess.getUser(), EIMAccessRole.DELETE)) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODELETEROLE");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
				}

				//対象オブジェクトの親が画面で選択された親と一致するかチェック
				if (!AppObjectUtil.isObjectInParent(sess, Long.parseLong(prmParentObjId[ii]), object.getId())) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}

				//Relation Type
				EIMRelationType relType = helper.getRelationTypeOfDocument();

				// 親オブジェクトの取得
				EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId[ii]));

				// 対象オブジェクトのパス(履歴出力用)
				String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

				//Create Operation History
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.DELETE,
						EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, object,
						null, null, null, path);

				//タグオブジェクトの物理削除  ※ 属性表示色オブジェクトの削除も本関数で行う
//				AppLogicUtil.physicalDeleteTag(sess, object, parentObj, relType);

				// 有効期限のチェック
				Date effectDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));	// 有効期限
				if (effectDate != null) {
					// システム日付 ＜＝ 対象オブジェクトの属性値「有効期限」の場合はエラー
					if (!DateUtils.judgeExpirationDate(sess, effectDate)) {
						// 有効なドキュメント、またはフォルダは削除できません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTDEL.EFFECTIVE.DOCFOL");
					}
				}

				List objList = TagUtil.getTagGivenObj(sess, object);
				if (objList != null) {
					EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "actDeleteObject");
					for (Iterator i = objList.iterator(); i.hasNext();) {
						EIMObject obj = (EIMObject)i.next();
						// タグの解除
						TagUtil.removeTag(sess, obj, object, false);
					}
				}

				// 権限チェック
				if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.DELETE)){
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
				}
				if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.DELETE_RELATION)){
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETERELROLE");
				}

				// ドキュメントリレーションの削除
				EIMRelation rel = RelationUtils.getRelationByParentAndChild(sess, relType, parentObj, object);
				RelationUtils.deleteRelation(sess, rel);

				// 属性表示色オブジェクト削除
				DisplayColorUtil.deleteDisplayColorObject(sess, object);

				// タグオブジェクトの削除
				ObjectUtils.deleteObject(sess, object);

				// SearchFramework タグ 対象+対象の親フォルダ・親ワークスペース
				AppUpdateNoticeUtils.updateNoticeDelete(object.getId(), "SEARCHFW_PHYSICALDEL_TAG");
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PHYSICALDEL_PARENT_FOLDER", "SEARCHFW_PHYSICALDEL_PARENT_WORKSPACE", null, helper);
			}

			// ドキュメントリンク
			else
			{
				//Parent Object
				EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId[ii]));
				if(parentObj == null)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}

				//Parent Objectのセキュリティのチェック
				if (parentObj.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, parentObj,sess.getUser(), EIMAccessRole.UPDATE)) {
						// リンク貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
					}
				}

				//Parent Object Path
				String path = AppObjectUtil.getPath(parentObj);
				if(path == null){
					// ワークスペースの場合、パス属性の値を保持していない
					path = "/";
				}
				path += parentObj.getName() + "/";

				// Parent Objectのステータス種別ID
				long stsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

				// 上位WFフォルダのステータスのチェック
				if ((helper.isTypeOfFolderWithWorkflow(parentObj)
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))
							&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
					// 削除権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
				}


				// 「リンク先」「パス」属性の中から、Parent Objectに関するものを削除
				AppLogicUtil.deleteDocLinkPath(sess, object, Long.parseLong(prmParentObjId[ii]) );

				//Relation Type
				EIMRelationType relType = helper.getRelationTypeOfDocLink();

				// 「リンク」リレーションの削除
				List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType, EIMAccessRole.NONE);
				for (int i = 0; i < parentRelList.size(); i++) {
					EIMRelation rel = (EIMRelation) parentRelList.get(i);
					if( ((EIMObject) rel.getParent()).getId() == Long.parseLong(prmParentObjId[ii]) )
						RelationUtils.deleteRelation(sess, rel);
				}

				// SearchFramework ドキュメントリンク 対象＋対象の親フォルダ・親ワークスペース
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_PHYSICALDEL_DOCUMENTLINK");
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj,"SEARCHFW_PHYSICALDEL_PARENT_FOLDER", "SEARCHFW_PHYSICALDEL_PARENT_WORKSPACE", null, helper);

				//Access
				AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DOCLINK.DELETE");

				//Create Operation History
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.DOCLINK_DELETE,
							AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, object,
							EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, parentObj, path);

			}
		}

		//返却OIDは使用していないようなので<OK></OK>に変更
		//XML
//		out.println("<object");
//			out.println(" objId=\"" + object.getId() + "\"");
//		out.println(">");
//		out.println("</object>");
		out.println("<OK></OK>");

		//Commit
		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
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