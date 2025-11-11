<%@page import="app.document.object.UpdateObjectLink"%>
<%@page import="common.bo.VersionBranch"%>
<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>
<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.bo.VersionNode"%>
<%@ page import="common.util.*"%>
<%@page import="eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	/**
	 * ブランチツリー取得用クラス
	 */
	class RecurrentUtils
	{
		private EIMSession _sess;
		private AppObjectConditionHelper _helper;
		private EIMRelationType _relTypeBranch;
		private EIMFormat _formatPDF;
		private EIMAttributeType _attTypeDelFlag;
		private EIMRelationType _relTypeLink;
		private boolean _isDisplayLatestLinkConfig;
		
		/** 
		 * コンストラクタ
		 *
		 * @param sess セッション情報
		 * @param helper 条件判定ヘルパー作成
		 * @param formatPDF 公開フォーマット
		 * @param relTypeBranch ブランチリレーションタイプ
		 * @param attTypeDelFlag ブランチ先削除属性タイプ
		 * @param relTypeLink リンクリレーションタイプ
		 * @param isDisplayLatestLinkConfig 過去リビジョン通知フラグ
		 */
		public RecurrentUtils(
				EIMSession sess, 
				AppObjectConditionHelper helper,
				EIMFormat formatPDF,
				EIMRelationType relTypeBranch,
				EIMAttributeType attTypeDelFlag,
				EIMRelationType relTypeLink,
				boolean isDisplayLatestLinkConfig) throws Exception {
			_sess = sess;
			_helper = helper;
			_formatPDF = formatPDF;
			_relTypeBranch = relTypeBranch;
			_attTypeDelFlag = attTypeDelFlag;
			_relTypeLink = relTypeLink;
			_isDisplayLatestLinkConfig = isDisplayLatestLinkConfig;

		}
	
		/**
		 * 再帰的にブランチツリーを取得します。
		 *
		 * @param parentObject 親オブジェクト
		 * @param parentNode 親ノード情報
		 * @param childRelList 子リレーションリスト
		 */
		public void getBranchTree(	
			EIMObject		parentObject,
			VersionNode		parentNode,
			List			childRelList,
			List 			objIdList) throws Exception {

			// 子リレーションを取得
			if(childRelList.size() <= 0){
				// 対象のオブジェクトが最下位の場合終了
				return;
			}
			
			// ブランチリストの生成
			List branchList = new ArrayList();
			for(int i = 0 ; i < childRelList.size() ; i++) {
				// 削除チェック
				EIMRelation rel = (EIMRelation)childRelList.get(i);
				EIMAttribute delFlag = rel.getAttribute(_attTypeDelFlag.getDefName());
				if(	delFlag != null &&
					delFlag.getInts()[0] == AppConstant.FLAG_ON ){
					continue;
				}
				
				// ブランチ情報の生成
				VersionBranch branch = new VersionBranch();
				
				// オブジェクトのリストの取得
				EIMObject childObject = rel.getChild();
				// [09/03/10 modified by ik.]
				// 権限チェックを追加
				if (!_helper.checkAccessibleStatusSelf(childObject, false)) {
					continue;
				}
				EIMVersion version = VersionUtils.getVersion(_sess, childObject);
				List objList = version.getList();
				
				// 最新バージョンのパス情報の保持()
				String path = version.getLatest().getAttribute(_helper.getAttrNameOfPath()).getStrings()[0];
				
				// ノードリストの生成
				List nodeList = new ArrayList();
				for(int j = 0 ; j < objList.size(); j++)
				{
					// オブジェクト情報の取得
					EIMObject object = (EIMObject)objList.get(j);
					
					// [09/03/18 modified by ik.]
					// 権限チェックを追加
					if (!_helper.checkAccessibleStatusSelf(object, false)) {
						break;
					}
					
					// ブランチツリー情報の生成
					VersionNode versionNode = new VersionNode();
					
					// オブジェクト情報の設定
					List relList = 
						versionNode.setObjectInfo(_sess, object, _helper, _formatPDF, _relTypeBranch, _attTypeDelFlag);

					// パス情報の設置絵
					versionNode.setPath(path);
					
					// 探索済みフラグの設定
					versionNode.setSearch(true);
					
					// 子ブランチの情報を再帰的に取得
					getBranchTree(object, versionNode, relList, objIdList);
					
					// ノードリストに追加
					nodeList.add(versionNode);
					
					// リストに追加
					objIdList.add(object.getId());

				}
				// ブランチにノードリストを追加
				branch.setNodeList(nodeList);
				
				// ブランチリストにブランチを追加
				branchList.add(branch);
			}
			
			// 親ノードにブランチリストを設定
			parentNode.setChildBranchList(branchList);
		}
		
		/**
		 * デフォルトルート情報を判別、設定します
		 *
		 * @param versionBranch ブランチ情報
		 * @param initialSelectedObjId 初期選択されたオブジェクトID
		 */
		public boolean checkDefaultRoot(
			VersionBranch	versionBranch,
			long				initialSelectedObjId ) throws Exception {
			
			// ノードリストの取得
			List nodeList = versionBranch.getNodeList();
			
			for(int i = 0 ; i < nodeList.size() ; i++){
				// ノード情報を取得
				VersionNode node = (VersionNode)nodeList.get(i);
				
				// 初期選択ノードの場合
				if(node.getObjId() == initialSelectedObjId){
					
					// 初期選択フラグの設定
					node.setInitialSelect(true);
					
					// デフォルトルートフラグを設定
					node.setDefaultFlag(true);
					
					// 親のブランチもデフォルトルートフラグを設定
					versionBranch.setDefaultFlag(true);
					
					// これ以前のバージョンのノードのデフォルトルートフラグを設定
					for(int j = i - 1 ; j >= 0 ; j--){
						
						// ノード情報を取得
						node = (VersionNode)nodeList.get(j);
						
						node.setDefaultFlag(true);
					}
					// ここで上位にtrueを返す
					return true;
				}
				
				// 子ブランチ情報の取得
				List childBranchList = node.getChildBranchList();
				for(int k = 0; k < childBranchList.size() ; k++){
					
					// 子ブランチにデフォルトルートはないか
					VersionBranch childBranch = (VersionBranch)childBranchList.get(k);
					if(checkDefaultRoot(childBranch, initialSelectedObjId)){
						
						// デフォルトルートフラグを設定
						node.setDefaultFlag(true);
						
						// 親のブランチもデフォルトルートフラグを設定
						versionBranch.setDefaultFlag(true);
						
						// これ以前のバージョンのノードのデフォルトルートフラグを設定
						for(int j = i - 1 ; j >= 0 ; j--){
							
							// ノード情報を取得
							node = (VersionNode)nodeList.get(j);
							
							node.setDefaultFlag(true);
						}
						// ここで上位にtrueを返す
						return true;
						
					}
				}
			}
			
			// ブランチにはデフォルトルートはなかった
			return false;
		}
		
		/**
		 * ブランチ情報出力処理
		 *
		 * @param versionBranch 出力するブランチ情報
		 * @param out 出力先JspWriter
		 */
		public void write(
			VersionBranch	versionBranch,
			Map<Long, String>	noSTPublicObjMap,
			JspWriter		out) throws Exception {

			// ブランチ情報の出力
			out.println("<branch");
				// デフォルトルートフラグ
				out.println(" default=\"" + versionBranch.isDefaultFlag() + "\"");
			out.println(">");
			
			// ノードリストの取得
			List nodeList = versionBranch.getNodeList();
	
			for(int i = 0 ; i < nodeList.size() ; i ++ ){
				// ノード情報の出力
				VersionNode versionNode = (VersionNode)nodeList.get(i);
				EIMObject obj = versionNode.getObj();
				
				// リンク元が格納されているオブジェクトのIDを取得
				List relList = RelationUtils.getParentRelationListByRelType(this._sess, obj, this._relTypeLink ,EIMAccessRole.READ);
				
				// リンク一覧を取得
				String paths[] = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStrings();
				
				for (int j = 0; j < relList.size() + 1; j++)
				{
					EIMRelation parentRel;
					String parentObjId;
					String path;
					boolean isDocumentLink;
					String linkDspLatest = "false";
					String linkUpdateTiming = "0";	//0:手動 1:自動
					
					// 1つめはドキュメント本体、2つ目以降がドキュメントリンク
					if (j == 0) {
						parentObjId = "";
						path = paths[0];
						isDocumentLink = false;
					} else {
						parentRel = (EIMRelation)relList.get(j-1);
						EIMObject parent = parentRel.getParent();
						
						// ワークスペースは名称がそのままパスになる
						if (parent.getType().getDefName().compareTo(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")) == 0)
						{
							path = "/" + parent.getName() + "/";
						}
						else
						{
							path = AppObjectUtil.getPath(parent) + parent.getName() + "/";
						}
						parentObjId = Long.toString(parent.getId());
						isDocumentLink = true;

						// リンクの過去リビジョンを通知するかどうか
						if(this._isDisplayLatestLinkConfig){
							if( !obj.getLatest() ) {
								linkDspLatest = "true";
							}
						}
						
						// リンクのリンク更新タイミングをアイコンで表示する
						linkUpdateTiming = Long.toString(parentRel.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING")).getInt());
					}
					
					out.println("<node");
					// オブジェクトID
					out.println(" objId=\"" + versionNode.getObjId() + "\"");
					// オブジェクト名
					out.println(" objName=\"" + StringUtils.xmlEncode(versionNode.getObjName()) + "\"");
					// オブジェクトタイプID
					out.println(" objTypeId=\"" + versionNode.getObjTypeId() + "\"");
					// オブジェクトタイプ名
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(versionNode.getObjTypeName()) + "\"");
					// バージョン番号
					out.println(" rev=\"" + versionNode.getRev() + "\"");
					// 更新ユーザ名
					out.println(" modifyUserName=\"" + StringUtils.xmlEncode(versionNode.getModifyUserName()) + "\"");
					// 更新日
					String modifyDate = DateUtils.getDBTzToCLTzDate(_sess, versionNode.getModifyTime());
					out.println(" modifyDate=\"" + modifyDate + "\"");
					// ソート用更新日時
					String modifyTime = String.valueOf(versionNode.getModifyTime().getTime() / 1000);
					out.println(" modifyTime=\"" + modifyTime + "\"");
					// 改訂内容
					out.println(" updateComment=\"" + StringUtils.xmlEncode(StringUtils.nullToBlank(versionNode.getUpdateComment())) + "\"");
					// 場所
					out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
					// ステータスタイプ種別
					out.println(" statusTypeKind=\"" + versionNode.getStatusTypeKind() + "\"");
					// 格納先フォルダオブジェクトID
					out.println(" parentObjId=\"" + parentObjId + "\"");
					
					// PDF結合失敗の場合、ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」に非該当の場合
					// 公開アイコンを表示しない
					if( versionNode.isDspPubIconForNoWF() == true ){
						if(versionNode.isPDFJoinFailed() || !noSTPublicObjMap.containsKey((long)versionNode.getObjId())){
							versionNode.setDspPubIconForNoWF(false);
						}
					}

					// WFなしドキュメントの公開アイコン表示フラグ
					out.println(" isDspPubIconForNoWF=\"" + versionNode.isDspPubIconForNoWF() + "\"");
					// PDF結合処理失敗フラグ
					out.println(" isPDFJoinFailed=\"" + versionNode.isPDFJoinFailed() + "\"");										
					// PDF公開フラグ
					out.println(" isDspPdfIcon=\"" + versionNode.isDspPdfIcon() + "\"");
					// PDF変換ステータス出力 ★PDF変換中、PDF変換失敗はステータスが表示対象でないため判定しない
					int pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_NONE;
					if (versionNode.isDspPdfIcon()) {
						// PDF変換完了
						Date pdfConvExecDate = AppObjectUtil.getDateAttr(_sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
						Date pdfPreRegistDate = AppObjectUtil.getDateAttr(_sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
						if (pdfConvExecDate != null && pdfConvExecDate.getTime() <= obj.getModifyDate().getTime()) {
							// PDF変換処理実行日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
							pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
						} else {
							pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
						}
						// 公開PDF事前登録日時が存在する場合はその判定を優先する
						if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() <= obj.getModifyDate().getTime()) {
							// 公開PDF事前登録日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
							pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
						} else if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() > obj.getModifyDate().getTime()) {
							pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
						}
						
						// nullでなければ公開PDF事前登録済み
						boolean isPdfPreRegistered = pdfPreRegistDate != null ? true : false;
						out.println(" isPdfPreRegistered=\"" + isPdfPreRegistered + "\"");
					}
					out.println(" pdfConversionStatus=\"" + pdfConversionStatus + "\"");
					// 有効期限切れフラグ
					out.println(" expiration=\"" + versionNode.isExpiration() + "\"");
					// 読み取り権限のみフラグ
					out.println(" readOnly=\"" + versionNode.isReadOnly() + "\"");
					// 署名・暗号化状態
					out.println(" signencr=\"" + versionNode.getSignencr() + "\"");
					// OCR結果ステータス
					if(versionNode.getOcrResultStatus() != AppConstant.OCR_RESULT_STATUS_NONE){
						out.println(" ocrResultStatus=\"" + versionNode.getOcrResultStatus() + "\"");
					}else{
						out.println(" ocrResultStatus=\"\"");
					}
					// デフォルトルートフラグ
					out.println(" default=\"" + versionNode.isDefaultFlag() + "\"");
					// 初期選択フラグ
					out.println(" initialSelect=\"" + versionNode.isInitialSelect() + "\"");
					// 子ブランチ有無フラグ
					out.println(" hasChild=\"" + versionNode.isHasChild() + "\"");
					// 子ブランチ探索済みフラグ
					out.println(" isSearch=\"" + versionNode.isSearch() + "\"");
					// ドキュメントリンクフラグ
					out.println(" isDocumentLink=\"" + Boolean.toString(isDocumentLink) + "\"");
					//過去リビジョン通知フラグ
					out.println(" isDspLatestLink=\"" + linkDspLatest + "\"");
					// リンク更新タイミング
					out.println(" documentLinkUpdateTiming=\"" + linkUpdateTiming + "\"");
					
					// ドキュメント本体は子ノードにブランチ情報が着く
					if (j == 0) {
						out.println(">");
						// 子ブランチの情報を再帰的に出力
						List childBranchList = versionNode.getChildBranchList();
						for(int k = 0; k < childBranchList.size() ; k++){
							VersionBranch childBranch = (VersionBranch)childBranchList.get(k);
							write(childBranch, noSTPublicObjMap, out);
						}
						out.println("</node>");
					} else {
						out.println("/>");
					}
				}
			}
			out.println("</branch>");
		}
		
	}

	// Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//改訂履歴ドキュメントIDリスト
	List<Long> objIdList = new ArrayList<>();

	// Parameter
	String prmObjId = request.getParameter("objId");
	String prmInitialSelectedObjId = request.getParameter("initialSelectedObjId");

	// Message
	String message = null;
	Object[] paramId = {
		"objId=" + prmObjId,
		"initialSelectedObjId=" + prmInitialSelectedObjId
	};
	try {
		// ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

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
		user = (EIMUser)sess.getAttribute("USER");
		
		// 設定確認
		boolean isDisplayLatestLinkConfig  = AppLogicUtil.isDisplayLatestLink(sess);
		
		// パラメータ情報を保持
		long targetObjId = Long.parseLong(prmObjId);
		long initialSelectedObjId = Long.parseLong(prmInitialSelectedObjId);
		
		// 対象オブジェクト情報の取得
		EIMObject target = ObjectUtils.getObjectById(sess, targetObjId);
		if(target == null || !SecurityUtils.authorized(sess, target, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// オブジェクトタイプの取得
		if(target.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// オブジェクトのリストの取得
		EIMVersion version = VersionUtils.getVersion(sess, target);
		List objList = version.getList();
		
		// ブランチ情報を生成
		VersionBranch versionBranch = new VersionBranch();
		List nodeList = new ArrayList();
		
		// 公開ドキュメントのフォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// リンクリレーションタイプの取得
		EIMRelationType relTypeLink = helper.getRelationTypeOfDocLink();
		
		// ブランチリレーションタイプの取得
		EIMRelationType relTypeBranch = 
			RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_BRANCH"));
		
		// 削除フラグ属性タイプの取得
		EIMAttributeType attTypeDelFlag =
			AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_BRANCH_TARGET_DELETE"));
		
		RecurrentUtils ru = new RecurrentUtils(sess,helper,formatPDF,relTypeBranch, attTypeDelFlag, relTypeLink, isDisplayLatestLinkConfig);

		// 最新バージョンのパス情報の保持()
		String path = version.getLatest().getAttribute(helper.getAttrNameOfPath()).getStrings()[0];

		// 前後バージョンの情報を取得する
		for(int i = 0 ; i < objList.size(); i++)
		{
			// オブジェクト情報の取得
			EIMObject object = (EIMObject)objList.get(i);

			// [09/03/18 modified by ik.]
			// 権限チェックを追加
			if (!helper.checkAccessibleStatusSelf(object, false)) {
				break;
			}
			
			// ノード情報の生成
			VersionNode versionNode = new VersionNode();
			
			// オブジェクト情報の設定
			List childRelList = 
				versionNode.setObjectInfo(sess, object, helper, formatPDF, relTypeBranch, attTypeDelFlag);

			// パス情報の設定
			versionNode.setPath(path);
			
			// 探索済みフラグの設定
			versionNode.setSearch(true);
			
			// 子ブランチの情報を再帰的に取得
			ru.getBranchTree(object, versionNode, childRelList, objIdList);
			
			// リストに追加
			nodeList.add(versionNode);
			
			// リストに追加
			objIdList.add(new Long(object.getId()));
		}

		// ブランチにノードのリストを設定
		versionBranch.setNodeList(nodeList);

		// デフォルトルート情報を設定
		ru.checkDefaultRoot(versionBranch, initialSelectedObjId);
		
		// 公開アイコン表示判定用Map
		Map<Long, String> noSTPublicObjMap = new HashMap();
		
		//ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
		if(objIdList.size() > 0){
			EIMSearchSelectEIMObject noSTPublicObjSelect = new EIMSearchSelectEIMObject();
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(),objType.getId(), sess);

			String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objIdList);

			noSTPublicObjSelect.setCondition(
					h.group(h.opOr()).addCondition(
										new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
										.addCondition(objTypeCond));
			noSTPublicObjSelect.setResultAttrs(new ArrayList());
			List noSTPublicObjList = SearchUtils.searchObjects(sess, noSTPublicObjSelect, new EIMSearchLimitCountCondition(-1, false));
			for(int i = 0; i< noSTPublicObjList.size() ;i ++){
				noSTPublicObjMap.put((long)((EIMObject)noSTPublicObjList.get(i)).getId(),"");
			}
		}
		
		// XML出力
		ru.write(versionBranch, noSTPublicObjMap, out);
				
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
		try {
			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
