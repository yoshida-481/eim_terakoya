<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "app.document.search.*"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.bo.EIMSearchSelectEIMObject.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.bo.*" %>
<%@ page import = "common.util.*" %>

<%
	/* 指定のフォルダ、またはドキュメントを元に、ルートまでのツリー情報を取得 */

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * フォルダツリー取得用クラス
	 */
	class RecurrentUtils
	{
		private EIMSession _sess;
		private EIMUser _user;
		private AppObjectConditionHelper _helper;

		// キー：親オブジェクトID、値：表示対象の子タグの順序付きリスト
		private Map<Long, List<EIMObject>> _tagMap = new HashMap();

		// キー：親オブジェクトID、値：表示対象の子フォルダの順序付きリスト
		private Map<Long, List<EIMObject>> _folderMap = new HashMap();

		// キー：親オブジェクトID、値：表示対象の子ワークスペース固有ごみ箱の順序付きリスト
		private Map<Long, List<EIMObject>> _wsRecycleMap = new HashMap();

		// 「常時読取」権限ありのオブジェクトのIDセット
		private Set<Long> _readAlwaysObjIdSet = new HashSet<Long>();

		// 子リレーションをもつオブジェクトのIDセット
		private Set<Long> _hasParentSet = new HashSet<>();

		/** コンストラクタ */
		public RecurrentUtils(EIMSession sess, EIMUser user)
		{
			_sess = sess;
			_user = user;
			_helper = new AppObjectConditionHelper(_sess);
		}

		/**
		 * フォルダツリーを出力します。
		 * @param object 親オブジェクト（ワークスペースオブジェクト）
		 * @param out 出力対象
		 * @param treeLevel 親オブジェクトの階層(ワークスペースの階層を1とする)
		 * @param objList ルートから指定オブジェクトまでを繋ぐオブジェクトのリスト (最下位階層が前に格納される)
		 * @param tagObjIds オブジェクトID配列。タグ配下を選択時のみ値がある。ルートタグから選択対象までの並び。
		 * @param garbageBoxName ごみ箱の多言語名称
		 */
		public void write(
				EIMObject		object,
				JspWriter		out,
				int 			treeLevel,
				List			objList,
				long[]			tagObjIds,
				String			garbageBoxName
		) throws Exception
		{
			// objList [0]ターゲットのフォルダorタグ、[1..n-1]フォルダ、[n]ワークスペース
			long parentObjIds[] = new long[objList.size()];
			for (int i = 0; i < objList.size(); i++)
			{
				parentObjIds[i] = ((EIMObject)objList.get(i)).getId();
			}

			// パス上の全ての子リレーション取得
			List<EIMRelation> childRelList =
				AppSearchUtils.searchRelationByConditionMaker(_sess, EIMDocSearchType.DISPLAY_ALL_CHILD_OBJECTS, EIMAccessRole.READ, null, parentObjIds);

			// 子リレーションをもつオブジェクトのIDセット
			for (EIMRelation rel : childRelList) {
				_hasParentSet.add((long)rel.getParent().getId());
			}

			// ドキュメントタイプはリストから削除する。
			for(int i = childRelList.size() -1 ; 0 <= i ; i--){
				if(_helper.isTypeOfDocument(childRelList.get(i).getChild().getType())){
					childRelList.remove(i);
				}
			}

			// ｢常時読取｣権限ありオブジェクトのIDセット
			// ワークフロー付きフォルダ用
			initReadAlwaysObjIdSet(childRelList);

			// タグマップ、フォルダマップ、ワークスペース固有ごみ箱マップ作成
			for (EIMRelation childRel : childRelList)
			{
				EIMObject childObj = childRel.getChild();

				if (_helper.isTypeOfFolder(childObj.getType()))
				{
					// フォルダの場合
					if (childObj.getStatus() != null)
					{
						// WF付きフォルダの場合
						if (!_readAlwaysObjIdSet.contains((long)childObj.getId())
								&& childObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
						{
							// 常時読取権限なし かつ 未公開フォルダ
							// 表示対象外
							continue;
						}
					}

					putObjectToMap(_folderMap, childRel.getParent(), childObj);
//					out.println("folder parent:"+childRel.getParent().getId()+" child:"+childObj.getId());
				}
				else if (_helper.isTypeOfTag(childObj.getType()))
				{
					// タグの場合
					putObjectToMap(_tagMap, childRel.getParent(), childObj);
//					out.println("folder parent:"+childRel.getParent().getId()+" child:"+childObj.getId());
				}
				else if (_helper.isTypeOfWorkspaceRecycle(childObj.getType()))
				{
					// ワークスペース固有ごみ箱の場合
					putObjectToMap(_wsRecycleMap, childRel.getParent(), childObj);
				}
			}

			writeTree(object, out, treeLevel, objList, tagObjIds, garbageBoxName);

		}

		/**
		 * 再帰的にフォルダツリーを出力します。
		 * @param object 親フォルダオブジェクト
		 * @param out
		 * @param treeLevel 親オブジェクトの階層(ワークスペースの階層を1とする)
		 * @param objList ルートから指定オブジェクトまでを繋ぐオブジェクトのリスト (最下位階層が前に格納される)
		 * @param tagObjIds オブジェクトID配列。タグ配下を選択時のみ値がある。ルートタグから選択対象までの並び。
		 * @param garbageBoxName ごみ箱の多言語名称
		 */
		public void writeTree(	EIMObject		object,
							JspWriter		out,
							int 			treeLevel,
							List			objList,
							long[]			tagObjIds,
							String			garbageBoxName
		) throws Exception {
			if(!_hasParentSet.contains((long)object.getId())) {
				return;
			}

			int objIndex = objList.size() - treeLevel - 1;

			// 指定オブジェクトのルート上にあるフォルダのID
			long rootChildObjId = 0;
			if (objIndex >= 0) {
				rootChildObjId = ((EIMObject)(objList.get(objIndex))).getId();
			}

			//Child Tags (タグはフォルダより上に表示する)
			List childTagList = AppSearchUtils.searchRelationByConditionMaker(_sess, EIMDocSearchType.DISPLAY_CHILDTAG, EIMAccessRole.READ, null, object);
			if (childTagList != null)
			{
				for(Iterator i = childTagList.iterator(); i.hasNext(); )
				{
					//Child Object
					EIMObject childObj = ((EIMRelation)i.next()).getChild();
					boolean isRootWork = (objIndex >= 0 && rootChildObjId == childObj.getId());

					TagTreeItem item = null;
					if (isRootWork) {

						// タグツリーの取得
						if (tagObjIds == null) {
							// タグ配下のみ一式取得
							item = TagUtil.getTagTree(_sess, childObj);
						} else {
							// ルートタグから選択対象を含むタグまでを取得
							item = TagUtil.getTagTreeForTarget(_sess, tagObjIds);
						}
						// タグツリーのソート
						TagUtil.sortTagTree(_sess, item, true, _helper);
					}

					out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
					out.println(" objId=\"" + childObj.getId() + "\"");
					out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + _helper.getObjTypeNameTagXmlEscaped() + "\"");
					out.println(" isWorkflowFolder=\"false\"");
					if (isRootWork) {
						out.println(" isBranch=\"" + (item.getTreeItemList().size() > 0 ? "true" : "false") + "\"");	// ブランチの△設定(子フォルダあり/なし)
						out.println(" isSearch=\"true\"");	// 探索済みフラグをtrueに設定 (タグ配下フォルダの更に配下も取得済みのため)
					} else {
						out.println(" isBranch=\"true\"");	// ブランチに△(子フォルダあり)を設定
						out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
					}
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_ROOT_TAG + "\"");		// タグ付与対象一覧種別：ルートタグ
					out.println(">");

					if (isRootWork) {
						// タグツリー描画の再起呼び出し
						writeTagTree(item, out);
					}
					out.println("</node>");
				}
			}

			//Child Folders
			List<EIMObject> childObjList= _folderMap.get((long)object.getId());		// フォルダのみ取得
			if (childObjList != null)
			{
				for(Iterator i = childObjList.iterator(); i.hasNext(); )
				{
					//Child Object
					EIMObject childObj = (EIMObject)i.next();
					boolean isRootWork = (objIndex >= 0 && rootChildObjId == childObj.getId());

					out.println("<node");
						out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
						out.println(" objId=\"" + childObj.getId() + "\"");
						out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
						out.println(" objTypeName=\"" + _helper.getObjTypeNameFolderXmlEscaped() + "\"");
						out.println(" isWorkflowFolder=\""+_helper.isTypeOfFolderWithWorkflow(childObj)+"\"");
						if (isRootWork) {
							// Child Relation
//							List grandChildRelList = _helper.getChildFolderTagsInAccessibleStatus(childObj, true, true, false);	// フォルダ、タグを取得
							boolean existsChildBranch =
								(_tagMap.get((long)childObj.getId()) != null
										|| 	_folderMap.get((long)childObj.getId()) != null);
							out.println(" isBranch=\""+ existsChildBranch +"\"");	// 子フォルダ、タグが1件以上あれば、ブランチに△(子フォルダあり)を、無ければ"子フォルダなし"を設定
							out.println(" isSearch=\"true\"");	// 指定オブジェクトのルート上にある場合のみ探索済みフラグをtrueに設定
						} else {
							out.println(" isBranch=\"true\"");	// ブランチに△(子フォルダあり)を設定
							out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
						}
						out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
					out.println(">");

					if (isRootWork) {
						// 指定オブジェクトのルート上にある場合、下階層に掘り下げていく。それ以外の場合は取得しない。
						writeTree(childObj, out, treeLevel + 1, objList, tagObjIds, garbageBoxName);
					}
					out.println("</node>");
				}
			}

			//Child wsRecycle
			List<EIMObject> childWSRecycleList= _wsRecycleMap.get((long)object.getId());		// ワークスペース固有ごみ箱のみ取得
			if (childWSRecycleList != null)
			{
				//Child Object
				EIMObject childObj = childWSRecycleList.get(0);
				out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(garbageBoxName) + "\"");
					out.println(" objId=\"" + childObj.getId() + "\"");
					out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + _helper.getObjTypeNameWorkspaceRecycleXmlEscaped() + "\"");
					out.println(" isWorkflowFolder=\"false\"");
					out.println(" isBranch=\"false\"");	// ごみ箱フォルダはツリー展開しない
					out.println(" isSearch=\"true\"");	// ごみ箱フォルダは探索しない
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				out.println(">");
				out.println("</node>");
			}
		}

		/**
		 * 常時読取権限のあるオブジェクトのIDセットを初期化します。
		 *
		 * @param childRelList チェックするリレーションのリスト（子オブジェクトをチェックします）
		 */
		private void initReadAlwaysObjIdSet(List<EIMRelation> childRelList) throws Exception
		{
			// チェック対象の子オブジェクトをリストに変換
			List<EIMObject> childObjectList = new ArrayList<EIMObject>();
			int i = 0;
			if (childRelList == null)
			{
				return;
			}
			for (EIMRelation childRel : childRelList)
			{
				childObjectList.add(childRel.getChild());
			}

			// 常時読み込み権限のあるオブジェクトをセットに変換
			EIMObject[] childObjects = (EIMObject[])childObjectList.toArray(new EIMObject[0]);
			if (childObjects.length == 0)
			{
				return;
			}
			List<EIMObject> readAlwaysEIMObjectList =
				_helper.getAuthorizedObjects(_sess, childObjects, _user, AppConstant.ACCESS_ROLE_ALWAYS_READ);
			_readAlwaysObjIdSet = new HashSet<Long>();
			if (readAlwaysEIMObjectList == null)
			{
				return;
			}
			for (EIMObject readAlwaysEIMObject : readAlwaysEIMObjectList)
			{
				_readAlwaysObjIdSet.add((long)readAlwaysEIMObject.getId());
			}

		}

		/**
		 * 親オブジェクトのIDとその子オブジェクトのマップを作成します。
		 *
		 * @param map 設定対象マップ
		 * @param parentObj 親オブジェクト
		 * @param childObj 子オブジェクト
		 */
		private void putObjectToMap(Map<Long, List<EIMObject>> map, EIMObject parentObj, EIMObject childObj)
			throws Exception
		{
			List<EIMObject> childObjList = map.get((long)parentObj.getId());
			if (childObjList == null)
			{
				childObjList = new ArrayList<EIMObject>();
			}
			childObjList.add(childObj);

			map.put((long)parentObj.getId(), childObjList);
		}

		/**
		 * 再帰的にタグツリーを描画します。
		 *
		 * @param item タグツリー要素
		 * @param out
		 */
		public void writeTagTree (TagTreeItem item, JspWriter out) throws Exception {

			for (Iterator iter = item.getTreeItemList().iterator(); iter.hasNext();) {
				TagTreeItem childItem = (TagTreeItem) iter.next();
				EIMObject childObj = childItem.getEimObject();

				// タグの場合
				if (_helper.isTypeOfTag(childObj.getType())) {

					out.println("<node");
						out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
						out.println(" objId=\"" + childObj.getId() + "\"");
						out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
						out.println(" objTypeName=\"" + _helper.getObjTypeNameTagXmlEscaped() + "\"");
						out.println(" isWorkflowFolder=\"false\"");
						out.println(" isBranch=\"true\"");	// ブランチに△(子フォルダあり)を設定
						out.println(" isSearch=\"" + (childItem.getTreeItemList().size() > 0 ? "true" : "false") + "\"");	// 探索済みフラグ (配下なしはfalseとみなす)
						out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_TAG_UNDER_TAG + "\"");	// タグ付与対象一覧種別：タグ配下のタグ
					out.println(">");

				// フォルダの場合
				} else {

					out.println("<node");
						out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
						out.println(" objId=\"" + childObj.getId() + "\"");
						out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
						out.println(" objTypeName=\"" + _helper.getObjTypeNameFolderXmlEscaped() + "\"");
						out.println(" isWorkflowFolder=\"" + _helper.isTypeOfFolderWithWorkflow(childObj) + "\"");
						out.println(" isBranch=\"" + (childItem.getTreeItemList().size() > 0 ? "true" : "false") + "\"");	// ブランチの△設定(子フォルダあり/なし)
						out.println(" isSearch=\"true\"");	// 探索済みフラグをtrueに設定 (タグ配下フォルダの更に配下も取得済みのため)
						out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_TAG_UNDER_FOLDER + "\"");		// タグ付与対象一覧種別：タグ配下のフォルダ
					out.println(">");
				}

				// 再起呼び出し
				writeTagTree(childItem, out);

				out.println("</node>");
			}
		}
	}

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	// Session
	EIMSession sess = null;
	EIMUser user = null;

	// Parameter
	String prmObjId =  EIMUtils.getParameter(request, "ObjectId");
	String prmTagObjectIds =  EIMUtils.getParameter(request, "tagObjectIds");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"tagObjectIds=" + prmTagObjectIds
			};

	try {
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// User
		user = (EIMUser)sess.getAttribute("USER");

		if (prmObjId == null) {
			out.println("<nodes></nodes>");
			return;
		}

		// オブジェクトID配列の作成 (ルートタグから選択対象まで)
		String[] strTagObjIds = null;
		long[] tagObjIds = null;
		if (!StringUtils.isBlank(prmTagObjectIds)) {
			strTagObjIds = prmTagObjectIds.split(",");
			tagObjIds = new long[strTagObjIds.length];
			for (int i = 0 ; i < strTagObjIds.length ; i++) {
				tagObjIds[i] = Long.parseLong(strTagObjIds[i]);
			}
			// 対象オブジェクトをルートタグに差し替える
			prmObjId = strTagObjIds[0];
		}

		// Object Type
		EIMObjectType objTypeWowkSpace = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
		EIMObjectType objTypeRecycle = ObjectUtils.getObjectTypeByName(sess, "ごみ箱");

		// 指定のオブジェクト
		EIMObject selectObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(selectObj == null || !SecurityUtils.authorized(sess, selectObj, sess.getUser(),EIMAccessRole.READ))
		{
			out.clear();
			// 指定のワークスペース、フォルダ、またはタグは取得できません。
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// ルートから指定オブジェクトの親までを繋ぐオブジェクトのリスト (最下位階層が前に格納される)
		List objList = helper.getUpperObjectsToToplevel(selectObj);

		// 指定オブジェクトを先頭に追加
		objList.add(0,selectObj);

		// 存在しないパスやごみ箱内のフォルダの中身を参照しようとしている場合はエラーとする
		if ((!helper.isTypeOfWorkspace(((EIMObject)objList.get(objList.size() - 1)).getType()) && 					// ①第1階層がワークスペースではない かつ
			!helper.isTypeOfRecycle(selectObj.getType())) 															// 　選択オブジェクトがごみ箱ではない
				|| (objList.size() > 2 && 																			// または　②第二階層が存在する かつ
					helper.isTypeOfWorkspaceRecycle(((EIMObject)objList.get(objList.size() - 2)).getType()) &&		// 			 第二階層がワークスペースごみ箱 かつ
					helper.isTypeOfFolder(((EIMObject)objList.get(objList.size() - 3)).getType())					// 			 第三階層がフォルダ
					)
			) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTOBJECT.NOGET");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 全ワークスペースの取得
		List<EIMObject> targetObjectList = null; // オブジェクトを絞り込む場合に指定する（ワークスペースにのみ有効）
		String targetObjectIdCSV = request.getParameter("targetRootObjectIds");
		if (targetObjectIdCSV != null && targetObjectIdCSV.length() > 0) {

			targetObjectList = new ArrayList<>();
			String[] idsStr = targetObjectIdCSV.split(",");
			for (String idStr : idsStr) {
				EIMObject targetObject = new EIMObject(Long.parseLong(idStr), null,
						null, -1,
						false, null, null, null, null,
						null, null, false, false, null);
				targetObjectList.add(targetObject);
			}
		}

		List<EIMObject> result = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_WORKSPACE,
		EIMAccessRole.READ, null, targetObjectList);

		// Root Node
		out.println("<nodes");

		// Root Nodeの続き。ルートからタグ配下の選択対象までのカンマ区切りオブジェクトIDを返す
		if (tagObjIds != null) {
			String rootObjToTargetObjIds = "";
			// ワークスペースからルートタグまで
			for (int i = objList.size() - 1 ; i >= 0 ; i--) {
				rootObjToTargetObjIds += ((EIMObject)objList.get(i)).getId() + ",";
			}
			// ルートタグ配下から選択対象まで
			for (int i = 1 ; i < tagObjIds.length ; i++) {
				rootObjToTargetObjIds += tagObjIds[i];
				if (i != tagObjIds.length - 1) {
					rootObjToTargetObjIds += ",";
				}
			}
			out.print(" rootToTargetIds=\"" + rootObjToTargetObjIds + "\"");
		}

		out.println(">");

		// 指定オブジェクトのルートにあるワークスペースのID
		long rootWorkObjId= ((EIMObject)(objList.get(objList.size() - 1))).getId();

		for (int i = 0; i < result.size(); i++) {
			// ワークスペースオブジェクトの取得
			EIMObject workObj = (EIMObject)result.get(i);
			boolean isRootWork = (rootWorkObjId == workObj.getId());

			out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(workObj.getName()) + "\"");
				out.println(" objId=\"" + workObj.getId() + "\"");
				out.println(" objTypeId=\"" + workObj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(workObj.getType().getDefName()) + "\"");
				out.println(" isWorkflowFolder=\"false\"");
				out.println(" isWorkSpace=\"true\"");
				out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
				out.println(" isSearch=\""+isRootWork+"\"");	// 指定オブジェクトのルートにあるワークスペースの場合は、探索済みフラグをtrueに設定
				out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				if (workObj.getSecurity() != null) {
					out.println(" secName=\"" + workObj.getSecurity().getDefName() + "\"");
				} else {
					out.println(" secName=\"\"");
				}
			out.println(">");

			// 指定オブジェクトのルートにあるワークスペースの場合のみ下階層に掘り下げていく、それ以外は取得しない
			if (isRootWork) {
				RecurrentUtils ru = new RecurrentUtils(sess, user);
				ru.write(workObj, out, 1, objList, tagObjIds, objTypeRecycle.getName());
			}
			out.println("</node>");
		}

		//ごみ箱（systemセキュリティに権限のあるユーザにのみ表示する）
		if (AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.READ))
		{
			EIMObject recycleObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeRecycle, "ごみ箱");
			if (recycleObj != null)
			{
				out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(objTypeRecycle.getName()) + "\"");
					out.println(" objId=\"" + recycleObj.getId() + "\"");
					out.println(" objTypeId=\"" + recycleObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(recycleObj.getType().getDefName()) + "\"");
					out.println(" isWorkflowFolder=\"false\"");
					out.println(" isBranch=\"false\"");	// ごみ箱フォルダはツリー展開しない
					out.println(" isSearch=\"true\"");	// ごみ箱フォルダは探索しない
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				out.println(">");
				out.println("</node>");
			}
		}

		// End Root Node
		out.println("</nodes>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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