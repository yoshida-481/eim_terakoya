<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@page import="java.util.Arrays"%>
<%@page import="java.util.HashMap"%>
<%@page import="java.util.Iterator"%>
<%@page import="java.util.Stack"%>
<%@page import="java.util.Map"%>
<%@page import="java.util.List"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	// Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	// Parameter
	String prmFolderId = request.getParameter("folderId");

	/* --------------------------------------------*/
	/* [09/01/27 modified by ik.]
	 * 「ドキュメントリンク含む」フォルダツリー複製に対応
	 */
	boolean prmWithDocLink = Boolean.valueOf(request.getParameter("withDocLink")).booleanValue();
	/* --------------------------------------------*/
	String prmLinkUpdateTiming = request.getParameter("linkUpdateTiming");

	// Message
	String message = null;
	Object[] paramId = {
			"folderId=" + prmFolderId,
			"withDocLink=" + request.getParameter("withDocLink"),
			"linkUpdateTiming=" + prmLinkUpdateTiming
			};

	try {
		// Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
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
		
		user = (EIMUser) sess.getAttribute("USER");

		// 条件判定ヘルパー
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// コピー元フォルダオブジェクト
		EIMObject sourceFolder = ObjectUtils.getObjectById(sess, Long.parseLong(prmFolderId));
		if (sourceFolder == null || !SecurityUtils.authorized(sess, sourceFolder, sess.getUser(),EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// Parent Object
		EIMObject parentObj = helper.getUpperObject(sourceFolder);
		if (parentObj == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.PARENT.FOLDAER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.PARENT.FOLDAER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 選択フォルダがワークスペース固有ごみ箱配下である場合
		if (AppObjectUtil.isObjectInWsRecycle(sess,sourceFolder)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANT.COPYFOLDERTREE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.COPYFOLDERTREE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 選択フォルダの親ワークスペースが存在しない場合
		EIMObject rootObject = ObjectUtils.getRootObject(sess, sourceFolder.getId(),
			helper.getRelationTypeOfDocument());
		if (rootObject == null || !helper.isTypeOfWorkspace(rootObject.getType())) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.PARENT.WORKSPACE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.PARENT.WORKSPACE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// アクセス権限チェック
		// フォルダ構成管理権限
		if (!helper.checkUpdatableToFolder(parentObj, EIMAccessRole.CREATE)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 上位フォルダステータス。無ステータスか編集中で無ければNG
		if (parentObj.getStatus() != null
				&& parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// Create Object
		String copyPrefix = EIMConfig.get("COPY_FILENAME_PREFIX_" + sess.getLangId());
		String objName = copyPrefix + " - " + sourceFolder.getName();
		EIMObject newFolder = ObjectUtils.createObject(sess, sourceFolder.getType(), objName);

		// Create Relation
		// 複製フォルダは「コピー - コピー - ･･･ 名前」で重複の無い名前を探す
		boolean isRenamed = false;
		while (true) {
			try {
				RelationUtils.createRelation(sess, helper.getRelationTypeOfDocument(),
					parentObj, newFolder, EIMConstant.DEPU_CHECK_AVAILABLE);
				break;
			} catch (EIMException e) {
				if (!e.getMessageKey().equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE"))
					throw e;
			}
			objName = copyPrefix + " - " + objName;
			ObjectUtils.rename(sess, newFolder, objName);
			isRenamed = true;
			// retry loop
		}

		if (isRenamed) // objectのnameが異なるので再取得
			newFolder = ObjectUtils.getObjectById(sess, newFolder.getId());

		// 再帰して複製実行
		DuplicateFolderTreeWalker treeWalker = null;

		/* --------------------------------------------*/
		/* [09/01/27 modified by ik.]
		 * ・「ドキュメントリンク含む」フォルダツリー複製に対応。
		 * ・操作履歴も切り替える。
		 */
		String opeHistType = null;
		if (prmWithDocLink) {
			// ドキュメントリンク含む
			treeWalker = new DuplicateFolderWithDocLinkTreeWalker();
			
			treeWalker.linkUpdateTiming = Integer.parseInt(prmLinkUpdateTiming);
			if( treeWalker.linkUpdateTiming == AppConstant.LINK_UPDATE_TIMING_AUTO ) {
				// 公開時
				opeHistType = AppConstant.FOLDER_TREE_COPY_WITH_DOC_LINK_AUTO;
			}
			else {
				opeHistType = AppConstant.FOLDER_TREE_COPY_WITH_DOC_LINK;
			}
			
			// ### SEARCH FRAMEWORK ADD 検索FW更新通知 コピーしたフォルダ＋配下フォルダブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(newFolder.getId(), "SEARCHFW_FOLDERTREECOPYDOC_FOLDER");
			
		} else {
			// フォルダのみ
			treeWalker = new DuplicateFolderTreeWalker();
			opeHistType = EIMConstant.FOLDER_TREE_COPY;
			
			// ### SEARCH FRAMEWORK ADD 検索FW更新通知 コピーしたフォルダ＋配下フォルダブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(newFolder.getId(), "SEARCHFW_FOLDERTREECOPY_FOLDER");
		}
		/* --------------------------------------------*/

		// 性能向上のためフォルダ配下のオブジェクトを一括ロードする
		List<Integer> roleIdList = Arrays.asList(EIMAccessRole.READ);
		helper.loadChildObjectsRecursive(sourceFolder, roleIdList, AppObjectConditionHelper.LoadingModeEnum.VERSION);

		treeWalker.setTopObj(newFolder, parentObj, helper);
		AppLogicUtil.processFolderTree(sess, sourceFolder, false, true, treeWalker, helper);

		// Create Operation History
		// Path
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, opeHistType,
			AppConstant.TARGET_COPY, EIMConstant.OBJECT, newFolder, null, null, null,
			AppObjectUtil.getPath(sourceFolder));

		// Commit
		sess.commit();

		// [09/02/03 modified by ik.]リンク作成できなかったドキュメントの個数を返す
		out.println("<OK outOfTargetCount=\"" + treeWalker.outOfTargetCount + "\" objId=\"" + newFolder.getId() + "\"></OK>");

	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
		try {
			if (sess != null) {
				sess.rollback();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try {
			if (sess != null) {
				sess.rollback();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	} finally {
		try {
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}

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
<%!


/* [09/01/27 added by ik.]
 * フォルダツリー複製（ドキュメントをリンクで複製）を実行するFolderTreeWalkerクラス
 * ※ ドキュメントに対する処理のみをオーバーライド
 */
static class DuplicateFolderWithDocLinkTreeWalker extends DuplicateFolderTreeWalker {

	// ドキュメントに対する処理（ドキュメントリンクを作成する）
	public void processDocument(EIMObject object, AppObjectConditionHelper helper)
		throws Exception {

		// タグに対しては何もしない
		if (helper.isTypeOfTag(object.getType())) {
			return;
		}

		// リンク作成先のフォルダ
		EIMObject parentObj = (EIMObject)_treeStack.peek().getObj();

		// [09/03/18 modified by ik.]
		// ドキュメントリンク作成先フォルダパスを _treeStack の属性マップから取得し、createDocLink() 時に
		// ダイレクト指定するように修正。
		// --> 作成したドキュメントリンクのパスが相対パスになってしまう現象の改修。
		
		// リンク作成先のフォルダのパスを取得
		EIMAttribute attrPath = (EIMAttribute)_treeStack.peek().getAttrMap().get(helper.getAttrNameOfPath());
		String parentPath = ((attrPath != null) ? attrPath.getStrings()[0] : "/") + _treeStack.peek().getObj().getName() + "/";

		// [09/02/03 modified by ik.]
		// リンク作成処理を AppLogicUtil に移動
		//  - 操作ログは作成しない（フォルダツリー複製としてまとめて作成するため）
		try {
			// 元がドキュメントリンクの場合は、リンク更新タイミングを引き継ぐ
			AppLogicUtil.createDocLink(object, parentObj, parentPath, true, false, helper, 
					this.linkUpdateTiming, this.childObjVersionMap, this.parentObjVersionListMap);
			
			//### SEARCH FRAMEWORK ADD 検索FW更新通知 作成したドキュメントリンク元・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_FOLDERTREECOPYDOC_CHILD_DOCUMENTLINK");
			
		} catch (Exception e) {
			// リンクを作成できなかったドキュメントが存在する場合、個数をカウント
			this.outOfTargetCount++;
		}
		
	}
	
	// ### SEARCH FRAMEWORK ADD
	public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
			AppObjectConditionHelper helper) throws Exception {
		
		EIMObject obj = this.walkOutInternal(lowerFolderObjFrom, upperFolderObjTo, helper);
		
		// 検索FW更新通知 配下オブジェクトID・処理種別キーを指定
		AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), "SEARCHFW_FOLDERTREECOPYDOC_CHILD_FOLDER");
	}
}

/**
 * フォルダツリー複製（フォルダのみ）を実行するFolderTreeWalkerクラス
 */
static class DuplicateFolderTreeWalker implements AppLogicUtil.ProcessFolderTreeWalker {
	/** 最上位の複製新フォルダ */
	EIMObject _topTargetFolderObj;

	/** 現在処理中の新規複製フォルダツリー階層を表現するスタック */
	TreeStack _treeStack = new TreeStack();

	/** 名称割り当てする属性タイプ名 */
	EIMAttributeType attrTypeOfSyncName = null;

	/** 名称割り当てする属性値 */
	String attrValueOfSyncName = null;

	/** [09/02/03 added by ik.] 権限がなくリンク作成できなかったドキュメントの数 */
	/**   --> DuplicateFolderWithDocLinkTreeWalker 用に追加                      */
	public int outOfTargetCount = 0;
	
	/**  リンク更新タイミング --> DuplicateFolderWithDocLinkTreeWalker 用に追加 */       
	public int linkUpdateTiming = AppConstant.LINK_UPDATE_TIMING_MANUAL;
	
	/** リンク更新タイミングが公開時更新の場合に使用 */
	public HashMap<Long, Long> childObjVersionMap = new HashMap<Long, Long>();
	public HashMap<Long, List> parentObjVersionListMap = new HashMap<Long, List>();
	
	/** フォルダツリー複製を行わない属性 */
	String[] noCopyAttribute = {
		EIMConfig.get("ATTR_NAME_FOLDER_TAG"),
		EIMConfig.get("ATTR_NAME_FOLDER_TAG_GIVER"),
		EIMConfig.get("ATTR_NAME_FOLDER_TAG_GIVENDATE")
	};

	/**
	 * 最上位の複製基点となるフォルダーをセットします
	 *
	 * @param sourceFolderObj ソースフォルダ
	 * @param topTargetFolderObj 新規複製フォルダのトップフォルダ
	 * @param parentObj トップフォルダの親フォルダ
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	void setTopObj(EIMObject topTargetFolderObj, EIMObject parentObj, AppObjectConditionHelper helper)
			throws Exception {
		_topTargetFolderObj = topTargetFolderObj;
		_treeStack.push(new ObjectInfo(parentObj, getAttrMap(parentObj), parentObj.getStatus(), false));
	}

	public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
			AppObjectConditionHelper helper) throws Exception {
		EIMObject newFolderObj = null;
		if (upperFolderObjFrom == null) {
			// トップレベル
			newFolderObj = _topTargetFolderObj;
			// 名称割り当て情報を取得
			EIMAttribute attrFolderName = lowerFolderObjTo.getAttribute(helper.getAttrNameOfNameAttr());
			if (attrFolderName != null) {
				attrTypeOfSyncName = AttributeUtils.getAttributeTypeById(helper.getSession(),
					attrFolderName.getInt());
				attrValueOfSyncName = _topTargetFolderObj.getName();
			}
		} else {
			// 下位フォルダ
			// 同名フォルダを作成
			newFolderObj = ObjectUtils.createObject(helper.getSession(),
				lowerFolderObjTo.getType(), lowerFolderObjTo.getName());
			// 親フォルダとリレーションを作成
			RelationUtils.createRelation(helper.getSession(),
				helper.getRelationTypeOfDocument(), (EIMObject) _treeStack.peek().getObj(),
				newFolderObj);
		}
		_treeStack.push(inheritInfomation(lowerFolderObjTo, newFolderObj, helper));

		// ※ここでは、オブジェクトへのセキュリティは設定しない。オブジェクト操作が終わってからwalkOutでセットする。
	}

	// ### SEARCH FRAMEWORK MOD
	public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
			AppObjectConditionHelper helper) throws Exception {
		
		EIMObject obj = this.walkOutInternal(lowerFolderObjFrom, upperFolderObjTo, helper);
		
		// 検索FW更新通知 配下オブジェクトID・処理種別キーを指定
		AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), "SEARCHFW_FOLDERTREECOPY_CHILD_FOLDER");
	}

	public void processDocument(EIMObject object, AppObjectConditionHelper helper)
			throws Exception {
		// ドキュメントには何もしない
	}

	/**
	 * ソースオブジェクトからターゲットオブジェクトに属性値をコピーします。
	 *
	 * @param sourceObj ソースオブジェクト
	 * @param targetObj ターゲットオブジェクト
	 * @param helper 条件判定ヘルパー
	 * @return オブジェクト情報
	 * @throws Exception
	 */
	private ObjectInfo inheritInfomation(EIMObject sourceObj, EIMObject targetObj,
			AppObjectConditionHelper helper) throws Exception {
		Map newAttrMap = getAttrMap(sourceObj);
		boolean doBreakTopObjNameInherit = _treeStack.isTopLevel() ? false
				: _treeStack.peek().doBreakTopObjNameInherit();

		// 下位フォルダの場合は、パスは複写せず新パスを求めて差し替える
		if (!_treeStack.isTopLevel()) {
			EIMAttribute attrPath = (EIMAttribute) _treeStack.peek().getAttrMap().get(
				helper.getAttrNameOfPath());
			String path = ((attrPath != null) ? attrPath.getStrings()[0] : "/")
					+ _treeStack.peek().getObj().getName() + "/";

			newAttrMap.put(helper.getAttrNameOfPath(), new EIMAttribute(attrPath.getType(), -1,
					path, null));
		}

		// 名称割り当てされている場合、下位引き継ぎを処理
		if (attrTypeOfSyncName != null) {
			if (!newAttrMap.containsKey(attrTypeOfSyncName.getDefName())) {
				// このフォルダで引継ぎが断絶している
				doBreakTopObjNameInherit = true;
			} else if (_treeStack.isTopLevel() || !_treeStack.peek().doBreakTopObjNameInherit()) {
				// 名称割り当て属性の差し替え
				// ただし、上位で名称割り当て属性の下位引継ぎが中断されていたら差し替えない
				newAttrMap.put(attrTypeOfSyncName.getDefName(), new EIMAttribute(
						attrTypeOfSyncName, -1, attrValueOfSyncName, null));
			}
		}

		// 有効期限/公開処理失敗/上位WFフォルダは除外
		newAttrMap.remove(helper.getAttrNameOfPubProcFail());
		newAttrMap.remove(helper.getAttrNameOfEffectDate());
		newAttrMap.remove(helper.getAttrNameDocumentHigherWFFolder());

		// 上位WFフォルダ属性を作成&ステータスを引き継ぎ
		EIMStatus newStatus = targetObj.getStatus();
		if (_treeStack.peek()._attrMap.containsKey(helper.getAttrNameDocumentHigherWFFolder())) {
			// 上位層が上位WFフォルダ属性を持っていたら、そのままもらう
			newAttrMap.put(helper.getAttrNameDocumentHigherWFFolder(),
				_treeStack.peek()._attrMap.get(helper.getAttrNameDocumentHigherWFFolder()));
		} else if (_treeStack.peek().getStatus() != null) {
			// 上位層がステータを持っていたら、上位WFフォルダ属性を作成する。
			newAttrMap.put(helper.getAttrNameDocumentHigherWFFolder(), new EIMAttribute(
					helper.getAttrTypeOfHigherWFFolder(),
					_treeStack.peek().getObj().getId(), null, null));
		}
		//親がステータスを持っていたら、同じステータスをセットする
		if (_treeStack.peek().getStatus() != null)
			newStatus = _treeStack.peek().getStatus();
			WorkFlowUtils.updateObjectStatus(helper.getSession(), targetObj,
				newStatus);

		// 属性値複写
		for (Iterator i = newAttrMap.values().iterator(); i.hasNext();) {
			EIMAttribute attr = (EIMAttribute) i.next();
			boolean canCopy = true;
			for(int ii = 0; ii < noCopyAttribute.length; ii++) {
				if(attr.getType().getDefName().equals(noCopyAttribute[ii])) {
					canCopy = false;
					break;
				}
			}
			
			if(canCopy) {
				AppObjectUtil.setAttr(helper.getSession(), targetObj, attr);
				// 表示色情報のコピー
				DisplayColorUtil.copyFolderDuplicateDisplayColor(
						helper.getSession(), Long.toString(targetObj.getId()), attr, sourceObj, helper);
			}
		}

		// Access
		AccessUtils.createAccess(helper.getSession(), targetObj,
			"EIM.ACCESS.TYPE.COPY_FOLDER_TREE");

		return new ObjectInfo(targetObj, newAttrMap, newStatus, doBreakTopObjNameInherit);
	}

	/**
	 * 指定オブジェクトが持つ属性の名前.vs.属性のマップを生成して返します
	 *
	 * @param obj 対象オブジェクト
	 * @return 指定オブジェクトが持つ属性の名前.vs.属性のマップ
	 */
	public static Map getAttrMap(EIMObject obj) {
		Map ret = new HashMap();
		for (Iterator i = obj.getAttributeList().iterator(); i.hasNext();) {
			EIMAttribute attr = (EIMAttribute) i.next();
			ret.put(attr.getType().getDefName(), attr);
		}
		return ret;
	}
	
	// ### SEARCH FRAMEWORK ADD
	/**
	 * フォルダ複写後にセキュリティを複写します。
	 *
	 * @return 複写したオブジェクト
	 */
	protected EIMObject walkOutInternal(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo, AppObjectConditionHelper helper) throws Exception {

		EIMObject newFolderObj = (EIMObject) _treeStack.pop().getObj();
		// 最後にセキュリティを複写する
		SecurityUtils.setSecurity(helper.getSession(), newFolderObj,
			lowerFolderObjFrom.getSecurity());
		
		return newFolderObj;
	}
}

/**
 * 複製中のオブジェクトの情報を格納するクラス
 */
static class ObjectInfo {
	/**
	 * 複製中のオブジェクト
	 */
	private EIMObject _obj;

	/**
	 * 複製中のオブジェクトに割り当てた属性。<br>
	 * DBからの再読み込みを行わないようにする為、オブジェクトにsetAttribute()した値をここで管理する。
	 */
	private Map _attrMap;

	/**
	 * 複製中のオブジェクトに割り当てたステータス。<br>
	 * DBからの再読み込みを行わないようにする為、オブジェクトにupdateObjectStatus()した値をここで管理する。
	 */
	private EIMStatus _status;

	/**
	 * トップフォルダの名称の下位引継ぎが断絶しているかのフラグ
	 */
	private boolean _doBreakTopObjNameInherit;

	/**
	 * コンストラクタ
	 *
	 * @param obj オブジェクト
	 * @param attrMap オブジェクトにセットしたオブジェクト属性のマップ
	 * @param status オブジェクトにセットしたステータス
	 * @param doBreakTopObjNameInherit トップフォルダの名称の下位引継ぎが断絶しているかのフラグ
	 */
	ObjectInfo(EIMObject obj, Map attrMap, EIMStatus status, boolean doBreakTopObjNameInherit) {
		_obj = obj;
		_attrMap = attrMap;
		_status = status;
		_doBreakTopObjNameInherit = doBreakTopObjNameInherit;
	}

	/**
	 * オブジェクト属性のマップを返します。
	 *
	 * @return オブジェクト属性のマップ
	 */
	public Map getAttrMap() {
		return _attrMap;
	}

	/**
	 * ステータスを返します。
	 *
	 * @return ステータス
	 */
	public EIMStatus getStatus() {
		return _status;
	}

	/**
	 * オブジェクトを返します
	 *
	 * @return オブジェクト
	 */
	public EIMObject getObj() {
		return _obj;
	}

	/**
	 * トップフォルダの名称の下位引継ぎが断絶しているかのフラグを返します
	 *
	 * @return トップフォルダの名称の下位引継ぎが断絶しているかのフラグ
	 */
	public boolean doBreakTopObjNameInherit() {
		return _doBreakTopObjNameInherit;
	}
}

/**
 * 新規複製フォルダツリー階層を表現するスタッククラス
 */
static class TreeStack {
	/** フォルダツリースタック */
	private Stack _targetTree = new Stack();

	/**
	 * フォルダツリースタックにオブジェクト情報を積みます
	 *
	 * @param info オブジェクト情報
	 */
	void push(ObjectInfo info) {
		_targetTree.push(info);
	}

	/**
	 * フォルダツリースタックから1つ取り出して、スタックから減らします。
	 *
	 * @return スタック最上位のオブジェクト情報
	 */
	ObjectInfo pop() {
		return (ObjectInfo) _targetTree.pop();
	}

	/**
	 * フォルダツリースタックの最上位のオブジェクト情報を返します。
	 *
	 * @return スタック最上位のオブジェクト情報
	 */
	ObjectInfo peek() {
		return (ObjectInfo) _targetTree.peek();
	}

	/**
	 * フォルダツリースタックがトップレベルかどうかを返します。<br>
	 * サイズが1(親フォルダ情報だけが入っている)ならトップレベル
	 *
	 * @return トップレベルならtrue
	 */
	boolean isTopLevel() {
		return _targetTree.size() == 1;
	}
}
%>