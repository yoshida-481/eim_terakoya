<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>
<%@ page import="common.bo.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

/**
 * リスト作成用クラス
 */
class ObjListMaker {
	private EIMSession _sess;
	private AppObjectConditionHelper _helper;
	/** Constructor */
	ObjListMaker(){}
	ObjListMaker(EIMSession sess, AppObjectConditionHelper helper){
		_sess = sess;
		_helper = helper;
	}
	/**
	 * folderObjId の配下のオブジェクトについて,
	 * taggedList に含まれないものが一つでもある → false
	 * taggedList にすべて含まれている           → true
	 */
	public boolean isAllObjectsUnderInList( EIMObject folderObj, List taggedList ) {
		try {
			ArrayList childObjs = (ArrayList)getChildObjsRecurrently(folderObj);
			for (Iterator i = childObjs.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				boolean isExist = false;
				for( Iterator j = taggedList.iterator(); j.hasNext(); ) {
					EIMObject taggedObj = (EIMObject) j.next();
					if( tmpObj.getId() == taggedObj.getId() ) {
						isExist = true;
						break;
					}
				}
				if( isExist == false ) {
					return false;
				}
			}
			return true;
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}
	/**
	 * varList に存在するオブジェクトのうち
	 *  isAddedFolder が TRUE なら, そのオブジェクト + 配下の全オブジェクトも含め
	 *  isAddedFolder が FALSE なら, そのオブジェクトを含め
	 * たリストを作成し返す
	 */
	 public List createToDelList( List varList, List addedFolderFlagList) throws Exception {
		try {
			ArrayList retList = new ArrayList();
			for( int i = 0; i < varList.size(); i++ ) {
				EIMObject obj = ObjectUtils.getObjectById(_sess, Long.parseLong(((String)varList.get(i))));
				if( obj == null || !SecurityUtils.authorized(_sess, obj, _sess.getUser(), EIMAccessRole.READ)) {
					throw new EIMException(_sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				}
				if( ((String)addedFolderFlagList.get(i)).equalsIgnoreCase("true") &&
					_helper.isTypeOfFolder(obj.getType()) == true ) {
					retList.add(obj);
					retList.addAll(getChildObjsRecurrently(obj));
				} else {
					retList.add(obj);
				}
			}
			return retList;
		} catch(EIMException e) {
			throw new EIMException(_sess,e.getMessage());
		} catch( Exception e ) {
			throw new RuntimeException(e);
		}
	}
	/**
	 * obj の配下のオブジェクトのリストを再帰的に返す (返すリストに obj 自身は含まない)
	 */
	public List getChildObjsRecurrently( EIMObject obj ) {
		try {
			ArrayList retList = new ArrayList();
			// 配下のドキュメント・フォルダ・ドキュメントリンクのリンク元ドキュメントを取得
			List childObjs = _helper.getChildObjectsWithDocLinkInAccessibleStatus(obj, null);
			for (Iterator i = childObjs.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				retList.add(tmpObj);
				if( _helper.isTypeOfFolder(tmpObj.getType()) ) {
					retList.addAll(getChildObjsRecurrently(tmpObj));
				}
			}
			return retList;
		} catch( Exception e ) {
			throw new RuntimeException(e);
		}
	}
}


	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	//Parameter
	/* EIMObjID of Tag */
	String prmObjId = request.getParameter("objId");
	String prmObjIds = request.getParameter("objIdToAddDelTag");
	String prmAddedFolders = request.getParameter("isAddedFolder");

	//Message
	String message = null;
	Object[] paramId = { "objId=" + prmObjId,
			             "objIdToAddDelTag=" + prmObjIds,
			             "isAddedFolder=" + prmAddedFolders
			           };

	try {

		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request,"EIM.ERROR.SESSIONTIMEOUT");
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
		
		// User
		user = (EIMUser) sess.getAttribute("USER");

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);


		// インナーヘルパークラス生成
		ObjListMaker maker = new ObjListMaker(sess,helper);

		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			return;
		}
		// 権限チェック
		if (object.getSecurity() != null) {
			if (!SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.UPDATE)) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
		}

		// 可変リスト引数
		String [] ids;
		ArrayList varListIds = null;
		// 固定リストのタグを付与すべきオブジェクト. key = objId, value = true(配下も付与)/false(配下は付与せず)
		LinkedHashMap toAdd = new LinkedHashMap();
		ArrayList toDel = new ArrayList();
		if( prmObjIds != null && !prmObjIds.equals("") ) {
			ids = prmObjIds.split(",");
			varListIds = new ArrayList(Arrays.asList(ids));
		} else {
			varListIds = new ArrayList();
		}

		// 新規に追加されたフォルダ引数
		String [] addedFolders;
		ArrayList addedFoldersAL = null;
		if( prmAddedFolders != null && prmAddedFolders != "" ) {
			addedFolders = prmAddedFolders.split(",");
			addedFoldersAL = new ArrayList(Arrays.asList(addedFolders));
		} else {
			addedFolders = null;
			addedFoldersAL = new ArrayList();
		}

		// 固定リストのタグが付与されているオブジェクトのリスト取得
		ArrayList objList = (ArrayList)TagUtil.getTagGivenObj(sess,object);
		boolean existFlag = false;
		if( objList != null ) {
			// タグ付与すべきオブジェクトと, それらを再帰的に行うか否かを判定
			// 可変リストに存在しているオブジェクト１つ１つにつき
			for( int i = 0; i < varListIds.size(); i++ ) {
				long varObjId = Long.parseLong((String)varListIds.get(i));
				EIMObject varObj = ObjectUtils.getObjectById(sess,varObjId);
				if (varObj == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
					message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
					return;
				}
				// 現在そのタグが付与されているか?
				for( Iterator j1 = objList.iterator(); j1.hasNext();) {
					EIMObject tmp1Obj = (EIMObject) j1.next();
					if( varObjId == tmp1Obj.getId() ) {
						// 付与されている場合, フラグを立て(下記を除き)なにもしない
						existFlag = true;
						// 1) 当該オブジェクトがフォルダ
						// 2) 今回可変リストに新規追加
						// 3) 配下のオブジェクトすべてにタグが付与されているわけではない
						// 場合は, 配下も再帰的に付与
						if( addedFolders[i].equalsIgnoreCase("true") &&
							helper.isTypeOfFolder(varObj.getType()) == true &&
							maker.isAllObjectsUnderInList(varObj,objList) == false ) {
							toAdd.put( varListIds.get(i), "true" );
						}
						break;
					}
				}
				if( existFlag == false ) {
					// 付与されていない場合
					if( addedFolders[i].equalsIgnoreCase("true") &&
						helper.isTypeOfFolder(varObj.getType()) == true ) {
						// 1) 当該オブジェクトがフォルダ
						// 2) 今回可変リストに新規追加
						// の場合は, 配下も再帰的に付与
						toAdd.put( varListIds.get(i), "true" );
					} else {
						// そうでない場合は, そのオブジェクトのみに付与
						toAdd.put( varListIds.get(i), "false" );
					}
				}
				existFlag = false;
			}
			// タグ除去すべきオブジェクトを判定
			ArrayList toDelIds = (ArrayList)maker.createToDelList(varListIds,addedFoldersAL);
			for (Iterator i2 = objList.iterator(); i2.hasNext();) {
				EIMObject tmp2Obj = (EIMObject) i2.next();
				for( Iterator j2 = toDelIds.iterator(); j2.hasNext();) {
					EIMObject tmp3Obj = (EIMObject) j2.next();
					if( tmp3Obj.getId() == tmp2Obj.getId() ) {
						existFlag = true;
						break;
					}
				}
				if( existFlag == false ) {
					toDel.add( tmp2Obj );
				} else {
					// 両方に存在したのでなにもしない
					existFlag = false;
				}
			}
		} else {
			// 可変リストに存在したものは, すべて新規付与
			for( Iterator it = varListIds.iterator(); it.hasNext(); ) {
				String id = (String)it.next();
				toAdd.put( id, "true");
			}
		}
		
		// 検索FW 更新通知用 タグ削除の準備
		TagTreeItem removePlanTag = null;
		HashMap<Long, EIMObject> removeObjMap = new HashMap();
		if(AppUpdateNoticeUtils.doEntry())
		{
			//本jspのタグ削除では削除予定のタグを１つずつTagUtil.removeTag()に渡しているため、
			//TagUtil.removeTag()ではツリー構成がわからない。更新通知ではツリー構成が必要なため、ここで取得。
			removePlanTag = TagUtil.getTagTree(sess,object);
			EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "None");
			
		}

		// タグ削除
		for( Iterator d = toDel.iterator(); d.hasNext();) {
			EIMObject targetObj = (EIMObject)d.next();
			TagUtil.removeTag(sess,targetObj,object,false, removeObjMap);

			// アクセス履歴はremoveTag()の内部で登録されるため、本関数での処理は不要

		}

		// 検索FW 更新通知用 タグ削除の通知
		if(AppUpdateNoticeUtils.doEntry())
		{
			AppUpdateNoticeUtils.updateNoticeInsertTagRemove(sess, object, removePlanTag, removeObjMap);
		}
		
		// 属性タイプの冗長な取得回避用Mapを生成
		HashMap attrTypeMap = TagUtil.getTagAttrTypeMap(sess);

		// タグ付与
		boolean isTagSignFlagOff = false;
		EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "SelectTag");
		for( Iterator a = toAdd.entrySet().iterator(); a.hasNext();) {
			Map.Entry entry = (Map.Entry)a.next();
			String tmpObjId = (String)entry.getKey();
			String isRecursive = (String)entry.getValue();
			EIMObject targetObj = ObjectUtils.getObjectById(sess, Long.parseLong(tmpObjId));
			if (targetObj == null || !SecurityUtils.authorized(sess, targetObj, user, EIMAccessRole.READ)) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
			if( isRecursive.equalsIgnoreCase("true") ) {
				isTagSignFlagOff = isTagSignFlagOff | TagUtil.assignTag(sess, targetObj, object, user, attrTypeMap, helper);
			} else {
				isTagSignFlagOff = isTagSignFlagOff | TagUtil.assignTag(sess, targetObj, object, user, attrTypeMap, helper, false);
			}

			// アクセス履歴はassignTag()の内部で登録されるため、本関数での処理は不要

		}
		TagUtil.getOnlyTagTree(sess,object);
		// duplicate name check
		TagUtil.isDupObjNameUnderTag(sess,object);

		if (isTagSignFlagOff) {

			long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(),
					AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
				// 署名・暗号化済の場合、署名・暗号化未実施に戻す
				AppObjectUtil.setAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);

				// 親タグの署名・暗号化状態を更新
				SignUtil.setParentTagSignFlagOff(sess, object, "SEARCHFW_SELECTTAG_PARENT_TAG");

			}
		}
		
		// SearchFramework 検索FW更新通知 対象タグ
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SELECTTAG_TAG");
		
		// 操作履歴
		String path = AppObjectUtil.getPath(object);
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.ADD_TAG_A,
			AppConstant.ADDED_TAG, EIMConstant.OBJECT, object,
			null, null, null,
			path);

		//Commit
		sess.commit();
		out.println("<ok/>");
	} catch (EIMException eime) {
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
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	} catch (Exception e) {
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
			message = EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()),se);
		}
	}
%>
