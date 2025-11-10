<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

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
	/* EIMObjID of Tag */
	String prmObjId = request.getParameter("objId");
	String prmObjIds = request.getParameter("objIdToAddDelTag");

	//Message
	String message = null;
	Object[] paramId = { "objId=" + prmObjId,
			             "objIdToAddDelTag=" + prmObjIds};

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


		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			return;
		}

		// 可変リスト引数
		String [] ids;
		ArrayList varListIds = null;
		ArrayList toAdd = new ArrayList();
		ArrayList toDel = new ArrayList();
		if( prmObjIds != null && !prmObjIds.equals("") ) {
			ids = prmObjIds.split(",");
			varListIds = new ArrayList(Arrays.asList(ids));
		} else {
			varListIds = new ArrayList();
		}

		// 付与されているタグ取得
		ArrayList objList = new ArrayList();
		long[] tags = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if( tags != null ) {
			for( int i = 0; i < tags.length; i++ ) {
				EIMObject tagObj = ObjectUtils.getObjectById(sess, tags[i]);
				if( tagObj != null && SecurityUtils.authorized(sess, tagObj, user, EIMAccessRole.READ)) {
					// 属性に ID があるが, 取得できないものは読み取り権限がないと判断し何もしない
					objList.add(tagObj);
				}
			}
		}
		boolean existFlag = false;
		if( objList != null ) {
			// 可変リストに追加されたものを探す
			for( Iterator i1 = varListIds.iterator(); i1.hasNext();) {
				String tmpObj = (String) i1.next();
				for( Iterator j1 = objList.iterator(); j1.hasNext();) {
					EIMObject tmp1Obj = (EIMObject) j1.next();
					if( Long.parseLong(tmpObj) == tmp1Obj.getId() ) {
						existFlag = true;
						break;
					}
				}
				if( existFlag == false ) {
					toAdd.add( tmpObj );
				} else {
					// 両方に存在したのでなにもしない
					existFlag = false;
				}
			}
			// 可変リストからなくなっているものを探す
			for (Iterator i2 = objList.iterator(); i2.hasNext();) {
				EIMObject tmp2Obj = (EIMObject) i2.next();
				for( Iterator j2 = varListIds.iterator(); j2.hasNext();) {
					String tmp3Obj = (String) j2.next();
					if( Long.parseLong(tmp3Obj) == tmp2Obj.getId() ) {
						existFlag = true;
						break;
					}
				}
				if( existFlag == false ) {
					toDel.add( String.valueOf(tmp2Obj.getId()) );
				} else {
					// 両方に存在したのでなにもしない
					existFlag = false;
				}
			}
		} else {
			// 可変リストに存在したものは, すべて新規付与
			toAdd = varListIds;
		}
		
		// タグ削除
		EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "SelectDoc");
		for( Iterator d = toDel.iterator(); d.hasNext();) {
			String tmpObjId = (String) d.next();
			EIMObject tagObj = ObjectUtils.getObjectById(sess, Long.parseLong(tmpObjId));
			if (tagObj == null) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
			if (tagObj.getSecurity() != null) {
				if (!SecurityUtils.authorized(sess, tagObj, sess.getUser(),EIMAccessRole.UPDATE)) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
					return;
				}
			}
			// EIMObject 再取得
			object = ObjectUtils.getObjectById(sess, object.getId());
			if (object == null) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
			TagUtil.removeTag(sess,object,tagObj,true);

			// アクセス履歴はremoveTag()の内部で登録されるため、本関数での処理は不要

		}
		// 属性タイプの冗長な取得回避用Mapを生成
		HashMap attrTypeMap = TagUtil.getTagAttrTypeMap(sess);

		// タグ付与
		boolean isTagSignFlagOff = false;
		for( Iterator a = toAdd.iterator(); a.hasNext();) {
			String tmpObjId = (String) a.next();
			EIMObject tagObj = ObjectUtils.getObjectById(sess, Long.parseLong(tmpObjId));
			if (tagObj == null) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
			if (tagObj.getSecurity() != null) {
				if (!SecurityUtils.authorized(sess, tagObj, sess.getUser(),EIMAccessRole.UPDATE)) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
					return;
				}
			}
			// EIMObject 再取得
			object = ObjectUtils.getObjectById(sess, object.getId());
			if (object == null) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
				return;
			}
			isTagSignFlagOff = isTagSignFlagOff | TagUtil.assignTag(sess, object, tagObj, user, attrTypeMap, helper);
			
			// アクセス履歴はassignTag()の内部で登録されるため、本関数での処理は不要

		}
		// EIMObject 再取得
		object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			return;
		}
		// loop, duplinate name check
		tags = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if( tags != null ) {
			for( int i = 0; i < tags.length; i++ ) {
				EIMObject tagobj = ObjectUtils.getObjectById(sess,tags[i]);
				if (tagobj != null || !SecurityUtils.authorized(sess, tagobj, user, EIMAccessRole.UPDATE)) {
					TagUtil.getOnlyTagTree(sess,object);
					// duplicate name check
					TagUtil.isDupObjNameUnderTag(sess,tagobj);
				}
			}
		}
		if (isTagSignFlagOff) {

			// 親タグの署名・暗号化状態を更新
			SignUtil.setParentTagSignFlagOff(sess, object, "SEARCHFW_SELECTDOC_PARENT_TAG");
		}
		
		// 操作履歴
		String path = AppObjectUtil.getPath(object);
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.ADD_TAG_B,
			AppConstant.TARGET_TO_ADD, EIMConstant.OBJECT, object,
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
