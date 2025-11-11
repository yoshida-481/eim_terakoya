<%@page import="java.text.SimpleDateFormat"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.TagTreeItem" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class JudgeTargetUtils
	{
		AppObjectConditionHelper _helper = null;
		Map _checkedObjIdMap = null;
		
		/**
		 * コンストラクタ
		 */
		public JudgeTargetUtils()
		{
		}
		
		/**
		 * 再帰的にタグをたどり、署名・暗号化対象のドキュメントを取得する
		 * 
		 * @param sess EIMSessionインスタンス
		 * @param tagObj タグオブジェクト
		 * @param targetList 署名・暗号化対象ドキュメント
		 */
		public void getTarget(EIMSession sess, EIMObject tagObj, List targetList, List tagList) throws Exception
		{
			if (_helper == null)
			{
				// 条件判定ヘルパー作成
				_helper = new AppObjectConditionHelper(sess);
			}
			if (_checkedObjIdMap == null)
			{
				// チェック済オブジェクトIdマップ作成
				_checkedObjIdMap = new HashMap();
			}
			
			List tagGivenObjList = TagUtil.getTagGivenObj(sess, tagObj);
			for (Iterator iter = tagGivenObjList.iterator(); iter.hasNext();) {
				EIMObject tagGivenObj = (EIMObject) iter.next();
				
				if (_checkedObjIdMap.get(String.valueOf(tagGivenObj.getId())) != null)
				{
					// チェック済み
					continue;
				}
				// チェック済みマップに追加
				_checkedObjIdMap.put(String.valueOf(tagGivenObj.getId()), "");

				// 署名・暗号化状態をチェック
				long signencr = AppObjectUtil.getIntAttr(sess, tagGivenObj, _helper.getAttrNameOfSignEncStatus(),
														AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
				if (signencr != AppConstant.SIGNENCR_KIND_SIGNENCR)
				{
					if (_helper.isTypeOfDocument(tagGivenObj.getType()))
					{
						// ドキュメントの場合
						// 拡張子をチェック
						if (SignUtil.isSignEncrTarget(sess, tagGivenObj))
						{
							// 処理対象リストに追加
							targetList.add(tagGivenObj);
						}
					}
					else if (_helper.isTypeOfTag(tagGivenObj.getType()))
					{
						// タグの場合
						// さらにこのタグが付与されたドキュメントを取得
						getTarget(sess, tagGivenObj, targetList, tagList);
						tagList.add(tagGivenObj);
					}
				}
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
	String prmObjId[] = request.getParameterValues("objId");

	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
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
				

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// 選択オブジェクトリスト
		List selectedObjList = new ArrayList();
		// 署名・暗号化対象ドキュメントリスト
		List signencrList = new ArrayList();
		
		//
		// エラーチェック＆署名・暗号化対象ドキュメントを取得
		//
		for( int ii = 0; ii < prmObjId.length; ii++ ) {
			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId[ii]));
			if(object == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			selectedObjList.add(object);

			boolean isTagType = helper.isTypeOfTag(object.getType());

			// 対象オブジェクトのセキュリティのロールチェック
			if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				// 署名・暗号化権限がありません。
				if (isTagType) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.ROLE.FOR.TAG",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
				} else {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.ROLE.FOR.DOC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
				}
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				if (isTagType) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.ROLE.FOR.TAG",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
				} else {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.ROLE.FOR.DOC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
				}
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			// 署名・暗号化状態、拡張子をチェック
			long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(),
													AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			if (isTagType)
			{
				//タグ
				if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXECUTED.SIGN.AND.ENCR.FOR.TAG",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXECUTED.SIGN.AND.ENCR.FOR.TAG",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			else
			{
				//ドキュメント
				if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXECUTED.SIGN.AND.ENCR.FOR.DOC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXECUTED.SIGN.AND.ENCR.FOR.DOC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
				else if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ALREADY.SIGN.AND.ENCR.PROC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.ALREADY.SIGN.AND.ENCR.PROC",
													 new Object[]{StringUtils.xmlEncode(object.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
				
				// 拡張子のチェック
				if (!SignUtil.isSignEncrTarget(sess, object))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OUT.OF.SIGN.AND.ENCR",
							 new Object[]{StringUtils.xmlEncode(object.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.OUT.OF.SIGN.AND.ENCR",
							 new Object[]{StringUtils.xmlEncode(object.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}

			// 署名・暗号化対象リストを作成する
			List targetDocList = new ArrayList();
			List tagList = new ArrayList();
			if (isTagType)
			{
				// 署名・暗号化対象ドキュメント取得用Utils
				JudgeTargetUtils utils = new JudgeTargetUtils();
				
				// タグの場合、タグが付与されたドキュメントを取得（再帰処理）
				utils.getTarget(sess, object, targetDocList, tagList);
				
				// ### SEARCH FRAMEWORK 検索FW更新通知 オブジェクトID・処理種別キーを指定 
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SIGNENCRYPT_TAG");
				AppUpdateNoticeUtils.updateNoticeInsertObjectList(tagList, "SEARCHFW_SIGNENCRYPT_CHILD_TAG");
				AppUpdateNoticeUtils.updateNoticeInsertObjectList(targetDocList, "SEARCHFW_SIGNENCRYPT_CHILD_DOCUMENT");
			}
			else
			{
				// ドキュメントの場合、署名・暗号化対象リストに追加
				targetDocList.add(object);
				
				// ### SEARCH FRAMEWORK 検索FW更新通知 オブジェクトID・処理種別キーを指定   
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SIGNENCRYPT_DOCUMENT");
			}
			signencrList.add(targetDocList);
		}
	
		//
		// バッチ処理用の署名・暗号化オブジェクトを作成
		//
		
		// 実施日時文字列を取得
		Date date = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String execDate = "_" + sdf.format(date);
		
		EIMObjectType signencObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGN_ENC"));
		for( int ii = 0; ii < prmObjId.length; ii++ )
		{
			EIMObject selectedObject = (EIMObject)selectedObjList.get(ii);
			boolean isTagType = helper.isTypeOfTag(selectedObject.getType());
			ArrayList targetDocList = (ArrayList)signencrList.get(ii);
			
			// 対象ドキュメント分ループ
			for (int jj = 0; jj < targetDocList.size(); jj++)
			{
				EIMObject docObj = (EIMObject)targetDocList.get(jj);
				long signencr = AppObjectUtil.getIntAttr(sess, docObj, helper.getAttrNameOfSignEncStatus(),
														AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
				if (signencr != AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR)
				{
					//更新権限がありません。
					if(!SecurityUtils.authorized(sess, docObj, sess.getUser(), EIMAccessRole.UPDATE))
					{
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
					// 署名・暗号化状態を「処理中」に変更
					AppObjectUtil.setAttr(sess, docObj, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR);
				}
				
				// 署名・暗号化オブジェクトを作成
				EIMObject signencObj = ObjectUtils.createObject(sess, signencObjType, sess.getUser().getId() + execDate);
				AppObjectUtil.setAttr(sess, signencObj, EIMConfig.get("ATTR_NAME_SIGNENC_TARGET_DOC"), docObj.getId());
				AppObjectUtil.setAttr(sess, signencObj, EIMConfig.get("ATTR_NAME_SIGNENC_OPERATOR"), sess.getUser().getId());
				if (isTagType)
				{
					AppObjectUtil.setAttr(sess, signencObj, EIMConfig.get("ATTR_NAME_SIGNENC_SELECT_TAG"), selectedObject.getId());
				}
			}
			
			
			// 操作履歴を作成
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(selectedObject));
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.SIGN_AND_ENCR, 
					AppConstant.TARGET_TO_SIGN_AND_ENCR, EIMConstant.OBJECT, selectedObject,
					null, null, null, path);
		}
		

		// 署名・暗号化状態の更新
		for( int ii = 0; ii < prmObjId.length; ii++ )
		{
			EIMObject selectedObject = (EIMObject)selectedObjList.get(ii);
			if (helper.isTypeOfTag(selectedObject.getType()))
			{
				EIMThreadContext.put("SEARCHFW.SIGNTAG.KEY", null);
				EIMThreadContext.put("SEARCHFW.SIGNCHILDTAG.KEY", null);
				SignUtil.setTagSignFlagOn(sess, selectedObject);
			}
		}
		
		sess.commit();
		out.println("<OK></OK>");
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