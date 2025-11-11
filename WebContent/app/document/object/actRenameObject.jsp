<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "java.util.*" %>
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
	String prmObjId = EIMUtils.getParameter(request, "objId");
	String prmIsDspAttributeInfo = EIMUtils.getParameter(request, "isDspAttributeInfo");
		
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"isDspAttributeInfo=" + prmIsDspAttributeInfo
			};
	
	//タグの署名・暗号化状態判定フラグ
	boolean checkTagSignFlag = false;

	try 
	{
		//Session
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
		
		user = (EIMUser)sess.getAttribute("USER");
		

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 対象オブジェクトのセキュリティのロールチェック
		if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
			// 改名権限がありません。
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NORENAMEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;			
		}
		
		/* フォルダ構成管理チェック */		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// 対象オブジェクトがフォルダの場合
		if (helper.isTypeOfFolder(object.getType())) {
			// 下位フォルダ管理セキュリティのロールチェック	
			if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				// 改名権限がありません。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NORENAMEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				sess.rollback();
				return;
			}
			
			// ### SEARCH FRAMEWORK 検索FW更新通知 フォルダ、配下オブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_RENAME_FOLDER");
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_RENAME_CHILD_FOLDER", "SEARCHFW_RENAME_CHILD_DOCUMENT", "SEARCHFW_RENAME_CHILD_TAG", "SEARCHFW_RENAME_CHILD_DOCUMENTLINK");
		}
		
		boolean isUpdateOnlyName = (prmIsDspAttributeInfo.equals("0"));
		
		if (helper.isTypeOfDocument(object.getType())) {
			long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(),
													AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			if (signencr != AppConstant.SIGNENCR_KIND_SIGNENCR) {
				if (!SignUtil.isSignEncrTarget(sess, object)) {
					// 署名・暗号化対象外のドキュメントについては、後でタグの署名・暗号化状態の判定が必要
					checkTagSignFlag = true;
				}
			}
			
			// ### SEARCH FRAMEWORK 検索FW更新通知 ドキュメントオブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_RENAME_DOCUMENT");
		} else if(helper.isTypeOfTag(object.getType())) {
			// ### SEARCH FRAMEWORK 検索FW更新通知 タグオブジェクトID・処理種別キーを指定
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_RENAME_TAG");
		}
		
		//リスト値表示色オブジェクトの更新
		DisplayColorUtil.updateDisplayColor(sess, request, object);
		
		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, object, isUpdateOnlyName);
		
		if (checkTagSignFlag) {
			object = ObjectUtils.getObjectById(sess, object.getId());
			// 署名・暗号化対象外から署名・暗号化対象に改名されたか？
			if (SignUtil.isSignEncrTarget(sess, object)) {

				// 付与されているタグの署名・暗号化状態を更新
				SignUtil.setParentTagSignFlagOff(sess, object, "SEARCHFW_RENAME_PARENT_TAG");
			}
		}

		//Commit
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
			if (sess != null) {
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