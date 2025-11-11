<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class RecurrentUtils
	{
		
		private AppObjectConditionHelper helper;
		
	   /**
		* コンストラクタ
		*/
		public RecurrentUtils(EIMSession sess)
		{
			super();
			
			helper = new AppObjectConditionHelper(sess);

		}
		
		public void set(EIMSession		sess,
										EIMObject		object,
										EIMRelationType	relType,
										EIMSecurity		sec,
										EIMSecurity		lowSec,
										String			lowCheck,
										String          isNotApplyUnderFolder)
		throws Exception
		{
			//Apply Security
			SecurityUtils.setSecurity(sess, object, sec);
			
			if (lowCheck != null)	// チェックがNULLの場合は、更新対象外(チェックボックスがenable=false)
			{	
				if (lowCheck.equals("true")) {
					
					// 値が異なる場合あるいは、チェックした該当オブジェクトの場合のみ更新（値がない場合は更新しない）
					AppObjectUtil.setAttr(sess, 
						object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), lowSec.getId());

				}
				else
				{
					AppObjectUtil.deleteAttribute(sess, 
							object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"));
				}
			}
			
			//Child Relation
			List childRelList = RelationUtils.getChildRelationListByRelType(sess, object, relType,EIMAccessRole.READ);
			for(int i = 0; i < childRelList.size(); i++)
			{
				//Relation
				EIMRelation relation = (EIMRelation)childRelList.get(i);
				
				//Child Object
				EIMObject childObj = relation.getChild();
				//子オブジェクトタイプがフォルダでありかつ、セキュリティ更新チェックボックスにチェックがない場合
				if (helper.isTypeOfFolder(childObj.getType()) && isNotApplyUnderFolder.equals("false"))
				{
					//権限チェック
					boolean enabled = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
					if(enabled != true)
					{
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
					}
					
					//フォルダ構成管理をチェック
					if (! AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE, sec)) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
					}
					
					//Recurrenty
					set(sess, childObj, relType, sec, lowSec, lowCheck, isNotApplyUnderFolder);
				}
				//子オブジェクトタイプがタグでありかつ、セキュリティ更新チェックボックスにチェックがない場合
				//または子オブジェクトタイプがドキュメントの場合
				else if((helper.isTypeOfTag(childObj.getType()) && isNotApplyUnderFolder.equals("false"))
						|| helper.isTypeOfDocument(childObj.getType()))
				{
					//Apply Security
					SecurityUtils.setSecurity(sess, childObj, sec);
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
	String prmObjId = request.getParameter("objId");
	String prmSecId = request.getParameter("secId");
	String prmLowerSuccessionSecId = request.getParameter("lowerSuccessionSecId");
	String prmCheckedLowerSuccession = request.getParameter("checkedLowerSuccession");
	String prmIsNotApplyUnderFolder = request.getParameter("isNotApplyUnderFolder");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"secId=" + prmSecId,
			"lowerSuccessionSecId=" + prmLowerSuccessionSecId,
			"checkedLowerSuccession=" + prmCheckedLowerSuccession
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		user = (EIMUser)sess.getAttribute("USER");

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//権限チェック
		boolean enabled = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
		if(enabled != true)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//上位フォルダ、またはワークスペースのフォルダ構成管理セキュリティ解除をチェック
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		if (helper.isTypeOfFolder(object.getType()))
		{
			EIMObject parentObj = helper.getUpperObject(object);
			long parentSecId = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
			
			if (prmCheckedLowerSuccession != null
				&& prmCheckedLowerSuccession.equals("true"))
			{
				// もしチェックしたなら、親もチェックされている必要がある。
				if (parentSecId == Integer.MIN_VALUE)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPLY.FOLDERSECURITY");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPLY.FOLDERSECURITY");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			
			//フォルダ構成管理をチェック
			if (! AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
				//parentSecがnullなら、ここには来ない
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			// SearchFramework 検索FW更新通知 対象：フォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SET_SECURITY_FOLDER");
		}
		else if (helper.isTypeOfWorkspace(object.getType()))
		{
			//フォルダ構成管理をチェック
			if (! AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) 
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			// SearchFramework 検索FW更新通知 対象：ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SET_SECURITY_WORKSPACE");
			
		}
		
		// SearchFramework 検索FW更新通知 対象：配下のフォルダ・ドキュメント・タグ 
		AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_SET_SECURITY_CHILD_FOLDER", "SEARCHFW_SET_SECURITY_CHILD_DOCUMENT", "SEARCHFW_SET_SECURITY_CHILD_TAG", null);
		
		
		//Security
		EIMSecurity sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
		
		//下位フォルダ管理セキュリティ
		EIMSecurity lowSec = null;
		if (prmCheckedLowerSuccession != null
				&& prmCheckedLowerSuccession.equals("true"))
		{
			lowSec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmLowerSuccessionSecId));
		}

		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		
		//Recurrently
		RecurrentUtils ru = new RecurrentUtils(sess);
		ru.set(sess, object, relType, sec, lowSec, prmCheckedLowerSuccession, prmIsNotApplyUnderFolder);
		
		//Path
		String path = AppObjectUtil.getPath(object);
		if(path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.APPLY_SECURITY, 
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				EIMConstant.TARGET_APPLIED_SECURITY, EIMConstant.SECURITY, sec, path);

		//Commit
		sess.commit();

		out.println("<ok/>");
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
