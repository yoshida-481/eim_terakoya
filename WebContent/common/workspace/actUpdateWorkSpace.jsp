<%@page import="java.util.List"%>
<%@page import="java.util.Arrays"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
//--------------------------------Inner Class
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
										boolean			hasWorkSpaceCreateAuth)
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
				if (helper.isTypeOfFolder(childObj.getType()))
				{
					//ワークスペース作成権限があるか
					if(hasWorkSpaceCreateAuth){
						//権限チェックは行わない
					}else{
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
					}
					//Recurrenty
					set(sess, childObj, relType, sec, lowSec, lowCheck, hasWorkSpaceCreateAuth);
				}
				else
				{
					//Apply Security
					SecurityUtils.setSecurity(sess, childObj, sec);
				}
			}
		}
	}
//--------------------------------end

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
	String prmObjId = request.getParameter("objId");
	String prmIsDspAttributeInfo = EIMUtils.getParameter(request, "isDspAttributeInfo");
	String prmIsUpdateSecurity = EIMUtils.getParameter(request, "isUpdateSecurity");
	String prmSecId = request.getParameter("secId");
	String prmCheckedLowerSuccession = request.getParameter("checkedLowerSuccession");
	String prmLowerSuccessionSecId = request.getParameter("lowerSuccessionSecId");
	String newWorkspaceName = EIMUtils.getParameter(request,"objName");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"isDspAttributeInfo=" + prmIsDspAttributeInfo,
			"isUpdateSecurity=" + prmIsUpdateSecurity
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

		// 操作履歴用：アプリケーション種別
		boolean isAdmin = (session.getAttribute("ADMIN_APP_ID") != null);
		String applicationType = isAdmin ? AppConstant.SYSTEM : AppConstant.DOCUMENT;

		// Check login user
		user = (EIMUser)sess.getAttribute("USER");

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// Check Object Type
		if(!object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//権限チェック
		boolean enabled = false;	//更新可能フラグ
		boolean hasWorkSpaceCreateAuth = false;		//ワークスペース作成権限あり

		if(EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
			//ワークスペースの作成権限があるなら更新可
			enabled = true;
			hasWorkSpaceCreateAuth = true;
		}
		else{

			// オブジェクトに対して更新権限があるか
			boolean hasUpdateRole = false;
			// 下位フォルダ管理セキュリティ取得
			long sec_id = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
			if(sec_id != Integer.MIN_VALUE) 
			{
				// ワークスペースのセキュリティ&下位フォルダ管理セキュリティでチェック
				if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)
						&& AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					hasUpdateRole = true;
				}
			}
			else
			{
                // ワークスペースのセキュリティでチェック
				hasUpdateRole = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
			}

			if(hasUpdateRole && WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object)){
				// 更新権限があるかつワークスペースの責任者であるなら更新可
				enabled = true;
			}
		}

		if(object != null && enabled == false){
			// ワークスペース責任者は更新可
			enabled = WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, object);
			hasWorkSpaceCreateAuth = true;
		}

		// ここまで権限チェック
		if(enabled == false)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// 名称変更チェック
		boolean isChangeWorkSpaceName = false;
		if(!object.getName().equals(newWorkspaceName)){
			isChangeWorkSpaceName = true;
		}
		//---------------------------------------------------------------------
		//セキュリティの更新
		//※以下app/document/security/actApplySecurity.jspを参考に追加
		//---------------------------------------------------------------------
		boolean isUpdateSecurity = (prmIsUpdateSecurity.equals("true"));
		if(isUpdateSecurity){


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
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

			//Recurrently
			RecurrentUtils ru = new RecurrentUtils(sess);
			ru.set(sess, object, relType, sec, lowSec, prmCheckedLowerSuccession, hasWorkSpaceCreateAuth);

			// SearchFramework 検索FW更新通知 対象：ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SET_SECURITY_WORKSPACE");
			// SearchFramework 検索FW更新通知 対象：配下のフォルダ・ドキュメント・タグ
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_SET_SECURITY_CHILD_FOLDER", "SEARCHFW_SET_SECURITY_CHILD_DOCUMENT", "SEARCHFW_SET_SECURITY_CHILD_TAG", null);

			// ワークスペースの場合、パス属性の値を保持していない
			String path = "/";
			//Create Operation History
			OperationHistoryUtils.create(sess, applicationType, EIMConstant.APPLY_SECURITY,
					EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
					EIMConstant.TARGET_APPLIED_SECURITY, EIMConstant.SECURITY, sec, path);
		}

		//属性情報および名称の更新
		boolean isUpdateAttribute = (prmIsDspAttributeInfo.equals("1"));

		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, object, !isUpdateAttribute);

		//属性表示色の更新
		DisplayColorUtil.updateDisplayColor(sess, request, object);


		//WS管理に必要な各種属性情報の更新
		//***責任者/使用可能タイプ/使用可能セキュリティ情報の更新***
		WorkSpaceUtil.updateAttributeForWSManagement(sess, request, object);

		// SearchFramework 検索FW更新通知 対象：ワークスペース + 配下のフォルダ・ドキュメント・タグ・ドキュメントリンク
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_UPDATE_WORKSPACE");
		AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_UPDATE_WORKSPACE_CHILD_FOLDER",
				"SEARCHFW_UPDATE_WORKSPACE_CHILD_DOCUMENT", "SEARCHFW_UPDATE_WORKSPACE_CHILD_TAG", "SEARCHFW_UPDATE_WORKSPACE_CHILD_LINK");

		//Create Operation History
		OperationHistoryUtils.create(sess, applicationType, EIMConstant.UPDATE_WORKSPACE,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, null);

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
