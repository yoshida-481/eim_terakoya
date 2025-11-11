<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%

//----------------------------------------------------------------------------
//ワークスペース管理機能追加のため、app/folder/dspProperty.jspをコピーして編集
//※ワークスペースの参照、編集時に呼び出される
//															(2012/02/17)
//----------------------------------------------------------------------------

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmObjId = request.getParameter("wsId");

	//Message
	String message = null;
	Object[] paramId = {
			"wsId=" + prmObjId
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

		//User
		user = (EIMUser)sess.getAttribute("USER");

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 「ワークスペース作成」権限がなければ、読み取り権限があるかチェック
		if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
			if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.READ) == false)
			{
				message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.NOACCESS");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.NOACCESS");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}

		// 下位フォルダ管理セキュリティ情報取得
		boolean checkedThisLowerSuccession = false;
		long lowerSuccessionSecId = Integer.MIN_VALUE;
		String lowerSuccessionSecName = "";

		long secId = AppObjectUtil.getIntAttr(sess,
					object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
		if (secId != Integer.MIN_VALUE)
		{
			checkedThisLowerSuccession = true;
			lowerSuccessionSecId = secId;

			EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, lowerSuccessionSecId);
			lowerSuccessionSecName = eimSec.getName();
		}

		//XML
		out.println("<object");

			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");

			//Security
			boolean updateRole = false;
			if(object.getSecurity() != null)
			{
				out.println(" secId=\"" + object.getSecurity().getId() + "\"");
				out.println(" secName=\"" + StringUtils.xmlEncode(object.getSecurity().getName()) + "\"");
				out.println(" secDefName=\"" + StringUtils.xmlEncode(object.getSecurity().getDefName()) + "\"");
			}

			out.println(" checkedThisLowerSuccession=\"" + checkedThisLowerSuccession + "\"");

			if (lowerSuccessionSecId != Integer.MIN_VALUE)
			{
				out.println(" lowerSuccessionSecId=\"" + lowerSuccessionSecId + "\"");
				out.println(" lowerSuccessionSecName=\"" + StringUtils.xmlEncode(lowerSuccessionSecName) + "\"");
			}

			// 名称割当て属性チェック
			long nameAllocate = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_NAME_ATTR"), Integer.MIN_VALUE);
			long objIdNameAllocate = 0;
			int isNameAllocate = 0;
			// 名称割当て属性有りの場合
			if (nameAllocate != Integer.MIN_VALUE) {
				objIdNameAllocate = nameAllocate;
				// 名称割り当て属性の属性IDを設定
				out.println(" objIdNameAllocate=\"" + objIdNameAllocate + "\"");
				isNameAllocate = 1;
			}
			// 名称割当て属性設定
			out.println(" nameAllocate=\"" + isNameAllocate + "\"");

			// 手動削除禁止フラグ
			long isManualDeleteFlag = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_MANUAL_DELETE_PROHIBITED_FLAG"), Integer.MIN_VALUE);
			if (isManualDeleteFlag != Integer.MIN_VALUE) {
				out.println(" isManualDeleteFlag=\"" + (isManualDeleteFlag == 1) + "\"");
			} else {
				out.println(" isManualDeleteFlag=\"false\"");
			}

			// ワークスペース管理権限を持つかどうかチェック
			boolean isWorkspaceSystemAuth = EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE);
			
			out.println(" isWorkspaceSystemAuth=\"" + isWorkspaceSystemAuth + "\"");

		out.println(">");
		out.println("</object>");
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
