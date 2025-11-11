<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
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
		
		if(object != null && SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			// ワークスペースの場合
			if(!helper.isTypeOfFolder(object.getType()))
			{
				//エラー発生フラグ
				boolean enabled = false;
				
				// 下位フォルダ管理セキュリティ取得
				long sec_id = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);

				if(sec_id != Integer.MIN_VALUE) 
				{
					// ワークスペースのセキュリティ&下位フォルダ管理セキュリティのロールチェック
					if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)
							&& AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
					{
						enabled = true;
					}
				}
				else
				{
                    // ワークスペースのセキュリティのみロールチェック
					enabled = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
				}
				
				// セキュリティ管理権限チェック				
				if(!enabled)
				{
					enabled = AdminAuthUtil.hasSpecifiedOrGeneralAuth(user, AppConstant.ADMIN_AUTH_ID_SECURITY);
				}
				
				if(!enabled)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
					log.warn(AppMessageUtils.makeLogMessage(message));
					return;					
				}
			}			
		}
		else{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		//XML
		out.println("<object");
			
			//For Create Document - Default Create User Information
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
			
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");
			out.println(" createDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate()) + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
			
			//Revision
			out.println(" rev=\"" + "-" + "\"");
			
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
			// 名称割当て属性チェック				
			int nameAllocate = (int) AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_NAME_ATTR"), Integer.MIN_VALUE);
			int objIdNameAllocate = 0;
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
