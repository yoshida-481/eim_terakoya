<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "java.util.*" %>

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
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		else if(!SecurityUtils.authorized(sess, object, sess.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ))
		{
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.NOACCESS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.NOACCESS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// (該当オブジェクトの)下位フォルダ管理セキュリティ取得
		boolean checkedThisLowerSuccession = false;
		long secId = AppObjectUtil.getIntAttr(sess,
					object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
		if (secId != Integer.MIN_VALUE)
		{
			checkedThisLowerSuccession = true;
		}

		// (親オブジェクトの)下位フォルダ管理セキュリティ取得
		long lowerSuccessionSecId = Integer.MIN_VALUE;
		boolean checkedParentLowerSuccession = false;
		boolean isWorkSpace = false;
		boolean wsMngRole = false;

		if (helper.isTypeOfWorkspace(object.getType()))
		{
			isWorkSpace = true;

			lowerSuccessionSecId = secId;

			if (lowerSuccessionSecId != Integer.MIN_VALUE)
			{
				EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, lowerSuccessionSecId);

				// Check Role
				if(AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					wsMngRole = true;
				}
				else
				{
					wsMngRole = false;
				}
			}
			else
			{
				wsMngRole = true;
			}

			checkedParentLowerSuccession = false;
		}
		// 選択オブジェクトがごみ箱であった場合
		else if ( helper.isTypeOfRecycle(object.getType()))
		{
			// 何もしない(システムエラー回避)
		}
		else
		{
			isWorkSpace = false;

			//初期選択するセキュリティを取得
			List upperObjList = helper.getUpperObjectsToToplevel(object);
			EIMObject wsObj = ((EIMObject)upperObjList.get(upperObjList.size()-1));//最後のオブジェクトを取得

			lowerSuccessionSecId = AppObjectUtil.getIntAttr(sess,
						wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);

			EIMObject parentObj = helper.getUpperObject(object);

			long parentSecId = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);

			if (parentSecId != Integer.MIN_VALUE)
			{
				EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, parentSecId);

				// Check Role
				if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					wsMngRole = true;
				}
				else
				{
					wsMngRole = false;
				}
				checkedParentLowerSuccession = true;
			}
			else
			{
				wsMngRole = true;
				checkedParentLowerSuccession = false;
			}
		}

		String lowerSuccessionSecName = "";
		if (lowerSuccessionSecId != Integer.MIN_VALUE)
		{
			EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, lowerSuccessionSecId);
			lowerSuccessionSecName = eimSec.getName();
		}

		//XML
		out.println("<object");

			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");
			out.println(" createDate=\"" + object.getCreateDate() + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + object.getModifyDate() + "\"");

			//Security
			boolean updateRole = false;
			if(object.getSecurity() != null)
			{
				out.println(" secId=\"" + object.getSecurity().getId() + "\"");
				out.println(" secName=\"" + StringUtils.xmlEncode(object.getSecurity().getName()) + "\"");
				out.println(" secDefName=\"" + StringUtils.xmlEncode(object.getSecurity().getDefName()) + "\"");

				//Check Upate Role
				if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)
						&& wsMngRole)
				{
					updateRole = true;
				}
				else
				{
					updateRole = false;

				}

			}
			out.println(" updateRole=\"" + updateRole + "\"");

			out.println(" checkedThisLowerSuccession=\"" + checkedThisLowerSuccession + "\"");
			out.println(" checkedParentLowerSuccession=\"" + checkedParentLowerSuccession + "\"");

			if (lowerSuccessionSecId != Integer.MIN_VALUE)
			{
				out.println(" lowerSuccessionSecId=\"" + lowerSuccessionSecId + "\"");
				out.println(" lowerSuccessionSecName=\"" + StringUtils.xmlEncode(lowerSuccessionSecName) + "\"");
			}
			out.println(" isWorkSpace=\"" + isWorkSpace + "\"");

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
