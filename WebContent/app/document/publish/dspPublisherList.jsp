<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.EIMAppException" %>

<%
	//Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmObjId = EIMUtils.getParameter(request, "objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
			};

	try {
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		user = (EIMUser) sess.getAttribute("USER");

		StringBuilder sb = new StringBuilder();
		String publicTiming = "";

		do {
			//メール通知オブジェクト取得
			EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
			EIMObject obj = ObjectUtils.getObjectByTypeAndName(sess, objType, prmObjId);
			if (obj == null) {
				break;
			}

			//公開通知タイミング
			EIMAttribute attrPublibTiming = obj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));
			if (attrPublibTiming == null) {
				break;
			}
			publicTiming = Long.toString(attrPublibTiming.getInt());

			//公開通知送信先属性取得
			EIMAttribute attrNotifyTo = obj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
			if (attrNotifyTo == null) {
				break;
			}

			//公開通知送信先読み込み
			String[] notifyToList = attrNotifyTo.getStrings();
			if (notifyToList == null) {
				break;
			}
			for (String notifyTo : notifyToList) {
				int delimiterIndex = notifyTo.indexOf(':', 1);
				if (delimiterIndex == -1) {
					throw new EIMAppException("EIM.ERROR.SYSTEMERROR");
				}
				String notifyToType = notifyTo.substring(0, delimiterIndex);
				long notifyToId = Long.parseLong(notifyTo.substring(delimiterIndex + 1), 10);

				String idAttrKey = null;
				long idAttrValue = 0;
				String nameAttrKey = null;
				String nameAttrValue = null;

				if (notifyToType.equals(AppConstant.ENTRY_TYPE_USER)) {
					EIMUser notifyUser = UserUtils.getUserById(sess, notifyToId);
					if (notifyUser == null) {
						continue;
					}
					idAttrKey = "userId";
					idAttrValue = notifyUser.getId();
					nameAttrKey = "userName";
					nameAttrValue = notifyUser.getName();
				} else if (notifyToType.equals(AppConstant.ENTRY_TYPE_GROUP)) {
					EIMGroup notifyGroup = GroupUtils.getGroupById(sess, notifyToId);
					if (notifyGroup == null) {
						continue;
					}
					idAttrKey = "groupId";
					idAttrValue = notifyGroup.getId();
					nameAttrKey = "groupName";
					nameAttrValue = notifyGroup.getName();
				} else if (notifyToType.equals(AppConstant.ENTRY_TYPE_ROLE)) {
					EIMRole notifyRole = RoleUtils.getRoleById(sess, notifyToId);
					if (notifyRole == null) {
						continue;
					}
					idAttrKey = "roleId";
					idAttrValue = notifyRole.getId();
					nameAttrKey = "roleName";
					nameAttrValue = notifyRole.getName();
				} else if (notifyToType.equals(AppConstant.ENTRY_TYPE_COMP_GROUP)) {
					EIMComp notifyComp = CompUtils.getCompById(sess, notifyToId);
					if (notifyComp == null) {
						continue;
					}
					idAttrKey = "compId";
					idAttrValue = notifyComp.getId();
					nameAttrKey = "compName";
					nameAttrValue = notifyComp.getName();
				} else {
					throw new EIMAppException("EIM.ERROR.SYSTEMERROR");
				}

				sb.append("<publisher");
				sb.append(' ').append(idAttrKey).append("=\"").append(idAttrValue).append('"');
				sb.append(' ').append(nameAttrKey).append("=\"").append(StringUtils.xmlEncode(nameAttrValue)).append('"');
				sb.append("/>");
			}
		} while (false);

		//Root Node
		out.println("<publisherList");
		out.println(" publicTiming=\"" + publicTiming + "\"");
		out.println(">");

		out.println(sb);

		//End Root Node
		out.println("</publisherList>");

	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
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
