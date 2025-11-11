<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmStatusTypeId = request.getParameter("statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"statusTypeId=" + prmStatusTypeId
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Status
		EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
		if(statusType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//Entry List
		EIMWorkFlow wfObj = WorkFlowUtils.getWorkFlowByStatusType(sess,statusType);
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
		EIMObject entry = ObjectUtils.getObjectByTypeAndName(sess,objType,Long.toString(wfObj.getId()));

		EIMAttribute attrTypeId = null;
		EIMAttribute attrTypeObj = null;

		//Root Node
		out.println("<entryList>");
		if( entry != null ) {
			attrTypeId = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE"));
			attrTypeObj = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ"));
			// エントリタイプID, 対象IDが存在
			if( attrTypeId != null && attrTypeObj != null ) {
				// 属性を取得
				long[] ids = TypeConvertUtils.convertToLongArray(attrTypeId.getInts());
				long[] types = TypeConvertUtils.convertToLongArray(attrTypeObj.getInts());
				// エントリリスト (後にソートの上 XML 化)
				List userList = new ArrayList();
				List groupList = new ArrayList();
				List roleList = new ArrayList();
				List compList = new ArrayList();
				for( int j = 0; j < ids.length; j++ ) {
					if( ids[j] == EIMAccessEntryType.USER ) {
						EIMUser entUser = UserUtils.getUserById(sess,types[j]);
						userList.add(entUser);
					} else if( ids[j] == EIMAccessEntryType.GROUP ) {
						EIMGroup entGroup = GroupUtils.getGroupById(sess,types[j]);
						groupList.add(entGroup);
					} else if( ids[j] == EIMAccessEntryType.ROLE ) {
						EIMRole entRole = RoleUtils.getRoleById(sess,types[j]);
						roleList.add(entRole);
					} else if( ids[j] == EIMAccessEntryType.COMP ) {
						EIMComp entComp = CompUtils.getCompById(sess,types[j]);
						compList.add(entComp);						
					}
				}
				if( groupList.size() > 0 ) {
					groupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
					for( Iterator ite = groupList.iterator(); ite.hasNext(); ) {
						EIMGroup grp = (EIMGroup)ite.next();
						out.println("<entry");
						out.println(" entryTypeId=\"2\"");
						out.println(" entryObjectId=\"" + grp.getId() + "\"");
						String entTypeName = " entryTypeName=\"";
						String entObjName = " entryObjName=\"";
						entTypeName = entTypeName + StringUtils.xmlEncode(EIMResource.getMessage(sess,"EIM.ACCESS.TYPE.GROUP")) + "\"";
						entObjName = entObjName + StringUtils.xmlEncode(grp.getName()) + "\"";
						out.println(entTypeName);
						out.println(entObjName);
						out.println(">");
						out.println("</entry>");
					}
				}
				if( roleList.size() > 0 ) {
					roleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
					for( Iterator ite = roleList.iterator(); ite.hasNext(); ) {
						EIMRole role = (EIMRole)ite.next();
						out.println("<entry");
						out.println(" entryTypeId=\"3\"");
						out.println(" entryObjectId=\"" + role.getId() + "\"");
						String entTypeName = " entryTypeName=\"";
						String entObjName = " entryObjName=\"";
						entTypeName = entTypeName + StringUtils.xmlEncode(EIMResource.getMessage(sess,"EIM.ACCESS.TYPE.ROLE")) + "\"";
						entObjName = entObjName + StringUtils.xmlEncode(role.getName()) + "\"";
						out.println(entTypeName);
						out.println(entObjName);
						out.println(">");
						out.println("</entry>");
					}
				}
				if( compList.size() > 0 ) {
					compList = AppObjectUtil.getStrSortedList(compList, "getName", true);
					for( Iterator ite = compList.iterator(); ite.hasNext(); ) {
						EIMComp comp = (EIMComp)ite.next();
						out.println("<entry");
						out.println(" entryTypeId=\"4\"");
						out.println(" entryObjectId=\"" + comp.getId() + "\"");
						String entTypeName = " entryTypeName=\"";
						String entObjName = " entryObjName=\"";
						entTypeName = entTypeName + StringUtils.xmlEncode(EIMResource.getMessage(sess,"EIM.ACCESS.TYPE.COMP")) + "\"";
						entObjName = entObjName + StringUtils.xmlEncode(comp.getName()) + "\"";
						out.println(entTypeName);
						out.println(entObjName);
						out.println(">");
						out.println("</entry>");
					}
				}
				if( userList.size() > 0 ) {
					userList = AppObjectUtil.getStrSortedList(userList, "getName", true);
					for( Iterator ite = userList.iterator(); ite.hasNext(); ) {
						EIMUser eUser = (EIMUser)ite.next();
						out.println("<entry");
						out.println(" entryTypeId=\"1\"");
						out.println(" entryObjectId=\"" + eUser.getId() + "\"");
						String entTypeName = " entryTypeName=\"";
						String entObjName = " entryObjName=\"";
						entTypeName = entTypeName + StringUtils.xmlEncode(EIMResource.getMessage(sess,"EIM.ACCESS.TYPE.USER")) + "\"";
						entObjName = entObjName + StringUtils.xmlEncode(eUser.getName()) + "\"";
						out.println(entTypeName);
						out.println(entObjName);
						out.println(">");
						out.println("</entry>");
					}
				}
			}
		}
		//End Root Node
		out.println("</entryList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
