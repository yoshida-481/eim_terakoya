<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "app.document.approve.ApproveCommonUtil" %>
<%
	/* ワークフロー表示処理 */
	
	class Util
	{
		public void outEditUsers(EIMSession sess, JspWriter out, EIMObject object, EIMStatusType nextStatusType)
		throws Exception
		{
			if (nextStatusType == null) return;
			EIMStatus nextStatus = WorkFlowUtils.getStatus(sess, object, nextStatusType);
			if (nextStatus == null) return;
			if (nextStatusType.getStep() > object.getStatus().getType().getStep()) return;

			List reqFromList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUEST_FROM"), String.valueOf(nextStatus.getId()));
			for (int i = 0 ; reqFromList.size() > i ; i++) {
				long fromUserId = AppObjectUtil.getIntAttr(sess, (EIMObject)reqFromList.get(i), EIMConfig.get("ATTR_NAME_REQUEST_FROM_USER"), Integer.MIN_VALUE);
				if (fromUserId != Integer.MIN_VALUE) {
					EIMUser fromUser = UserUtils.getUserById(sess, fromUserId);
					if (fromUser != null)
					{
						out.println("<user");
							out.println(" type=\"1\"");
							out.println(" name=\"" + StringUtils.xmlEncode(fromUser.getName()) + "\"");
						out.println(" />");					
					}
				}
			}
		}

		public void outApproveUsers(EIMSession sess, JspWriter out, EIMObject object, EIMStatus status)
		throws Exception
		{
			String statusId = String.valueOf(status.getId());
			
			List userList = new ArrayList();
			List appdList = new ArrayList();

			List objUserList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUEST_TO"), statusId);
			userList = new ArrayList();
			for (int i = 0 ; i < objUserList.size() ; i++)
			{
				userList.add(AppObjectUtil.getStrAttr(sess, (EIMObject)objUserList.get(i), EIMConfig.get("ATTR_NAME_REQUEST_TO_KIND_ID")));
			}
			userList = ApproveCommonUtil.sortCSVList(sess, userList);

			List objAppdList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_APPROVER"), statusId);
			appdList = new ArrayList();
			for (int i = 0 ; i < objAppdList.size() ; i++)
			{
				appdList.add(String.valueOf(AppObjectUtil.getIntAttr(sess, (EIMObject)objAppdList.get(i), EIMConfig.get("ATTR_NAME_APPROVER_USER"), Integer.MIN_VALUE)));
			}

			Map groupMap = new HashMap();
			Map roleMap  = new HashMap();
			
			for (int i = 0 ; i < userList.size() ; i++) {
				int type = Integer.valueOf(String.valueOf(userList.get(i)).substring(0, 1)).intValue();
				long id = Long.valueOf(String.valueOf(userList.get(i)).substring(2)).longValue();
				
				out.println("<user");
				out.println(" type=\"" + String.valueOf(type) + "\"");

				switch (type)
				{
				case 1:
					EIMUser user = UserUtils.getUserById(sess, id);
					out.println(" name=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
					for (int j = 0 ; j < appdList.size() ; j++)
					{
						if (String.valueOf(user.getId()).equals(String.valueOf(appdList.get(j)))) out.println(" approve=\"true\"");
					}
					break;
				case 2:
					EIMGroup group = GroupUtils.getGroupById(sess, id);
					out.println(" name=\"" + StringUtils.xmlEncode(group.getName()) + "\"");
					for (int j = 0 ; j < appdList.size() ; j++)
					{
						List groupList = GroupUtils.getGroupByUser(sess, UserUtils.getUserById(sess, Long.valueOf(String.valueOf(appdList.get(j))).longValue()));
						for (int k = 0 ; k < groupList.size() ; k++)
						{
							if (group.getId() == ((EIMGroup)groupList.get(k)).getId())
							{
								out.println(" approve=\"true\"");
								String gid = String.valueOf(group.getId());
								groupMap.put(gid, gid);
								continue;
							}
						}
					}
					break;
				case 3:
					EIMRole role = RoleUtils.getRoleById(sess, id);
					out.println(" name=\"" + StringUtils.xmlEncode(role.getName()) + "\"");
					for (int j = 0 ; j < appdList.size() ; j++)
					{
						List roleList = RoleUtils.getRoleByUser(sess, UserUtils.getUserById(sess, Long.valueOf(String.valueOf(appdList.get(j))).longValue()));
						for (int k = 0 ; k < roleList.size() ; k++)
						{
							if (role.getId() == ((EIMRole)roleList.get(k)).getId())
							{
								out.println(" approve=\"true\"");
								String rid = String.valueOf(role.getId());
								groupMap.put(rid, rid);
								continue;
							}
						}
					}
					break;
				case 4:
					EIMComp comp = CompUtils.getCompById(sess, id);
					out.println(" name=\"" + StringUtils.xmlEncode(comp.getName()) + "\"");
					for (int j = 0 ; j < appdList.size() ; j++)
					{
						String gid = String.valueOf(comp.getGroup().getId());
						String rid = String.valueOf(comp.getRole().getId());
						
						if (groupMap.get(gid) != null
								&& roleMap.get(rid) != null)
						{
							out.println(" approve=\"true\"");
						}
					}
					break;
				}

				if (status.getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) out.println(" must=\"true\"");

				out.println("/>");
			}
		}
	}

	Util util = new Util();
	
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
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ワークフローなしフォルダの場合
		if (!helper.isTypeOfDocument(object.getType()) && !AppObjectUtil.isWFFolder(sess, object)) 
		{
			//WFなしフォルダ
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// ユーザーが対象に対して公開読取権限しかなく、ステータスが公開済で無い場合
		if (helper.isReadOnlyAccess(object) 
				&& object.getStatus() != null 
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, new Object[]{paramId}));
			return;
		}
				
		// ワークフロー付フォルダ内のドキュメントかどうか
		boolean isDocumentInWFfolder = false;
		
		// Document
		if (helper.isTypeOfDocument(object.getType())) 
		{
			// 上位WFフォルダ属性取得
			long higherWFFolderID = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);

			if (higherWFFolderID != -1)
			{
				// ターゲットを上位WFフォルダに変更
				object = ObjectUtils.getObjectById(sess, higherWFFolderID);
				// 上位WFフォルダが取得できない場合
				if (object == null)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.HIGHRANK.WFFOLDER");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.HIGHRANK.WFFOLDER");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
				else
				{
					isDocumentInWFfolder = true;
				}
			}
		} 		
		
		if(object.getStatus() != null)
		{			
			EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());
			
			//Status Type List
			List statusTypeList = workFlow.getStatusTypeList();
			
			//Root Node
			out.println("<statusTypeList workFlowId=\"" + workFlow.getId() + "\" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");					
			
			// 上位WFフォルダ名称表示
			if(isDocumentInWFfolder)
			{
				out.println(" wfFolderName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			}			
			out.println(">");
			
			for(int i = 0; i < statusTypeList.size(); i++)
			{
				EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);

				out.println("<statusType");
					out.println(" statusTypeId=\"" + statusType.getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
					out.println(" step=\"" + statusType.getStep() + "\"");
				out.println(">");

				if (statusType.getStep() <= object.getStatus().getType().getStep())
				{
					switch ((int)statusType.getKind()) {
					case AppConstant.STATUS_TYPE_KIND_ID_EDITTING:
						util.outEditUsers(sess, out, object, (EIMStatusType)statusTypeList.get(i + 1));
						break;
					case AppConstant.STATUS_TYPE_KIND_ID_APPROVE:
						util.outApproveUsers(sess, out, object, WorkFlowUtils.getStatus(sess, object, statusType));
						break;
					}
				}
						
				out.println("</statusType>");
			}
			
			//End Root Node
			out.println("</statusTypeList>");
		}
		else
		{
			out.println("<statusTypeList></statusTypeList>");
		}
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