<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
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
	String prmStatusTypeId = request.getParameter("statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"statusTypeId=" + prmStatusTypeId
			};

	try{
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
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

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
			}
		}

		//Status Type
		EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
		EIMStatus status = WorkFlowUtils.getStatus(sess, object, statusType);

		//Attribute Types
		List attTypeList = StatusAttributeUtils.getAttributeTypeList(sess, statusType);

		out.println("<items>");

		out.println("<attList>");

		for(int i = 0; i < attTypeList.size(); i++)
		{
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
			String value = "";
			if(status != null && object.getStatus().getType().getStep() >= statusType.getStep())
			{
				StatusAttributeUtils.load(sess, status);
				EIMAttribute att = status.getAttribute(attType.getName());
				if(att != null)
				{
					if(attType.getValueType().getId() == EIMValueType.INTEGER)
					{
						value = String.valueOf(att.getInt());
					}
					else if(attType.getValueType().getId() == EIMValueType.STRING)
					{
						value = att.getString();
					}
					else if(attType.getValueType().getId() == EIMValueType.DATE)
					{
						value = String.valueOf(att.getDate());
						//value = value.replace('-', '/');
					} else if(attType.getValueType().getId() == EIMValueType.DOUBLE)
					{
						value = FormatUtil.getDoubleFormatedString(att.getDouble());
					}
				}
			}

			//XML Out
			out.println("<attribute");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" attValue=\"" + StringUtils.xmlEncode(value) + "\"");
				out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
			out.println("/>");
		}

		out.println("</attList>");

		if (statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
		{
			List sendBackObjList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_SEND_BACK"), String.valueOf(status.getId()));
			out.println("<sendBackCommentList>");
			for (int i = 0 ; i < sendBackObjList.size() ; i++)
			{
				String strText = AppObjectUtil.getTextAttr(sess, (EIMObject)sendBackObjList.get(i), EIMConfig.get("ATTR_NAME_SEND_BACK_COMMENT"));
				if (strText == null) strText = "";
				long userId = AppObjectUtil.getIntAttr(sess, (EIMObject)sendBackObjList.get(i), EIMConfig.get("ATTR_NAME_SEND_BACK_USER"), Integer.MIN_VALUE);
				out.println("<comment ");
					out.println(" user=\"" + StringUtils.xmlEncode(UserUtils.getUserById(sess, userId).getName()) + "\"");
					out.println(" text=\"" + StringUtils.xmlEncode(strText) + "\"");
				out.println("/>");
			}
			out.println("</sendBackCommentList>");
		}

		if (statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE)
		{
			if (status == null)
			{
				out.println("<approveDate date=\"\"/>");
			}
			else
			{
				List requestObjList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUEST_FROM"), String.valueOf(status.getId()));
				List approverObjList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_APPROVER"), String.valueOf(status.getId()));

				if (object.getStatus().getType().getStep() >= statusType.getStep())
				{
					Date approveDate = null;
					for (int i = 0 ; i < approverObjList.size() ; i++)
					{
						Date tempDate = AppObjectUtil.getDateAttr(sess, (EIMObject)approverObjList.get(i), EIMConfig.get("ATTR_NAME_APPROVER_APPROVE_DATE"));
						if (approveDate == null)
							approveDate = tempDate;
						else if (approveDate.before(tempDate))
							approveDate = tempDate;
					}

					if (approveDate != null)
					{
						String cltApproveDate = DateUtils.getDBTzToCLTzDate(sess, approveDate);
						out.println("<approveDate date=\"" + cltApproveDate + "\"/>");
					}
					else
					{
						out.println("<approveDate date=\"\"/>");
					}

					out.println("<requestCommentList>");
					for (int i = 0 ; i < requestObjList.size() ; i++)
					{
						String strText = AppObjectUtil.getTextAttr(sess, (EIMObject)requestObjList.get(i), EIMConfig.get("ATTR_NAME_REQUEST_FROM_COMMENT"));
						if (strText == null) strText = "";
						long userId = AppObjectUtil.getIntAttr(sess, (EIMObject)requestObjList.get(i), EIMConfig.get("ATTR_NAME_REQUEST_FROM_USER"), Integer.MIN_VALUE);
						out.println("<comment ");
							out.println(" user=\"" + StringUtils.xmlEncode(UserUtils.getUserById(sess, userId).getName()) + "\"");
							out.println(" text=\"" + StringUtils.xmlEncode(strText) + "\"");
						out.println("/>");
					}
					out.println("</requestCommentList>");

					out.println("<approveCommentList>");
					for (int i = 0 ; i < approverObjList.size() ; i++)
					{
						String strText = AppObjectUtil.getTextAttr(sess, (EIMObject)approverObjList.get(i), EIMConfig.get("ATTR_NAME_APPROVER_COMMENT"));
						if (strText == null) strText = "";
						long userId = AppObjectUtil.getIntAttr(sess, (EIMObject)approverObjList.get(i), EIMConfig.get("ATTR_NAME_APPROVER_USER"), Integer.MIN_VALUE);
						out.println("<comment ");
							out.println(" user=\"" + StringUtils.xmlEncode(UserUtils.getUserById(sess, userId).getName()) + "\"");
							out.println(" text=\"" + StringUtils.xmlEncode(strText) + "\"");
						out.println("/>");
					}
					out.println("</approveCommentList>");
				}
				else
				{
					out.println("<approveDate date=\"\"/>");
				}
			}
		}

		out.println("</items>");
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
