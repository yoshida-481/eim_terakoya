<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "java.util.List"%>
<%@ page import = "java.util.ArrayList"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.AttributeTreeItem"%>
<%@ page import = "common.bo.AttributeTree"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmAttrTreeId = EIMUtils.getParameter(request, "attrTreeId");
	String prmAttrTypeIdList = EIMUtils.getParameter(request, "attrTypeIdList");
	String prmViewNoValuesFlagList = EIMUtils.getParameter(request, "viewNoValuesFlagList");
	String prmOperationList = EIMUtils.getParameter(request, "operationList");
	
	//Message
	String message = null;
	Object[] paramId = {
			"attrTreeId=" + prmAttrTreeId,
			"attTypeIds=" + prmAttrTypeIdList,
			"viewNoValuesFlagList=" + prmViewNoValuesFlagList,
			"operationList=" + prmOperationList
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIUTE_TREE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Attribute Tree
		AttributeTree attributeTree = AttributeTreeUtil.getAttributeTreeById(sess, Long.parseLong(prmAttrTreeId));
		if(attributeTree == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// Attribute Tree Items
		List attTreeItems = new ArrayList();
		
		//属性ツリーIDリスト
		List attrTypeIdList = StringUtils.getStringListFromCSV(prmAttrTypeIdList);
		//属性値なし表示フラグリスト
		List viewNoValuesFlagList =StringUtils.getStringListFromCSV(prmViewNoValuesFlagList);
		//ユーザー操作内容リスト
		List operationList =StringUtils.getStringListFromCSV(prmOperationList);
		
		for (int i=0; i < attrTypeIdList.size(); i++) {
			// attribute type id
			String attTypeId = (String)attrTypeIdList.get(i);
			if (StringUtils.isBlank(attTypeId))
			{
				break;
			}
			
			for (int j=0; j < i; j++) 
			{
				String attTypeIdBefore = (String)attrTypeIdList.get(j);
				if (attTypeId.equals(attTypeIdBefore))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTR.IS.APPLYED");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTR.IS.APPLYED");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					sess.rollback();//例外発生時の処理は、外側のtry-catchに任せる
					return;
				}
			}
		}

		for (int i=0; i < attrTypeIdList.size(); i++) {
			// attribute type id
			String attTypeId = (String)attrTypeIdList.get(i);
			if (StringUtils.isBlank(attTypeId))
			{
				break;
			}
			
			// attribute type
			EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(attTypeId));
			if (attType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				sess.rollback();//例外発生時の処理は、外側のtry-catchに任せる
				return;
			}
			
			// 選択した属性がテキスト型属性の場合
			if (attType.getValueType().getId() == EIMValueType.TEXT){
				//エラーダイアログを表示させる
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTR.IS.NOT.AVAILABLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				//ログ出力
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTR.IS.NOT.AVAILABLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				sess.rollback();//例外発生時の処理は、外側のtry-catchに任せる
				return;
			}

			boolean viewNoValuesFlag = new Boolean((String)viewNoValuesFlagList.get(i)).booleanValue();
			String operation = (String)operationList.get(i);
			
			//Create Operation History
			//作成
			if ("add".equals(operation))
			{
				AttributeTreeUtil.createOperationHistory(sess, 
                    AppConstant.SYSTEM, 
                    EIMConstant.APPLY_ATTRIBUTE_TO_TREE_VIEW,
                    EIMConstant.TARGET_PARENT_ATTRIBUTE_TREE, 
                    EIMConstant.ATTRIBUTE_TREE, 
                    attributeTree,
                    EIMConstant.TARGET_CREATE, 
                    EIMConstant.ATTRIBUTE_TYPE, 
                    attType, 
                    null);
			}
			//移動
			else if("move".equals(operation))
			{
				AttributeTreeUtil.createOperationHistory(sess, 
                    AppConstant.SYSTEM, 
                    EIMConstant.UPDATE_ATTRIBUTE_TREE_ORDER,
                    EIMConstant.TARGET_PARENT_ATTRIBUTE_TREE, 
                    EIMConstant.ATTRIBUTE_TREE, 
                    attributeTree,
                    EIMConstant.TARGET_UPDATE, 
                    EIMConstant.ATTRIBUTE_TYPE, 
                    attType, 
                    String.valueOf(i + 1));
			}
			//削除
			else if("delete".equals(operation))
			{
				AttributeTreeUtil.createOperationHistory(sess, 
                    AppConstant.SYSTEM, 
                    EIMConstant.RELEASE_ATTRIBUTE_FROM_TREE_VIEW,
                    EIMConstant.TARGET_PARENT_ATTRIBUTE_TREE, 
                    EIMConstant.ATTRIBUTE_TREE, 
                    attributeTree,
                    EIMConstant.TARGET_DELETE, 
                    EIMConstant.ATTRIBUTE_TYPE, 
                    attType, 
                    null);
			}
			//更新
			else if("update".equals(operation))
			{
				String detail = "";
				if (viewNoValuesFlag)
				{
					detail = "カテゴリ無し表示をする";
				}
				else
				{
					detail = "カテゴリ無し表示をしない";
				}
				AttributeTreeUtil.createOperationHistory(sess, 
                    AppConstant.SYSTEM, 
                    EIMConstant.UPDATE_TREE_ATTRIBUTE_INFORMATION,
                    EIMConstant.TARGET_PARENT_ATTRIBUTE_TREE, 
                    EIMConstant.ATTRIBUTE_TREE, 
                    attributeTree,
                    EIMConstant.TARGET_UPDATE, 
                    EIMConstant.ATTRIBUTE_TYPE, 
                    attType, 
                    detail);
			}

			
			
			if(!"delete".equals(operation))
			{
				// attribute Tree Item
				AttributeTreeItem attTreeItem = new AttributeTreeItem(-1, attType , viewNoValuesFlag);
				// Attribute Tree Items
				attTreeItems.add(attTreeItem);
			}
		}
		// Set Attribute Type
		AttributeTreeUtil.setTreeItems(sess, attributeTree, attTreeItems);
		
		//Commit
		sess.commit();
		
		//XML
		out.println("<OK></OK>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
