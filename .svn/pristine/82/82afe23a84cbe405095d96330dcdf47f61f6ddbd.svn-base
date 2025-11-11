<%@ page import="common.bo.AttributeTree"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import="app.document.search.*"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	/* 指定フォルダの配下1レベルまでのツリーを取得 */
	
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	// Session
	EIMSession sess = null;
	EIMUser user = null;
	
	// Parameter
	String prmObjId = EIMUtils.getParameter(request, "ObjectId");
	//属性ツリー情報の取得
	AttributeTreeUtil.HttpParamParsed attrTreeParam = AttributeTreeUtil.parseHttpParam(request);

	// Message
	String message = null;
	Object[] paramId = new Object[1 + attrTreeParam.getParamIds().length];
	paramId[0] = "objId=" + prmObjId;
	System.arraycopy(attrTreeParam.getParamIds(), 0, paramId, 1, attrTreeParam.getParamIds().length);
	
	try {
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		if (prmObjId == null) {
			out.println("<nodes></nodes>");
			return;
		}
		

		// 選択されたフォルダ
		EIMObject selectObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(selectObj == null || !SecurityUtils.authorized(sess, selectObj, sess.getUser(),EIMAccessRole.READ)){
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		AttributeTree attrTree = null;
		try {
			//パラメータのチェックと属性ツリーの取得
			attrTree = AttributeTreeUtil.checkAndGetAttrTree(sess, attrTreeParam);
		} catch (EIMException e) {
			if (!AttributeTreeUtil.isErrorOfCheckAndGetAttrTree(e))
				throw e;
			message = e.getMessage();
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage(e.getMessageKey());
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		//属性ツリーの下位にいるかをチェック == attrTreeValuesの内容と一致するかをチェック
		if (attrTree != null) {
			boolean isValid = false;
attrTreeValid:			do {
				//属性ツリーの末端の直下のフォルダを取得する
				List attrTreeValues = new ArrayList(attrTreeParam.getAttrTreeValues().subList(0,attrTree.getTreeItemList().size()));
				AttributeTreeUtil.convertAttrTreeValueToDataType(attrTree, attrTreeValues);
				List objs = AttributeTreeUtil.getClassifiedObjects(sess, attrTree, attrTreeValues);
				//結果なしは即NG
				if (objs.size() == 0)
					break;
				//今検索した属性ツリーの末端の直下のフォルダと、attrTreeValuesの内容を比較する
				long folderObjId = Long.parseLong((String)attrTreeParam.getAttrTreeValues().get(attrTree.getTreeItemList().size()));
				EIMObject parentObj = null;
				for (Iterator i = objs.iterator(); i.hasNext();) {
					EIMObject leafObj = (EIMObject)i.next();
					if (leafObj.getId() == folderObjId) {
						parentObj = leafObj;
						break;
					}
				}
				if (parentObj == null)
					break attrTreeValid;//breakでもよいが、分かりやすく
				
				//attrTreeValuesのフォルダツリー部を下位リレーションをたどって検証する
				List folderTreeValues = attrTreeParam.getAttrTreeValues().subList(attrTree.getTreeItemList().size(),attrTreeParam.getAttrTreeValues().size());
				for (int i = 1;i < folderTreeValues.size();i++) {
					EIMObject childObj = ObjectUtils.getObjectById(sess, Long.parseLong((String)folderTreeValues.get(i)));
					if (childObj == null
							|| RelationUtils.getRelationByParentAndChild(sess, helper.getRelationTypeOfDocument(), parentObj, childObj) == null
							)
						break attrTreeValid;
					parentObj = childObj;
				}
				
				isValid = true;
			} while (false);
			if (!isValid) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		// 子タグのリスト (タグはフォルダより上に表示する)
		List childTagList = new ArrayList();
		// 属性ツリーではタグは表示しない
		if (attrTree == null) {
			childTagList = AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.DISPLAY_CHILDTAG, EIMAccessRole.READ, null, selectObj);
		}
		
		// 子フォルダのリスト
		List childObjList = AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.DISPLAY_CHILDFOLDER, EIMAccessRole.READ, null, selectObj);
		

		// Start Root Node
		out.print("<nodes objId=\"" + prmObjId + "\"");
		out.println(" label=\"" + StringUtils.xmlEncode(selectObj.getName()) + "\"");

		// Root Nodeの続き。属性情報を折り返す
		if (attrTreeParam.getAttrTreeId() != null)
			out.print(" attrTreeId=\"" + attrTreeParam.getAttrTreeId() + "\"");
		for (int i = 0;i < attrTreeParam.getAttrTreeValues().size(); i++) {
			out.print(" attrTreeValue" + (i + 1) + "=\"" + StringUtils.xmlEncode((String)attrTreeParam.getAttrTreeValues().get(i)) + "\"");
		}
		out.println(">");


		// 子タグ出力ループ
		for (Iterator i = childTagList.iterator(); i.hasNext(); ) {
			// 子オブジェクト取得
			EIMObject childObj = ((EIMRelation)i.next()).getChild();

			out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
				out.println(" objId=\"" + childObj.getId() + "\"");
				out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + helper.getObjTypeNameTagXmlEscaped() + "\"");
				out.println(" isWorkflowFolder=\"false\"");
				out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
				out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
				out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_ROOT_TAG + "\"");		// タグ付与対象一覧種別：ルートタグ
			out.println("/>");
		}
		// 子オブジェクト出力ループ
		for (Iterator i = childObjList.iterator(); i.hasNext(); ) {
			// 子オブジェクト取得
			EIMObject childObj = ((EIMRelation)i.next()).getChild();

			if (childObj != null && childObj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
				out.println(" objId=\"" + childObj.getId() + "\"");
				out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + helper.getObjTypeNameFolderXmlEscaped() + "\"");
				out.println(" isWorkflowFolder=\""+helper.isTypeOfFolderWithWorkflow(childObj)+"\"");
				out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
				out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
				out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				out.println("/>");
			} else if (childObj != null && childObj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))) {
				// 英語表記への対応のため「ごみ箱」オブジェクトタイプからオブジェクト名称を取得
				EIMObjectType objTypeRecycle = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(objTypeRecycle.getName()) + "\"");
				out.println(" objId=\"" + childObj.getId() + "\"");
				out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + helper.getObjTypeNameWorkspaceRecycleXmlEscaped() + "\"");
				out.println(" isWorkflowFolder=\"false\"");
				out.println(" isBranch=\"false\"");
				out.println(" isSearch=\"true\"");
				out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				out.println("/>");
			}
		}
		// End Root Node
		out.println("</nodes>");
		
	} 	catch(EIMException eime)
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