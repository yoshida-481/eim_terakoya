<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	// Parameter
	//属性ツリー情報の取得
	AttributeTreeUtil.HttpParamParsed param = AttributeTreeUtil.parseHttpParam(request);

	//Message
	String message = null;
	Object[] paramId = param.getParamIds();

	try
	{
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
		
		if (StringUtils.isBlank(param.getAttrTreeId())) {
			message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.NOATTRTREE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOATTRTREE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		AttributeTree openAttrTree = null;
		try {
			//パラメータのチェックと属性ツリーの取得
			openAttrTree = AttributeTreeUtil.checkAndGetAttrTree(sess, param);
		} catch (EIMException e) {
			if (!AttributeTreeUtil.isErrorOfCheckAndGetAttrTree(e))
				throw e;
			message = e.getMessage();
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage(e.getMessageKey());
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//属性値数のチェック
		List prmAttrTreeValueList = param.getAttrTreeValues();
		if (
				(openAttrTree.isClassifyTargetDocument() && prmAttrTreeValueList.size() >= openAttrTree.getTreeItemList().size())
				|| (openAttrTree.isClassifyTargetFolder() && prmAttrTreeValueList.size() > openAttrTree.getTreeItemList().size())
				){
			message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//XML
		//Root Node
		out.println("<nodes attrTreeId=\"" + param.getAttrTreeId() + "\"");
		for (int i = 0;i < prmAttrTreeValueList.size(); i++) {
			out.print(" attrTreeValue" + (i + 1) + "=\"" + StringUtils.xmlEncode((String)prmAttrTreeValueList.get(i)) + "\"");
		}
		
		//属性ツリー値の型変換
		AttributeTreeUtil.convertAttrTreeValueToDataType(openAttrTree,prmAttrTreeValueList);

		int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM"));	// 最大取得件数
		
		//属性ツリーの末端でフォルダ分類のある場合
		if(prmAttrTreeValueList.size() == openAttrTree.getTreeItemList().size()) {
			//末端の先のフォルダを返却する処理
			// 属性ツリーの末端先のオブジェクトリストの取得。
			List objList = AttributeTreeUtil.getClassifiedObjects(sess,openAttrTree, prmAttrTreeValueList);

			// <最大取得件数>を超過した場合、余分な最後の1件は除去
			if (objList.size() > maxnum) {
				objList.remove(maxnum);
				// 最大取得件数超過ラベルの設定
				String[] maxNumArray = {String.valueOf(maxnum)};
				out.println(" overflowLabel=\"" + EIMResource.getMessage(sess, "EIM.LABEL.ATTRTREE.OVERFLOW", maxNumArray) + "\"");
			}
			out.println(">");
			
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			// 末端先のオブジェクトリスト出力ループ
			for (Iterator j = objList.iterator(); j.hasNext(); ) {
				// EIMObjectに変換する。
				EIMObject obj = (EIMObject)j.next();
	
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(obj.getName()) + "\"");
				out.println(" objId=\"" + obj.getId() + "\"");
				out.println(" objTypeId=\"" + obj.getType().getId() + "\"");
				out.println(" objTypeName=\"" + helper.getObjTypeNameFolderXmlEscaped() + "\"");
				//ワークフロー付きフォルダであるかどうかのフラグ
				out.println(" isWorkflowFolder=\""+helper.isTypeOfFolderWithWorkflow(obj)+"\"");
				out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
				out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
				out.println(">");
				out.println("</node>");
			}
		} else {
			//中間層で、次段の属性値リストを返却する処理
			//属性ツリー項目
			List attrTreeItemValueList = AttributeTreeUtil.classifyAttributeValues(sess, openAttrTree, prmAttrTreeValueList);
			
			// <最大取得件数>を超過した場合、余分な最後の1件は除去
			if (attrTreeItemValueList.size() > maxnum) {
				attrTreeItemValueList.remove(maxnum);
				// 最大取得件数超過ラベルの設定
				String[] maxNumArray = {String.valueOf(maxnum)};
				out.println(" overflowLabel=\"" + EIMResource.getMessage(sess, "EIM.LABEL.ATTRTREE.OVERFLOW", maxNumArray) + "\"");
			}
			out.println(">");
			
			boolean isLeaf = ((prmAttrTreeValueList.size() + 1) == openAttrTree.getTreeItemList().size());
			//ドキュメント分類での末端は、isBranch=false、isSearch=trueとする
			boolean isBranch = openAttrTree.isClassifyTargetFolder()?true:!isLeaf;
			for(Iterator i = attrTreeItemValueList.iterator(); i.hasNext();)
			{
				Object attrItemValue = i.next();
				String label = (attrItemValue == null)
				?""
				:(
					(attrItemValue instanceof Date)
					?DateUtils.getDBTzToCLTzDate(sess, (Date)attrItemValue)
					:String.valueOf(attrItemValue)
				);
				String value = (attrItemValue == null)
				?""
						:(
							(attrItemValue instanceof Date)
							?String.valueOf(((Date)attrItemValue).getTime())
							:String.valueOf(attrItemValue)
						);
				
				//XML
				out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(label) + "\"");
				out.println(" value=\"" + StringUtils.xmlEncode(value) + "\"");
				out.println(" objTypeName=\"属性\"");
				out.println(" isBranch=\"" + isBranch + "\"");
				out.println(" isSearch=\"" + !isBranch + "\"");
				out.println(" isLeaf=\"" + isLeaf + "\"");	// 属性ツリーの末端にあるかどうか
				//ワークフロー付きフォルダであるかどうかのフラグ
				out.println(" isWorkFlowFolder=\"false\"");
				out.println(">");
				out.println("</node>");
			}
		}

		//End Root Node
		out.println("</nodes>");

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