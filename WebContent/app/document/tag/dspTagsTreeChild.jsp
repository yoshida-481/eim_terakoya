<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="common.util.AppConstant"%>
<%@page import="java.util.Iterator"%>
<%@page import="common.util.AppObjectConditionHelper"%>
<%@page import="common.util.TagUtil"%>
<%@page import="common.bo.TagTreeItem"%>
<%@page import="eim.bo.EIMAccessRole"%>
<%@page import="eim.util.SecurityUtils"%>
<%@page import="eim.util.ObjectUtils"%>
<%@page import="eim.bo.EIMObject"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.List"%>
<%@page import="eim.util.StringUtils"%>
<%@page import="common.util.AppMessageUtils"%>
<%@page import="eim.bo.EIMResource"%>
<%@page import="org.apache.commons.logging.LogFactory"%>
<%@page import="org.apache.commons.logging.Log"%>
<%@page import="eim.util.EIMUtils"%>
<%@page import="eim.bo.EIMUser"%>
<%@page import="eim.net.EIMSession"%>

<%
	/* 指定のタグ配下のタグツリー(タグ付与対象一覧)を取得する(複数用) */
	
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// 再帰的にタグツリーのXMLを出力
	class RecurrentUtils
	{
		private EIMSession _sess;
		private AppObjectConditionHelper _helper;
		
		/** コンストラクタ */
		public RecurrentUtils(EIMSession sess)
		{
			_sess = sess;
			_helper = new AppObjectConditionHelper(_sess);
		}
		
		public void write(TagTreeItem item, JspWriter out) throws Exception
		{
			for (Iterator iter = item.getTreeItemList().iterator(); iter.hasNext();) {
				TagTreeItem childItem = (TagTreeItem) iter.next();
				EIMObject childObj = childItem.getEimObject();
				
				// タグの場合
				if (_helper.isTypeOfTag(childObj.getType())) {
				
					out.println("<node");
						out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
						out.println(" objId=\"" + childObj.getId() + "\"");
						out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
						out.println(" objTypeName=\"" + _helper.getObjTypeNameTagXmlEscaped() + "\"");
						out.println(" isWorkflowFolder=\"false\"");
						out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
						out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定 (タグ配下のタグより下は未取得のため)
						out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_TAG_UNDER_TAG + "\"");	// タグ付与対象一覧種別：タグ配下のタグ
					out.println(">");
				
				// フォルダの場合
				} else {
					
					out.println("<node");
						out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
						out.println(" objId=\"" + childObj.getId() + "\"");
						out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
						out.println(" objTypeName=\"" + _helper.getObjTypeNameFolderXmlEscaped() + "\"");
						out.println(" isWorkflowFolder=\"" + _helper.isTypeOfFolderWithWorkflow(childObj) + "\"");
						out.println(" isBranch=\"" + (childItem.getTreeItemList().size() > 0 ? "true" : "false") + "\"");	// ブランチの△設定(子フォルダあり/なし)
						out.println(" isSearch=\"true\"");	// 探索済みフラグをtrueに設定 (タグ配下フォルダの更に配下も取得済みのため)
						out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_TAG_UNDER_FOLDER + "\"");		// タグ付与対象一覧種別：タグ配下のフォルダ
					out.println(">");
					
					// 再起呼び出し
					write(childItem, out);
				}
				out.println("</node>");
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	// Session
	EIMSession sess = null;
	EIMUser user = null;
	
	// Parameter
	String prmTagObjectIds = EIMUtils.getParameter(request, "tagObjectIds");
	
	// Message
	String message = null;
	Object[] paramId = {
			"tagObjectIds=" + prmTagObjectIds
	};
	
	try
	{
		// Session
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
		
		if (prmTagObjectIds == null) {
			out.println("<nodes></nodes>");
			return;
		}
		
		// オブジェクトID配列の作成 (ルートタグから選択対象まで)
		String[] strTagObjIds = null;
		List<Long> tagIdList = new ArrayList<Long>();
		long[] tagObjIds = null;
		if (!StringUtils.isBlank(prmTagObjectIds)) {
			strTagObjIds = prmTagObjectIds.split(",");
			for(int i = 0; i < strTagObjIds.length; i++){
				tagIdList.add(Long.parseLong(strTagObjIds[i]));
			}
		}
		
		// Start Root Node
		out.println("<root>");
		
		// Parameterにて渡されたID分のタグ取得
		for(long tagId : tagIdList)
		{
			// タグの取得
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagId);
			if(tagObj == null || !SecurityUtils.authorized(sess, tagObj, user, EIMAccessRole.READ))
			{
				out.clear();
				// 選択したワークスペース、フォルダ、またはタグは存在しません。\n「最新の情報に更新」を押してください。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			// タグツリーの取得
			TagTreeItem item = TagUtil.getTagTree(sess, tagObj);
			
			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			// タグツリーのソート
			TagUtil.sortTagTree(sess, item, true, helper);
			
			// Start Nodes
			out.print("<nodes objId=\"" + tagObj.getId() + "\"");
			out.println(" label=\"" + StringUtils.xmlEncode(tagObj.getName()) + "\"");
			out.println(">");
			// 再帰呼び出し
			RecurrentUtils ru = new RecurrentUtils(sess);
			ru.write(item, out);
			
			// End Nodes
			out.println("</nodes>");
		}
		// End Root Node
		out.println("</root>");
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
		}catch(Exception se){
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
