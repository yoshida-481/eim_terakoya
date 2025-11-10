<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="app.document.search.*"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	/* フォルダツリー(初期表示) */

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class RecurrentUtils
	{
		private EIMSession _sess;
		private AppObjectConditionHelper _helper;
		/** コンストラクタ */
		public RecurrentUtils(EIMSession sess) {
			_sess = sess;
			_helper = new AppObjectConditionHelper(_sess);
		}

		/**
		 * 再帰的にフォルダツリーを取得します。
		 *
		 * @param object 親フォルダオブジェクト
		 * @param out
		 * @param searchLevel 取得する階層数
		 */
		public void write(	EIMObject		object,
							JspWriter		out,
							int 			searchLevel
							)
		throws Exception
		{
			// 取得する階層数が0以下なら以降探索しない
			if (searchLevel <= 0) {
				return;
			}
			//Child Tags (タグはフォルダより上に表示する)
			List childTagList = AppSearchUtils.searchRelationByConditionMaker(_sess, EIMDocSearchType.DISPLAY_CHILDTAG, EIMAccessRole.READ, null, object);
			for(Iterator i = childTagList.iterator(); i.hasNext(); )
			{
				//Child Object
				EIMObject childObj = ((EIMRelation)i.next()).getChild();

				//XML
				out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
					out.println(" objId=\"" + childObj.getId() + "\"");
					out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + _helper.getObjTypeNameTagXmlEscaped() + "\"");
					out.println(" isWorkflowFolder=\"false\"");
					out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
					out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_ROOT_TAG + "\"");		// タグ付与対象一覧種別：ルートタグ
				out.println(">");

				//Recurrenty
				//※ タグが付与されたオブジェクトをここで再帰的に取得することはしない。
				out.println("</node>");
			}
			//Child Folders
			List childObjList = AppSearchUtils.searchRelationByConditionMaker(_sess, EIMDocSearchType.DISPLAY_CHILDFOLDER, EIMAccessRole.READ, null, object);
			for(Iterator i = childObjList.iterator(); i.hasNext(); )
			{
				//Child Object
				EIMObject childObj = ((EIMRelation)i.next()).getChild();

				//XML
				out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(childObj.getName()) + "\"");
					out.println(" objId=\"" + childObj.getId() + "\"");
					out.println(" objTypeId=\"" + childObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + _helper.getObjTypeNameFolderXmlEscaped() + "\"");
					out.println(" isWorkflowFolder=\""+_helper.isTypeOfFolderWithWorkflow(childObj)+"\"");
					out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
					out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				out.println(">");

				//Recurrenty
				//ワークスペース配下のフォルダツリーは指定の階層までしか取得しない
				write(childObj, out, searchLevel - 1);

				out.println("</node>");
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Message
	String message = null;

	// Parameter
	String prmIsReturnFirstWSFolder = request.getParameter("isReturnFirstWSFolder");
	boolean isReturnFirstWSFolder = false;
	if (prmIsReturnFirstWSFolder == null || prmIsReturnFirstWSFolder.equals("true")) {
		isReturnFirstWSFolder = true;
	}

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

		//Object Type Recycle
		EIMObjectType objTypeRecycle = ObjectUtils.getObjectTypeByName(sess, "ごみ箱");

		//Search Work Space
		List<EIMObject> targetObjectList = null; // オブジェクトを絞り込む場合に指定する（ワークスペースにのみ有効）
		String targetObjectIdCSV = request.getParameter("targetRootObjectIds");
		if (targetObjectIdCSV != null && targetObjectIdCSV.length() > 0) {

			targetObjectList = new ArrayList<>();
			String[] idsStr = targetObjectIdCSV.split(",");
			for (String idStr : idsStr) {
				EIMObject targetObject = new EIMObject(Long.parseLong(idStr), null,
						null, -1,
						false, null, null, null, null,
						null, null, false, false, null);
				targetObjectList.add(targetObject);
			}
		}
		List<EIMObject> result = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_WORKSPACE,
				EIMAccessRole.READ, null, targetObjectList);

		//Root Node
		out.println("<nodes>");

		for(int i = 0; i < result.size(); i++)
		{
			//Object
			EIMObject object = (EIMObject)result.get(i);

			//XML
			out.println("<node");
				out.println(" label=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" objTypeId=\"" + object.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
				out.println(" isWorkflowFolder=\"false\"");
				out.println(" isWorkSpace=\"true\"");
				if (i == 0) {
														// 一番上のワークスペースのみブランチの有無を指定しない
					out.println(" isSearch=\"true\"");	// 一番上のワークスペースのみ探索済みフラグをtrueに設定
				} else {
					out.println(" isBranch=\"true\"");	// ブランチに全て△(子フォルダあり)を設定
					out.println(" isSearch=\"false\"");	// 探索済みフラグをfalseに設定
				}
				out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
				if (object.getSecurity() != null) {
					out.println(" secName=\"" + object.getSecurity().getDefName() + "\"");
				} else {
					out.println(" secName=\"\"");
				}
			out.println(">");

			//Recurrently
			RecurrentUtils ru = new RecurrentUtils(sess);

			//一番上のワークスペースだけ1階層下まで取得、それ以外は取得しない
			if (i == 0) {
				// 一番上のワークスペース配下のフォルダを返却させる場合
				if (isReturnFirstWSFolder) {
					ru.write(object, out, 1);
				}
			}

			//End XML
			out.println("</node>");
		}

		//ごみ箱（systemセキュリティに権限のあるユーザにのみ表示する）
		if (AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.READ))
		{
			EIMObject recycleObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeRecycle, "ごみ箱");
			if (recycleObj != null)
			{
				out.println("<node");
					out.println(" label=\"" + StringUtils.xmlEncode(objTypeRecycle.getName()) + "\"");
					out.println(" objId=\"" + recycleObj.getId() + "\"");
					out.println(" objTypeId=\"" + recycleObj.getType().getId() + "\"");
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(recycleObj.getType().getDefName()) + "\"");
					out.println(" isWorkflowFolder=\"false\"");
					out.println(" isBranch=\"false\"");	// ごみ箱フォルダはツリー展開しない
					out.println(" isSearch=\"true\"");	// ごみ箱フォルダは探索しない
					out.println(" tagListKind=\"" + AppConstant.TAG_LIST_KIND_NOT_TAG_LIST + "\"");		// タグ付与対象一覧種別：タグ付与対象一覧以外
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
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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