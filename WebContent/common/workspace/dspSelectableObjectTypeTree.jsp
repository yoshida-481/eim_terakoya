<%@page import="app.document.object.FixedForm"%>
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
	
//------------------ Inner Class
	class RecurrentUtils
	{
	   /**
		* コンストラクタ
		*/
		public RecurrentUtils()
		{
		}
		
	   /**
	    * 再帰的にオブジェクトタイプツリーを表示する
	    * 
	    * @param sess EIMSessionインスタンス
	    * @param objType 親オブジェクトタイプ
	    * @param rootObjTypeName ルートのオブジェクトタイプデフォルト名称
	    * @param out
	    * @param form 定型ドキュメント（topは一般XXX）
	    * @param IdList 使用可能タイプのIDのリスト
	    */
		public void write(EIMObjectType objType,
						  String rootObjTypeName,
						  JspWriter out,
						  FixedForm form)
		throws Exception
		{
		
			for (FixedForm child : form.getChildType()) {
			
				if (child.getDspFlag() == true) {
					
				    out.println("<objType");
						out.println(" objTypeId=\"" + child.getFormType().getId() + "\"");
						out.println(" objTypeName=\"" + StringUtils.xmlEncode(child.getFormType().getName()) + "\"");
						out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
						out.println(" isRootType=\"false\"");
						out.println(" selected=\"" + child.getSelectedFlag() + "\"");
					out.println(">");
					
					//Recurrenty
					write(objType, rootObjTypeName, out, child);
					
					out.println("</objType>");
				}
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	// ワークスペースオブジェクトのID
	String prmWsId = EIMUtils.getParameter(request, "wsId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"wsId=" + prmWsId
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
		
		//WorkSpace
		EIMObject wsObj = null;
		if(prmWsId != null && !prmWsId.equals(""))
		{
			wsObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmWsId));
			if(wsObj == null){
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}
		
		//*************「使用可能タイプ」の取得***************
		
		List objTypeList = new ArrayList();
		
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")));	// ドキュメント
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")));	// フォルダ
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG")));	// タグ
			
		//Recurrently
		RecurrentUtils ru = new RecurrentUtils();
		
		
		//Root Node
		out.println("<result>");
		
		for(int i = 0; i < objTypeList.size(); i++)
		{
			String OBJ_TYPE_GENERAL_LABEL = null;
			String NODE_OBJ_TYPE_LIST = null;
			String ATTR_IS_ALL_OBJ_TYPE = null;
			String ATTR_NAME_LIMIT_FLAG = null;
			String ATTR_NAME_SELECTABLE_OBJ_TYPE = null;
			String objectTypeLabelName = null;
			//定型ドキュメント
			FixedForm form = null;
			ArrayList<Long> idList = null;
			
			//Object Type
			EIMObjectType objType = (EIMObjectType)objTypeList.get(i);
			
			//***********ドキュメントの場合*************
			if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")))
			{
				OBJ_TYPE_GENERAL_LABEL = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");				//「一般ドキュメント」
				NODE_OBJ_TYPE_LIST = "documentTypeList";														//タグ名：documentTypeList
				ATTR_IS_ALL_OBJ_TYPE = "isAllDocType";															//属性名：isAllDocType
				ATTR_NAME_LIMIT_FLAG = EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG");			//「使用可能ドキュメントタイプ絞込みフラグ」
				ATTR_NAME_SELECTABLE_OBJ_TYPE = EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE");	//「使用可能ドキュメントタイプ」
				objectTypeLabelName = EIMResource.getMessage(sess, "EIM.LABEL.DOCUMENT.TYPE");					//「ドキュメントタイプ」
				form = AppObjectTypeUtil.getDocumentFixedForm(sess);
			} 
			//**********フォルダの場合******************
			else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")))
			{
				OBJ_TYPE_GENERAL_LABEL = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");	//「一般フォルダ」
				NODE_OBJ_TYPE_LIST = "folderTypeList";															//タグ名：folderTypeList
				ATTR_IS_ALL_OBJ_TYPE = "isAllFolderType";														//属性名：isAllFolderType
				ATTR_NAME_LIMIT_FLAG = EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG");				//「使用可能フォルダタイプ絞込みフラグ」
				ATTR_NAME_SELECTABLE_OBJ_TYPE = EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE");	//「使用可能フォルダタイプ」
				objectTypeLabelName = EIMResource.getMessage(sess, "EIM.LABEL.FOLDER.TYPE");					//「フォルダタイプ」
				form = AppObjectTypeUtil.getFolderFixedForm(sess);
			}
			//***********タグの場合********************
			else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG")))
			{
				OBJ_TYPE_GENERAL_LABEL = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");	//「一般タグ」
				NODE_OBJ_TYPE_LIST = "tagTypeList";															//タグ名：tagTypeList
				ATTR_IS_ALL_OBJ_TYPE = "isAllTagType";														//属性名：isAllTagType
				ATTR_NAME_LIMIT_FLAG = EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG");			//「使用可能タグタイプ絞込みフラグ」
				ATTR_NAME_SELECTABLE_OBJ_TYPE = EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE");	//「使用可能タグタイプ」
				objectTypeLabelName = EIMResource.getMessage(sess, "EIM.LABEL.TAG.TYPE");					//「タグタイプ」
				form = AppObjectTypeUtil.getTagFixedForm(sess);
			}	
			
			boolean isAllType = false;
			boolean dspChildTypeList = true;
			
			long limitFlag = AppObjectUtil.getIntAttr(sess, wsObj, ATTR_NAME_LIMIT_FLAG, Integer.MIN_VALUE);
			if(limitFlag == Integer.MIN_VALUE){
				//属性なし（子の定型タイプは表示させない）
				isAllType = true;
				dspChildTypeList = false;
			}
			else{
				// 絞込みフラグがOFFなら「全て」の定型タイプを使用可能にする
				isAllType = (limitFlag == AppConstant.FLAG_OFF) ? true : false;
				
				//使用可能タイプのIDを取得
				idList = new ArrayList<Long>();
				long[] selectableIds = AppObjectUtil.getIntAttrs(sess, wsObj, ATTR_NAME_SELECTABLE_OBJ_TYPE);
				
				//使用可能タイプが未登録なら子の定型タイプは表示させない
				if(selectableIds == null && isAllType == true){
					dspChildTypeList = false;
				}
				else{
					
					//使用可能タイプが登録済み
					for(long id : selectableIds){
						idList.add(id);
					}
					
					//IDと一致する定型タイプの選択フラグをtrueにし、親子のタイプの表示フラグをtrueにする
					form = AppObjectTypeUtil.chkDspAndSelectedFlag(form, idList);
				}
			}
			
			//XML出力
			out.println("<" + StringUtils.xmlEncode(NODE_OBJ_TYPE_LIST) + " ");
				out.println(ATTR_IS_ALL_OBJ_TYPE + "=\"" + isAllType + "\"");
			out.println(">");
			
			if(dspChildTypeList){
				
				//ルート(「一般XXX」)のオブジェクトタイプデフォルト名称
				String rootObjTypeName = StringUtils.xmlEncode(objType.getDefName());
				//XML
				out.println("<objType");
					out.println(" objTypeId=\"" + objType.getId() + "\"");
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(OBJ_TYPE_GENERAL_LABEL) + "\"");
					out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
					out.println(" isRootType=\"true\"");
				out.println(">");
			
				//Recurrently
				ru.write(objType, rootObjTypeName, out, form);
				
				// 削除されたタイプが存在する場合、XMLを最後に追加
				if(form.getDeletedTypeCount() != -1){
					//XML
					out.println("<objType");
						out.println(" objTypeId=\"" + StringUtils.xmlEncode("-1") + "\"");
						out.println(" objTypeName=\"" + StringUtils.xmlEncode(EIMResource.getMessage(sess, "EIM.LABEL.WORKSPACE.DELETED.DATA")) + StringUtils.xmlEncode(objectTypeLabelName) + "\"");
						out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
						out.println(" isRootType=\"false\"");
						out.println(" selected=\"delete\"");
					out.println(">");
					out.println("</objType>");
				}
				
				out.println("</objType>");
			}
			
			out.println("</" + StringUtils.xmlEncode(NODE_OBJ_TYPE_LIST) + ">");
			
		}
		
		//End Root Node
		out.println("</result>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
