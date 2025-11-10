<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	
//-------------------------------------------------------------------------
//ワークスペース管理機能追加のため、admin/object/dspObjectTypeTree.jspを
//コピーして修正
//														(2012/02/21)
//-------------------------------------------------------------------------

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

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
	    */
		public void write(EIMSession sess, EIMObjectType objType, String rootObjTypeName, JspWriter out)
		throws Exception
		{
			//Child Object Type
			List objTypeList = ObjectUtils.getChildObjectTypeList(sess, objType);
			for(int i = 0; i < objTypeList.size(); i++)
			{
				//Object Type
				EIMObjectType childObjType = (EIMObjectType)objTypeList.get(i);
				
				//XML
				out.println("<objType");
					out.println(" objTypeName=\"" + StringUtils.xmlEncode(childObjType.getName()) + "\"");
					out.println(" objTypeId=\"" + childObjType.getId() + "\"");
					out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
				out.println(">");
				
				//Recurrenty
				write(sess, childObjType, rootObjTypeName, out);
				
				out.println("</objType>");
			}
		}
	}
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Message
	String message = null;
	
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
		
		//RootType
		List objTypeList =  new ArrayList();
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")));	// ドキュメント
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")));	// フォルダ
		objTypeList.add(ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG")));	// タグ
		
		//Recurrently
		RecurrentUtils ru = new RecurrentUtils();
		String label = null;
		
		//Root Node
		out.println("<objTypeList>");
		
		for(int i = 0; i < objTypeList.size(); i++)
		{
			//Object Type
			EIMObjectType objType = (EIMObjectType)objTypeList.get(i);
			
			if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
				label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");					// 一般ドキュメント
			} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
				label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");	// 一般フォルダ
			} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG"))) {
				label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");		// 一般タグ
			}
			
			//ルートのオブジェクトタイプデフォルト名称
			String rootObjTypeName = StringUtils.xmlEncode(objType.getDefName());
			
			//XML
			out.println("<objType");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(label) + "\"");
				out.println(" objTypeId=\"" + objType.getId() + "\"");
				out.println(" rootObjTypeDefName=\"" + StringUtils.xmlEncode(rootObjTypeName) + "\"");
				out.println(" isRootType=\"true\"");
			out.println(">");
			
			//Recurrently
			ru.write(sess, objType, rootObjTypeName, out);
			
			out.println("</objType>");
		}
		
		//End Root Node
		out.println("</objTypeList>");
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
