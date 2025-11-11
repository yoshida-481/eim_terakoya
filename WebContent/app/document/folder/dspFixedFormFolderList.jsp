<%@page import="app.document.search.EIMDocSearchType"%>
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
    
	class RecurrentUtils
	{
		/**
		* コンストラクタ
		*/
		public RecurrentUtils()
		{
		}

		public void write(EIMSession sess, FixedForm form, Map<Long, EIMObject> hasUpdateMap, JspWriter   out)
		throws Exception
		{
			for (FixedForm child : form.getChildType()) {
				
				if ((child.getFormType().getSecurity() == null ||
					hasUpdateMap.containsKey(child.getFormType().getId())) &&
					child.getDspFlag() == true) {
					
					out.println("<objType");
					out.println(" data=\"createFixedFormFolder\"");
					out.println(" objTypeId=\"" + child.getFormType().getId() + "\"");
					out.println(" label=\"" + StringUtils.xmlEncode(child.getFormType().getName()) + "\"");
					out.println(">");

					//Recurrenty
					write(sess, child, hasUpdateMap, out);

					//XML
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
	EIMUser user = null;
	
	//Message
	String message = null;

	try{
		// Parameter
		String prmObjId = request.getParameter("objId");
		String prmIsReturnGeneralFolder = request.getParameter("isReturnGeneralFolder");
		if(prmObjId == null || prmObjId == "")
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		boolean isReturnGeneralFolder = false;
		if (prmIsReturnGeneralFolder != null && prmIsReturnGeneralFolder.equals("true")) {
			isReturnGeneralFolder = true;
		}
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
		
		// 定型フォルダタイプ
		FixedForm form = null;
		
		// 対象のワークスペースオブジェクト取得
		EIMObject obj = ObjectUtils.getObjectById(sess, Long.valueOf(prmObjId));
		if(obj == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL.RELOAD");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}
		
		// すべての定型フォルダタイプ取得
		form = AppObjectTypeUtil.getFolderFixedForm(sess);
		
		// 絞込みフラグ取得
		EIMAttribute flagAttr = obj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"));
		if(flagAttr != null && flagAttr.getInt() == 1)
		{
			// 絞込みフラグの値が1の時、使用可能フォルダタイプ属性の値と一致するフォルダタイプの表示フラグをtrueにする(親、子含む)
			EIMAttribute folderTypeAttr = obj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"));
			if(folderTypeAttr != null){
				ArrayList<Long> idList = new ArrayList<Long>();
				long[] ids = TypeConvertUtils.convertToLongArray(folderTypeAttr.getInts());
				for(long id : ids){
					idList.add(id);
				}
				AppObjectTypeUtil.chkDspFlag(form, idList);
			}
		}
		else
		{
			// 絞込みフラグの値が1以外の時、すべての定型フォルダタイプの表示フラグをtrueにする
			form.setDspFlag(true);
			AppObjectTypeUtil.dspChildFixedForm(form);
		}
		
		// 表示可能なオブジェクトタイプを取得
		List<EIMObjectType> typeList = form.getObjTypeList();
		String[] typesStr = new String[typeList.size()];
		for(int i = 0;i<typeList.size();i++){
			typesStr[i] = String.valueOf(typeList.get(i).getId());
		}
		
		List<EIMObject> dspTypeList = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_OBJECTTYPE,
				EIMAccessRole.UPDATE, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false),
				typesStr);
		
		Map<Long, EIMObject> hasUpdateMap = new HashMap();
		for (EIMObject type : dspTypeList) {
			hasUpdateMap.put(Long.valueOf(type.getName()), type);
		}
		
		// XML出力
		
		// Start Root Node
		out.println("<objTypeList>");
		
		// 一般フォルダタイプL
		if (isReturnGeneralFolder) {
			EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "フォルダ");

			out.println("<objTypeGeneralFolder");
			out.println(" data=\"generalFolder\"");
			out.println(" objTypeId=\"" + objType.getId() + "\"");
			out.println(" label=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
			out.println(">");
			out.println("</objTypeGeneralFolder>");
		}

		RecurrentUtils ru = new RecurrentUtils();
		//Recurrently
		ru.write(sess, form, hasUpdateMap, out);

		//End Root Node
		out.println("</objTypeList>");
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
