<%@page import="app.document.search.EIMDocSearchType"%>
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

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser user = null;
	
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
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object Type Favorite
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "お気に入り");
		
		//Search
		List<EIMObject> result = AppSearchUtils.searchObject(	sess,
												objType,
												null,
												false,
												true,
												-1,
												user,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												EIMAccessRole.READ);
		
		// 取得できたお気に入りオブジェクトから表示させるオブジェクトを絞り込む
		int idx = 0;
		long[] favoriteObjs = new long[result.size()];
		for (EIMObject favoriteObj : result) {
			favoriteObjs[idx++] = Long.parseLong(favoriteObj.getName());
		}
		List<EIMObject> dspObjList = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_FAVORITEITEM,
				EIMAccessRole.READ, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false), favoriteObjs);
		// 表示対象となるオブジェクト一覧
		Map<Long, EIMObject> dspObjMap = new HashMap<Long, EIMObject>();
		for (EIMObject dspObj : dspObjList) {
			dspObjMap.put((long)dspObj.getId(), dspObj);
		}
		
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		//Root Node
		out.println("<objList>");
		
		//Document
		for(int i = 0; i < result.size(); i++)
		{
			//Object
			EIMObject object = (EIMObject)result.get(i);
			//Favorite Folder
			EIMObject favoriteObject = ObjectUtils.getObjectById(sess, Long.parseLong(object.getName()));
			if(favoriteObject == null || !SecurityUtils.authorized(sess, favoriteObject, user, EIMAccessRole.READ))
			{
				if(SecurityUtils.authorized(sess, object, user, EIMAccessRole.DELETE)) 
				{
					ObjectUtils.deleteObject(sess, object);
					//XML
					message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.DELETEFAVORITE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.DELETEFAVORITE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, 
							new Object[]{String.valueOf(object.getId())}));
				}
				continue;
			}
			
			//Result
			boolean security = true;
			
			//登録されているフォルダの実体が存在するかのチェック
			if(favoriteObject != null)
			{
				// 表示対象か
				if (!dspObjMap.containsKey((long)favoriteObject.getId())) {
					continue;
				}
				
				//Check
				if(favoriteObject.getSecurity() != null)
				{
					security = helper.checkAccessibleStatusSelf(favoriteObject,false);
					if(security == true)
					{
						//XML
						out.println("<object");
							out.println(" objId=\"" + favoriteObject.getId() + "\"");
							out.println(" objTypeId=\"" + favoriteObject.getType().getId() + "\"");
							out.println(" objTypeName=\"" + StringUtils.xmlEncode(favoriteObject.getType().getName()) + "\"");
							out.println(" objName=\"" + StringUtils.xmlEncode(favoriteObject.getName()) + "\"");
							
							//Path
							String path = StringUtils.nullToBlank(AppObjectUtil.getPath(favoriteObject));
							out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
							out.println(" isWorkflowFolder=\""+helper.isTypeOfFolderWithWorkflow(favoriteObject)+"\"");
							
							//気に入りobjId
							out.println(" fvrtObjId=\"" + object.getId() + "\"");
							
						out.println(">");
						out.println("</object>");
					}
				}
			} else {
				if(SecurityUtils.authorized(sess, object, user, EIMAccessRole.DELETE)) 
				{
					//お気に入り削除
					ObjectUtils.deleteObject(sess, object);
				
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DELETEFAVORITE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.DELETEFAVORITE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, new Object[]{String.valueOf(object.getId())}));
				}
			}
		}
		
		//Commit
		sess.commit();

		//End Root Node
		out.println("</objList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
