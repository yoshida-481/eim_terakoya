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

//----------------- Inner Class
	class UnknownEntryUtil
	{
		/**
		 *コンストラクタ
		 */
		 public UnknownEntryUtil()
		{
		}
		
		/**
		 *不明な（削除された）ユーザ用のXMLを形成する
		 * @param sess EIMSessionインスタンス
		 * @param entryTypeId エントリー種別ID(1:ユーザ、2:グループ、3:ロール、4:複合グループ)
		 * @param entryTypeName エントリーの種別名
		 * @param out
		 */
		 public void write(EIMSession sess, String entryTypeId, String entryTypeName, JspWriter out)
		 throws Exception
		 {
			//XML
			out.println("<entry");
				out.println(" entryId=\"" + StringUtils.xmlEncode("-1") + "\"");
				out.println(" entryTypeId=\"" + StringUtils.xmlEncode("-1") + "\"");
				out.println(" entryTypeName=\"" + StringUtils.xmlEncode(entryTypeName) + "\"");
				out.println(" entryObjName=\"" + StringUtils.xmlEncode(EIMResource.getMessage(sess, "EIM.LABEL.WORKSPACE.DELETED.DATA")) + StringUtils.xmlEncode(entryTypeName) + "\"");
			out.println("/>");
		 }
	
	}
//-------------------- end


	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmWsId = request.getParameter("wsId");
	
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
		
		user = (EIMUser)sess.getAttribute("USER");
		
		//WorkSpace
		EIMObject wsObj = null;
		if(prmWsId != null && !prmWsId.equals(""))
		{
			wsObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmWsId));
			if(wsObj == null){
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		//*************「責任者」の取得***************
		//責任者エントリーID配列の取得
		long[] adminEntryIdList = AppObjectUtil.getIntAttrs(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"));
		long[] adminEntryTypeList = AppObjectUtil.getIntAttrs(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"));
		
		//Root Node
		out.println("<entryList>");
		
		if(adminEntryIdList != null)	//属性が存在しない（＝責任者が設定されていない）場合はnull
		{
			
			//削除されたエントリー用Util
			UnknownEntryUtil unknown = new UnknownEntryUtil();
			
			for(int i = 0; i < adminEntryIdList.length; i++)
			{
				long eId = adminEntryIdList[i];
				long typeId = adminEntryTypeList[i];
				
				String entryName = null;
				String entryTypeName = null;
				
				if(typeId == AppConstant.WS_ADMIN_ENTRY_TYPE_USER)
				{
					entryTypeName = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.USER");
					EIMUser tmpUser = UserUtils.getUserById(sess, eId);
					if(tmpUser == null){
						unknown.write(sess, String.valueOf(typeId), entryTypeName, out);
						continue;
					}
					
					entryName = tmpUser.getName();
				}
				else if(typeId == AppConstant.WS_ADMIN_ENTRY_TYPE_GROUP)
				{
					entryTypeName = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.GROUP");
					EIMGroup tmpGroup = GroupUtils.getGroupById(sess, eId);
					if(tmpGroup == null){
						unknown.write(sess, String.valueOf(typeId), entryTypeName, out);
						continue;
					}
					
					entryName = tmpGroup.getName();
				}
				else if(typeId == AppConstant.WS_ADMIN_ENTRY_TYPE_ROLE)
				{
					entryTypeName = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.ROLE");
					EIMRole tmpRole = RoleUtils.getRoleById(sess, eId);
					if(tmpRole == null){
						unknown.write(sess, String.valueOf(typeId), entryTypeName, out);
						continue;
					}
					
					entryName = tmpRole.getName();
				}
				else if(typeId == AppConstant.WS_ADMIN_ENTRY_TYPE_COMP_GROUP)
				{
					entryTypeName = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.COMP");
					EIMComp tmpComp = CompUtils.getCompById(sess, eId);
					if(tmpComp == null){
						unknown.write(sess, String.valueOf(typeId), entryTypeName, out);
						continue;
					}
					
					entryName = tmpComp.getName();
				}else{
					//想定外のものは対処しない
					continue;
				}
				
				//XML
				out.println("<entry");
					out.println(" entryId=\"" + eId + "\"");
					out.println(" entryTypeId=\"" + typeId + "\"");
					out.println(" entryTypeName=\"" + StringUtils.xmlEncode(entryTypeName) + "\"");
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entryName) + "\"");
				out.println(">");
				out.println("</entry>");
			}
		}
		
		
		//End Root Node
		out.println("</entryList>");
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
