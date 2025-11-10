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
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
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
		//Object
		EIMObject target = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(target == null || !SecurityUtils.authorized(sess, target, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//Check Object Type
		if(target.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//Version
		EIMVersion version = VersionUtils.getVersion(sess, target);

		// 公開ドキュメントのフォーマット
		EIMFormat formatPDF = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));

		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));
		
		//Root Node
		out.println("<objList>");
		
		//Object List
		List objList = version.getList();
		for(int i = objList.size() - 1; i >= 0; i--)
		{
			//Object
			EIMObject object = (EIMObject)objList.get(i);

			String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
			String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
			EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
			
			//XML
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" objTypeId=\"" + object.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
				out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
				out.println(" rev=\"" + object.getRev() + "\"");
				out.println(" latest=\"" + object.getLatest() + "\"");
				out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");//作成者属性対応対象外:画面に表示されない
				out.println(" createDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate()) + "\"");
				out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
				out.println(" modifyDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
				
				if(object.getStatus() != null)
				{
					out.println(" statusId=\"" + object.getStatus().getId() + "\"");
					out.println(" statusTypeId=\"" + object.getStatus().getType().getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getDefName()) + "\"");
					out.println(" statusTypeKind=\"" + object.getStatus().getType().getKind() + "\"");
					out.println(" isDspPubIconForNoWF=\"false\"");		// WFなしドキュメントの公開アイコン表示フラグ
				}
				else
				{
					// WFなしドキュメントの公開アイコン表示フラグ
					out.println(" isDspPubIconForNoWF=\"true\"");
				}
				
				//改訂内容
				String updateComment = "";
				if(object.getAttribute("改訂内容") != null)
				{
					updateComment = object.getAttribute("改訂内容").getString();
				}
				out.println(" updateComment=\"" + StringUtils.xmlEncode(updateComment) + "\"");
				
				//PDFアイコンの表示判定
				out.println(" isDspPdfIcon=\"" + AppLogicUtil.isDspPdfIcon(sess, object, formatPDF) + "\"");
				
				/*
				 * 有効期限切れ判定
				 */
				boolean expiration = false;
				EIMAttribute expirationDate = object.getAttribute("有効期限");
				if(expirationDate != null)
				{
					expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
				}
				out.println(" expiration=\"" + expiration + "\"");
				
				//Security Check For Read Only Role
				
				boolean readOnly = false;
				
				// 条件判定ヘルパー作成
				AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
				
				// ユーザーが対象ドキュメントに対して公開読取権限しかない場合
				// ファイルリンクをグレーアウト
				if (helper.isReadOnlyAccess(object)) 
				{
					readOnly = true;
				}

				out.println(" readOnly=\"" + readOnly + "\"");

				out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
				out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
				out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");
				
			out.println(">");
			out.println("</object>");
		}
		
		//End Root Node
		out.println("</objList>");
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
