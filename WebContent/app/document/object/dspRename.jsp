<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "java.util.*" %>

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
		// Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		user = (EIMUser)sess.getAttribute("USER");
		
		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null) {			
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// 対象オブジェクトのセキュリティのロールチェック
		if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
			// 改名権限がありません。
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NORENAMEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;			
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// ステータスチェック - WF付きフォルダ以下、かつ、編集中以外の場合、改名できない
		if (helper.isUnderFolderWithWorkflow(object) 
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			// 改名権限がありません。
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NORENAMEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;						
		}
		
		/* フォルダ構成管理チェック */		
		// 対象オブジェクトがフォルダの場合
		if (helper.isTypeOfFolder(object.getType())) {
			
			// 下位フォルダ管理セキュリティのロールチェック	
			if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				// 改名権限がありません。
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORENAMEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NORENAMEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		//XML
		out.println("<object");
			
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" rev=\"" + object.getRev() + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");//作成者属性対応対象外:画面に表示されない
			out.println(" createDate=\"" + object.getCreateDate() + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + object.getModifyDate() + "\"");
			
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
			//Status
			if (object.getStatus() != null) {
				out.println(" statusId=\"" + object.getStatus().getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getDefName()) + "\"");
			}
			
			//Security
			if (object.getSecurity() != null) {
				out.println(" securityId=\"" + object.getSecurity().getId() + "\"");
				out.println(" securityName=\"" + StringUtils.xmlEncode(object.getSecurity().getDefName()) + "\"");
			}
			
			//Lock User
			if (object.getLockUser() != null) {
				out.println(" lockUserName=\"" + StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
				out.println(" lockDate=\"" + object.getLockDate() + "\"");
			}
			
			//Property
			String property = "";
			if (object.getAttribute("プロパティ") != null) {
				property = object.getAttribute("プロパティ").getString();
			}
			out.println(" property=\"" + StringUtils.xmlEncode(property) + "\"");
			
			//有効期限
			String expireDate = "";			//有効期限(msec)
			EIMAttribute expirationDate = object.getAttribute("有効期限");
			if (expirationDate != null) {
				expireDate = String.valueOf(DateUtils.convDBTzToCLTzExpirationTime(sess, expirationDate.getDate()));
			}
			out.println(" expireDate=\"" + StringUtils.xmlEncode(expireDate) + "\"");

			
			//Security Check For Read Only Role
			boolean readOnly = false;
			if (object.getSecurity() != null) {
				if (!SecurityUtils.authorized(sess, object,sess.getUser(), EIMAccessRole.UPDATE)) {
					readOnly = true;
				}
			}
			out.println(" readOnly=\"" + readOnly + "\"");

			// スキャン用表紙の名称プレフィックス（紙文書電子化オプション）
			out.println(" objNameCoverPrefix=\"" + EIMConfig.get("PDF_AUTO_REGIST_DOC_NAME_PREFIX") + "\"");

			// document
			if (helper.isTypeOfDocument(object.getType())) {
				// essential attribute
				EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
				EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
				EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
				EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));

				String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
				String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
				EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));

				out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
				out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
				out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");								
			}
			// folder
			else if(helper.isTypeOfFolder(object.getType())){
				// 名称割当て属性チェック				
				long nameAllocate = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_NAME_ATTR"), Integer.MIN_VALUE);
				long objIdNameAllocate = 0;
				int isNameAllocate = 0;
				// 名称割当て属性有りの場合				
				if (nameAllocate != Integer.MIN_VALUE) {
					objIdNameAllocate = nameAllocate;
					// 名称割り当て属性の属性IDを設定
					out.println(" objIdNameAllocate=\"" + objIdNameAllocate + "\"");				
					isNameAllocate = 1;					
				}
				// 名称割当て属性設定
				// この属性の有無で、改名後画面右のワークスペース更新を制御
				// ※フォルダなので画面右を更新（dspRenameObject.mxmlで判定）。
				out.println(" nameAllocate=\"" + isNameAllocate + "\"");				
			}
			
		out.println(">");
		out.println("</object>");
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
