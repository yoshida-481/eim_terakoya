<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

<%
//-------------------------------------------------------------------------
//ワークスペース管理機能追加のため、app/object/dspProperty.jspをコピーして
//追加修正
//※ワークスペースの属性情報取得の際に呼び出される
//														(2012/02/17)
//-------------------------------------------------------------------------


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
	String prmObjTypeId = request.getParameter("objTypeId");
	String prmObjTypeDefName = request.getParameter("objTypeDefName");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"objTypeId" + prmObjTypeId,
			"objTypeDefName" + prmObjTypeDefName
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
		
		if (prmObjId == null || prmObjId.length() < 1) {
			
			//新規作成関連の画面から遷移した場合
			long objTypeId = Integer.MIN_VALUE;

			if (prmObjTypeId == null || prmObjTypeId.length() < 1) {

				//新規ワークスペースまたは、新規フォルダ（一般フォルダ）から遷移した場合
				EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get(prmObjTypeDefName));
				objTypeId = objType.getId();

			}
			else {

				objTypeId = Long.parseLong(prmObjTypeId);

			}

			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, objTypeId);

			//ドキュメントかどうか判定
			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			// フォルダ、またはワークスペースの場合のみ下位引継ぎ処理を実施 (タグの場合は引き継がない)
			boolean isDocument = false;
			boolean isWorkSpace = false;
			boolean isTag =false;
			
			if (helper.isTypeOfDocument(objType)) {
				isDocument = true;
			} else if (helper.isTypeOfTag(objType)) {
				isTag = true;
			} else if (helper.isTypeOfWorkspace(objType)) {
				isWorkSpace = true;
			}

			// 表示用のオブジェクトタイプを設定する。
			String label = objType.getName();
			if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
				label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");					// 一般ドキュメント
			} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
				label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");	// 一般フォルダ
			} else if (objType.getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG"))) {
				label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");		// 一般タグ
			}
			//XML
			out.println("<object");
				
				out.println(" objTypeId=\"" + objType.getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getDefName()) + "\"");
				out.println(" dspObjTypeName=\"" + label + "\"");
				out.println(" isDocument=\"" + isDocument + "\"");
				out.println(" isWorkSpace=\"" + isWorkSpace + "\"");
				out.println(" isTag=\"" + isTag + "\"");

				//Security Check For Read Only Role
				out.println(" readOnly=\"" + "false" + "\"");
				
			out.println(">");
			out.println("</object>");
			return;
			
		}
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//ドキュメントかどうか判定
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// フォルダ、またはワークスペースの場合のみ下位引継ぎ処理を実施 (タグの場合は引き継がない)
		boolean isDocument = false;
		boolean isWorkSpace = false;
		boolean isTag =false;
		
		if (helper.isTypeOfDocument(object.getType())) {
			isDocument = true;
		} else if (helper.isTypeOfTag(object.getType())) {
			isTag = true;
		} else if (helper.isTypeOfWorkspace(object.getType())) {
			isWorkSpace = true;
		}
		
		// 表示用のオブジェクトタイプを設定する。
		String label = object.getType().getName();
		if (object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
			label = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");					// 一般ドキュメント
		} else if (object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {
			label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");	// 一般フォルダ
		} else if (object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG"))) {
			label = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");		// 一般タグ
		}

		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));

		String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
		String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
		EIMFile file = null;
		long fSize = 0;
		
		//XML
		out.println("<object");
			
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" dspObjTypeName=\"" + label + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" rev=\"" + object.getRev() + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			String createUserName = "";
			if (isDocument || isTag)
			{				
				if (object.getAttribute("作成者") != null)
				{
					createUserName = UserUtils.getUserById(sess, object.getAttribute("作成者").getInt()).getName();
				}
			}
			else 
			{
				createUserName = object.getCreateUser().getName();
			}
			out.println(" createUserName=\"" + StringUtils.xmlEncode(createUserName) + "\"");
			out.println(" createDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate()) + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
			if (isDocument) {
				file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
				if( file != null )
					fSize = file.getSize();
				out.println(" fileSize=\"" + fSize + "\"");
			}
			out.println(" url=\"" + StringUtils.xmlEncode(EIMConfig.get("DOCUMENT_URL") + EIMConfig.get("QUERY_STRING")) + "objId=" + object.getId() + "\"");
			out.println(" isDocument=\"" + isDocument + "\"");
			out.println(" isWorkSpace=\"" + isWorkSpace + "\"");
			out.println(" isTag=\"" + isTag + "\"");

			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
			//Status
			if(object.getStatus() != null)
			{
				out.println(" statusId=\"" + object.getStatus().getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getName()) + "\"");
			}
			
			//Security
			if(object.getSecurity() != null)
			{
				out.println(" securityId=\"" + object.getSecurity().getId() + "\"");
				out.println(" securityName=\"" + StringUtils.xmlEncode(object.getSecurity().getName()) + "\"");
			}
			
			//Lock User
			if(object.getLockUser() != null)
			{
				out.println(" lockUserName=\"" + StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
				out.println(" lockDate=\"" + object.getLockDate() + "\"");
			}
			
			//Property
			String property = "";
			if(object.getAttribute("プロパティ") != null)
			{
				property = object.getAttribute("プロパティ").getString();
			}
			out.println(" property=\"" + StringUtils.xmlEncode(property) + "\"");
			
			/*
			 * 有効期限切れ判定
			 */
			String expireDate = "";			//有効期限(msec)
			boolean expiration = false;	//有効期限切れフラグ
			EIMAttribute expirationDate = object.getAttribute("有効期限");
			if(expirationDate != null)
			{
				expireDate = String.valueOf(DateUtils.convDBTzToCLTzExpirationTime(sess, expirationDate.getDate()));
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
			}

			out.println(" expireDate=\"" + StringUtils.xmlEncode(expireDate) + "\"");
			out.println(" expiration=\"" + expiration + "\"");
			
			//署名・暗号化バージョン
			String signencrVer = AppObjectUtil.getStrAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
			if (signencrVer != null)
			{
				out.println(" signencrVer=\"" + signencrVer + "\"");
			}

			// タグ
			long[] tags = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
			if( tags != null ) {
				String tagStr = "";
				for( int i = 0; i < tags.length; i++ ) {
					tagStr += String.valueOf(tags[i]);
					if( (i + 1) < tags.length ) {
						tagStr += ",";
					}
				}
				out.println(" tags=\"" + tagStr + "\"");
			}

			//Security Check For Read Only Role
			boolean readOnly = false;

			//アクセス権限チェック
			if (helper.isTypeOfFolder(object.getType())) {

				// ドキュメントのリレーションタイプ取得
				EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

	            // 親リレーションの取得
				List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType,EIMAccessRole.READ);
				EIMObject parentObj = null;
				if (parentRelList != null && parentRelList.size() > 0) {
					// 親オブジェクトの取得
					parentObj = ((EIMRelation)parentRelList.get(0)).getParent();
				}
				
				if (parentObj != null) {
					// 下位フォルダ管理セキュリティ取得
					if(!AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
						readOnly = true;
					}
				}
				
				//オブジェクトの書き込み権限をチェック
				if(object.getSecurity() != null)
				{
					if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
					{
						readOnly = true;
					}
				}

			}
			else if (helper.isTypeOfWorkspace(object.getType())) {
				
				// システム管理で属性情報画面を開けるのは「ワークスぺース作成権限」を持つユーザだけ
				// もし権限が変更されていたら参照モードで開く
				// ※オブジェクトの権限はチェックしない
				if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
					readOnly = true;
				}
			}
			else {		
				if(object.getSecurity() != null)
				{
					if(!SecurityUtils.authorized(sess, object,sess.getUser(), EIMAccessRole.UPDATE))
					{
						readOnly = true;
					}
				}
			}
			out.println(" readOnly=\"" + readOnly + "\"");
			
			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
			if (isDocument) {
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + fSize + "\"");
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
